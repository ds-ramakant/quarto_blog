library(tidyverse)
library(tidymodels)
library(vip)

# reading data ------------------------------------------------------------

titanic_train <- read.csv("train.csv", header = T)
glimpse(titanic_train)

titanic_test <- read.csv("test.csv", header = T)
glimpse(titanic_test)
titanic_test <- titanic_test %>% 
  mutate(Survived = NA,
         source = "test")
glimpse(titanic_test)

titanic_train <- titanic_train %>% 
  mutate(source = "train")
glimpse(titanic_train)

titanic_data <- bind_rows(titanic_train, titanic_test)
glimpse(titanic_data)


# cleaning data -----------------------------------------------------------
#checking for columns with NA values
titanic_data %>% 
  group_by(source) %>% 
  summarise_all(~ sum(is.na(.)))

#adding family_count and cleaning names
titanic_data <- titanic_data %>% 
  mutate(family_count = SibSp+Parch+1) %>% 
  janitor::clean_names() 

#there are two rows where embarked is not NA but just a blank. so we will fill it with the most common embarked port
#https://stackoverflow.com/questions/21618423/extract-a-dplyr-tbl-column-as-a-vector
titanic_data %>% 
  count(embarked, sort = T) %>% 
  select(embarked) %>% unlist(use.names = F) %>% .[1]

mode_embarked <- titanic_data %>% 
  count(embarked, sort = T) %>% 
  select(embarked) %>% pull() %>% .[[1]]

titanic_data <- titanic_data %>% 
  mutate(embarked = if_else(embarked == "", mode_embarked, embarked))

#checking for replacement
titanic_data %>% filter(ticket == "113572") 

#checking break-up of embarked. earlier 'S' was 914 and now changes to 916
titanic_data %>% 
  count(embarked, sort = T) 

#converting few columns to factors
titanic_data <- titanic_data %>% 
  mutate(survived = as_factor(survived),
         pclass = as_factor(pclass),
         sex = as_factor(sex),
         embarked = as_factor(embarked))

glimpse(titanic_data)

# regex cleaning ----------------------------------------------------------
#as per https://regex101.com/
names_with_splchar <- regex("[A-Za-z]+[\\'\\-\\s]+[A-Za-z]+")
names_with_3words <- regex("[A-Za-z]+\\s[A-Za-z]+\\s[A-Za-z]+")
names_with_1word <- regex("[A-Za-z]+") 
names_with_2words <- regex("[A-Za-z]+\\s+[A-Za-z]+")


titanic_data <- titanic_data %>% 
  separate_wider_regex(
    name, 
    patterns = c(
      #IMP: ordering of regex patterns changes the outcome
      surname = str_c(c(names_with_splchar, 
                        names_with_3words,
                        names_with_1word), 
                      collapse = "|"),    # picks the first word before comma
      ", ",                               # the comma  
      #IMP: ordering of regex patterns changes the outcome
      title = str_c(c(names_with_2words , # two words with special char in between like 'the countess'
                      names_with_1word),  # one word such as Mr Miss Mrs etc
                    collapse = "|"),      
      ". ",                               # the dot
      given_name = ".+"),                 # picks anything else which occurs at least once
    #retains the original column    
    cols_remove = F
  ) 


titanic_data %>% 
  count(title, sort= T)


# creating grouping with surname + ticket_head ----------------------------

titanic_data <- titanic_data %>% 
  mutate(ticket_head = substr(ticket, 1, nchar(ticket)-1),
         ticket_tail = substr(ticket, nchar(ticket), nchar(ticket)),
         group_id = paste0(surname, "_", ticket_head)) 

titanic_data <- titanic_data %>% 
  add_count(group_id) %>% rename(pax_in_group = n)


# creating flags to identify kind of grouping ------------------------------


# creating flag to understand the relationships here:
# case 1: if (family_count ==1 AND grouping2_count ==1), then flag as "1_solo" (this may include relatives & friends)
# case 2: if (family_count == grouping2_count), then family is complete and flag is "2_familyfull"
# case 3: else it is "3_clubbed"

# flag 1
titanic_data <- titanic_data %>% 
  mutate(flag = case_when ((family_count==1 & pax_in_group==1) ~ "1_solo",
                           family_count == pax_in_group ~ "2_family_full",
                           !(family_count == pax_in_group) ~ "3_clubbed",
                           .default = "x"))

titanic_data <- titanic_data %>% 
  add_count(ticket_head) %>% 
  rename(pax_in_ticket_head = n)

#ticket_head_uniques <- unique(titanic_data$ticket_head)

# how many instances of the same ticket having multiple groups? 
titanic_data <- titanic_data %>% 
  group_by(ticket) %>% 
  mutate(groups_in_ticket = n_distinct(group_id)) %>% ungroup()

# which tickets that have more than 1 groups in them? 
#     these passengers will have ticket_grouping precedence as they may include 
#     nannies, relatives & friends that don't share the same surname
ticket_with_multiple_groups <- titanic_data %>% 
  filter(!groups_in_ticket==1) %>% 
  count(ticket, sort = T)

titanic_data <- titanic_data %>% 
  mutate(final_grouping = if_else(ticket %in% ticket_with_multiple_groups$ticket, 
                             ticket, group_id),
         final_label = if_else(ticket %in% ticket_with_multiple_groups$ticket,
                         "4_ticket_grouping", flag)) 
write.csv(titanic_data, row.names = F, 
          file = ".\\posts\\2023-03-06-day-14-of-50daysofkaggle\\debug_df2.csv")

titanic_data2 <- titanic_data %>% 
  select(source, passenger_id, survived,  sex, age, fare, pclass, embarked, 
         family_count, pax_in_group, pax_in_ticket_head, groups_in_ticket,
         final_grouping) %>% 
  mutate(final_grouping = as_factor(final_grouping))

to_predict <- titanic_data2 %>% 
  filter(source == "test") %>% select(-survived, -source)
given_data <- titanic_data2 %>% 
  filter(source == "train") %>% select(-source)


# replacing NA in age column ----------------------------------------------


median_age_calc <- function(df){
  median_ages <- df %>% 
    group_by(sex, pclass) %>% 
    summarise(age = median(age,na.rm = T))
  df %>%
    mutate(age = case_when((sex =="male" & pclass ==1 & is.na(age)) ~ median_ages$age[1],
                           (sex =="male" & pclass ==2 & is.na(age)) ~ median_ages$age[2],
                           (sex =="male" & pclass ==3 & is.na(age)) ~ median_ages$age[3],
                           (sex =="female" & pclass ==1 & is.na(age)) ~ median_ages$age[4],
                           (sex =="female" & pclass ==2 & is.na(age)) ~ median_ages$age[5],
                           (sex =="female" & pclass ==3 & is.na(age)) ~ median_ages$age[6],
                           .default = age)
    )
}


given_data <- given_data %>% median_age_calc() 
to_predict <- to_predict %>% median_age_calc()

given_data %>% 
  summarise_all(~sum(is.na(.)))
to_predict %>% 
  summarise_all(~sum(is.na(.)))
to_predict %>% 
  group_by(pclass, sex) %>% 
  summarise(median = mean(fare, na.rm = T))

to_predict <- to_predict %>% 
  filter() %>% 
  mutate(fare = case_when(passenger_id==1044 ~ 11.8, #median fare for males in pclass 3
                          .default = fare)
  )


# splitting train data for  ----------------------------------------------------------

df_split <- initial_split(given_data, prop = 0.8)
train <- training(df_split)
test <- testing(df_split)

# recipe ------------------------------------------------------------------


base_recipe <- recipe(survived ~ ., data = given_data) %>% 
  update_role(passenger_id, new_role = "id_variable") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 
  #replacing NA values in Age with median Age
#  step_mutate_at(age,sex, pclass, fn = median_age_calc) %>% 
  #updating the role of the PassengerId to exclude from analysis
  

base_recipe
tidy(base_recipe)


# model -------------------------------------------------------------------
dt_model <- decision_tree(mode = "classification", tree_depth = 3) %>% 
  set_engine("rpart")
dt_model %>% translate()


# workflow ----------------------------------------------------------------
dt_wf <- workflow() %>%
  add_model(dt_model) %>% 
  add_recipe(base_recipe)


# predicting on test ------------------------------------------------------
set.seed(2023)
dt_predict <- predict(fit(dt_wf, data = train), test)
head(dt_predict)



# testing the predictions -------------------------------------------------

predicted_table <- bind_cols(test, dt_predict) %>% 
  rename(dt_yhat = .pred_class) %>% 
  select(survived, dt_yhat) 
head(predicted_table)

conf_mat(predicted_table, truth = survived, estimate = dt_yhat)

accuracy(predicted_table, truth = survived, estimate = dt_yhat)

classification_metrics <- metric_set(accuracy, f_meas)
predicted_table %>% 
  classification_metrics(truth = survived, estimate = dt_yhat)


# submission on kaggle ----------------------------------------------------

final_predictions <- predict(fit(dt_wf, data = given_data), to_predict)
head(final_predictions)
table(final_predictions)
final_predictions <- final_predictions %>% 
  mutate(.pred_class= as_factor(.pred_class))

final_predictions <- final_predictions %>% 
  rename(Survived = .pred_class) %>% 
  bind_cols(PassengerId = to_predict$passenger_id)
head(final_predictions)
write.csv(final_predictions, row.names = F, 
          file = ".\\posts\\2023-03-06-day-14-of-50daysofkaggle\\submissions.csv")

# accuracy is 0.77272 after:



# multimodel approach --------------------------------------------------
# based on v04
# consider 4 models and create a workflow for each. no tuning involved (for now)
# bootstrapping to help us find roc_auc
# predict the final 


# creating validation folds ----------------------------------------------------------
set.seed(2023)
titanic_folds <- bootstraps(data = given_data, 
                            times = 10)
titanic_folds

# multiple model building -------------------------------------------------------------------
# with kind inputs from https://rpubs.com/tsadigov/titanic_tidymodels

#logistic regression
glm_model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

#random forest
rf_model <- rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

#support vector machines
svm_model <- svm_rbf() %>% # rbf - radial based
  set_engine("kernlab") %>% 
  set_mode("classification")

#decision tree
dt_model <- decision_tree(mode = "classification", 
                          tree_depth = 3) %>% 
  set_engine("rpart")

dt_model %>% translate()

# workflow ----------------------------------------------------------------


doParallel::registerDoParallel() # resample fitting is embarrasingly parrallel problem
glm_wf <- workflow() %>% 
  add_model(glm_model) %>% 
  add_recipe(base_recipe) %>% 
  fit_resamples(titanic_folds)
collect_metrics(glm_wf)

glm_workflow <- workflow() %>% 
  add_model(glm_model) %>% 
  add_recipe(base_recipe)

doParallel::registerDoParallel()
rf_wf <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(titanic_recipe) %>% 
  fit_resamples(titanic_folds, save_pred = T)
collect_metrics(rf_wf)

doParallel::registerDoParallel()
svm_wf <- workflow() %>% 
  add_model(svm_model) %>% 
  add_recipe(titanic_recipe) %>% 
  fit_resamples(titanic_folds)
collect_metrics(svm_wf)

doParallel::registerDoParallel()
dt_wf <- workflow() %>%
  add_model(dt_model) %>% 
  add_recipe(titanic_recipe) %>% 
  fit_resamples(titanic_folds)
collect_metrics(dt_wf)

# Random forest had the best roc_acu of 0.865



# creating a workflow set using v05 -------------------------------------------------
titanic_wf_set <- workflow_set(
  list(base_recipe),
  list(glm_model, rf_model, svm_model, dt_model),
  cross = T
)

# this part of the script courtesy of https://juliasilge.com/blog/giant-pumpkins/

set.seed(2023)
doParallel::registerDoParallel()
titanic_rs <- workflow_map(
  titanic_wf_set,
  "fit_resamples",
  resamples = titanic_folds
)

titanic_rs

autoplot(titanic_rs)

collect_metrics(titanic_rs) %>% 
  filter(.metric == "roc_auc")

final_fit <- extract_workflow(titanic_rs, 
                              "recipe_rand_forest") %>% 
  fit(given_data)

final_predictions <- predict(object = final_fit, 
                             new_data = to_predict)
final_predictions


final_predictions <- final_predictions %>% 
  rename(Survived = .pred_class) %>% 
  bind_cols(PassengerId = to_predict$passenger_id)
head(final_predictions)
write.csv(final_predictions, row.names = F, 
          file = ".\\posts\\2023-03-06-day-14-of-50daysofkaggle\\submissions.csv")

# finding variable importance ---------------------------------------------



final_fit %>% 
  pull_workflow_fit() %>% 
  vip()


# generating autoplot manually --------------------------------------------
# because I can't sleep without looking under the hood

tidy_titanic_rs <- tibble(wf = rep(unique(titanic_rs$wflow_id), 10*2),
                          metrics = rep(c(rep("accuracy", 4), 
                                          rep("roc_auc",4)),
                                        10),
                          values = rep(NA, 4*10*2))

tidy_titanic_rs



bigtable <- purrr:::pluck(titanic_rs, 4)
wflow_id_titanic <- unique(titanic_rs$wflow_id)
for(i in 1:length(wflow_id_titanic)){
    
  wflow_id <- wflow_id_titanic[i]
  smalltable <- bigtable[[i]]
  
  for(j in 1:length(smalltable$.metrics)){
    smallertable <- purrr::pluck(smalltable$.metrics, j)
    tidy_titanic_rs$values[(tidy_titanic_rs$wf==wflow_id & 
                              tidy_titanic_rs$metrics=="accuracy")][j] <- smallertable$.estimate[smallertable$.metric == "accuracy"]
    tidy_titanic_rs$values[(tidy_titanic_rs$wf==wflow_id & 
                              tidy_titanic_rs$metrics=="roc_auc")][j] <- smallertable$.estimate[smallertable$.metric == "roc_auc"]
    
  }
}

tidy_titanic_rs2 <- tidy_titanic_rs %>% 
  group_by(wf, metrics) %>% 
  summarise(value_min = mean(values) - 0.5*sd(values),
            value_max = mean(values) + 0.5*sd(values),
            value_mean = mean(values)) %>% ungroup() %>% 
  right_join(tidy_titanic_rs, by = c("wf", "metrics"))

write.csv(tidy_titanic_rs, row.names = F, 
          file = ".\\posts\\2023-03-06-day-14-of-50daysofkaggle\\tidy_titanic_rs.csv")


# generating manual plot --------------------------------------------------

p1 <- tidy_titanic_rs2 %>% 
  filter(metrics =="accuracy") %>% 
  ggplot(aes(x = reorder(wf, desc(value_mean)), 
             y = values, 
             color = wf))+
  geom_errorbar(aes(ymax = value_max, ymin = value_min), 
                width = 0.1)+
  geom_point(aes(y= value_mean))+
  scale_y_continuous(breaks = seq(0.6, 0.9, 0.05))+
  theme(legend.position = "none")+
  labs(title = "Accuracy",x = NULL)
p1  
p2 <- tidy_titanic_rs2 %>% 
  filter(metrics =="roc_auc") %>% 
  ggplot(aes(x = reorder(wf, desc(value_mean)), 
             y = values, 
             color = wf))+
  geom_errorbar(aes(ymax = value_max, ymin = value_min), 
                width = 0.1)+
  geom_point(aes(y= value_mean))+
  scale_y_continuous(breaks = seq(0.6, 0.9, 0.05))+
  theme(legend.position = "none")+
  labs(title = "ROC_AUC",x = NULL)
p2

tidy_titanic_rs2 %>% 
ggplot(aes(x = reorder(wf, desc(value_mean)), 
             y = values, 
             color = wf))+
  geom_errorbar(aes(ymax = value_max, ymin = value_min), 
                width = 0.1)+
  geom_point(aes(y= value_mean))+
  scale_y_continuous(breaks = seq(0.65, 0.9, 0.05),
                     limits = c(0.65, 0.9))+
  theme(legend.position = "none")+
  labs(x = NULL, y = NULL)+
  facet_wrap(~metrics)




# draft scripts -----------------------------------------------------------


xx_bigtable <- purrr::pluck(titanic_rs, 4)
xx_test <- tibble(wf = rep(unique(titanic_rs$wflow_id), 10*2),
                  metrics = rep(c(rep("accuracy", 4), 
                                  rep("roc_auc",4)),
                                  10),
                  values = rep(NA, 4*10*2))



for(i in 1:unique(titanic_rs$wflow_id)){
  for(j in 1:10){
    temp_table <- purrr::pluck(xx_bigtable[[i]])
    xx_bigtable[[i]]$.metrics[[j]]$.estimate[xx_bigtable[[i]]$.metrics[[j]]$.metric=="accuracy"]
  }
}


tibble(x = rep(c(1,2), 4), 
       y = rep(c("a", "b", "c", "d"), 2),
       z  = rep(0, 8)
       )
