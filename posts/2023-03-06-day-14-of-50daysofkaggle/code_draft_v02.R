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
  mutate(survived = as_factor(if_else(survived ==1, "yes", "no")),
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

ticket_head_uniques <- unique(titanic_data$ticket_head)

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
                         "4_ticket_grouping", flag)) %>% view()
write.csv(titanic_data, row.names = F, 
          file = ".\\posts\\2023-03-06-day-14-of-50daysofkaggle\\debug_df2.csv")

to_predict <- titanic_data %>% 
  filter(source == "test")
given_data <- titanic_data %>% 
  filter(source == "train") %>% 
  select(-name, -sib_sp, -parch, -cabin, -source, -group_id, -given_name, -surname, 
         -family_count, -contains("ticket"), -contains("_in_"), -flag)


# splitting train data for  ----------------------------------------------------------

df_split <- initial_split(given_data, prop = 0.8)
train <- training(df_split)
test <- testing(df_split)

median_age_calc <- function(sex, pclass){
given_data %>% 
  group_by(sex, pclass) %>% 
  summarise(median_age = median(age,na.rm = T)) %>% ungroup()
}

train %>% median_age_calc()


# recipe ------------------------------------------------------------------


dt_recipe <- recipe(survived ~ ., data = given_data) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  #replacing NA values in Age with median Age
  step_mutate_at(age, fn = ~ replace_na(age, median(age, na.rm = T))) %>% 
  #updating the role of the PassengerId to exclude from analysis
  update_role(passenger_id, new_role = "id_variable")

dt_recipe
tidy(dt_recipe)


# model -------------------------------------------------------------------
dt_model <- decision_tree(mode = "classification", tree_depth = 3) %>% 
  set_engine("rpart")
dt_model %>% translate()


# workflow ----------------------------------------------------------------
dt_wf <- workflow() %>%
  add_model(dt_model) %>% 
  add_recipe(dt_recipe)


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
  mutate(.pred_class= as_factor(if_else(.pred_class =="no", 0, 1)))

final_predictions <- final_predictions %>% 
  rename(Survived = .pred_class) %>% 
  bind_cols(PassengerId = to_predict$passenger_id)
head(final_predictions)
write.csv(final_predictions, row.names = F, 
          file = ".\\posts\\2023-03-06-day-14-of-50daysofkaggle\\submissions.csv")


