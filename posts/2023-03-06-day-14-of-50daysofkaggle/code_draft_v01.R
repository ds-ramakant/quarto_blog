library(tidyverse)
library(tidymodels)
library(DataEditR)

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

str_detect("Duran y More, Miss. Asuncion", names_with_splchar)
str_view("Duran y More, Miss. Asuncion", names_with_3words)

str_view("Vande Velde, Mr. Johannes Joseph", names_with_splchar)
str_view(head(titanic_data$name,20), 
           str_c(c(names_with_1word, names_with_3words, names_with_splchar), 
                 collapse = "|"))


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

titanic_data <- titanic_data %>% 
  mutate(grouping1 = str_c(surname, family_count, sep = "_"))

table1 <- titanic_data %>% 
  count(grouping1, sort = T) 

debug_df <- titanic_data %>% 
  select(passenger_id, 
         title, contains("name"),-name, family_count, sib_sp, parch,
         grouping1, 
         sex, age,
         ticket, fare, everything()) 


# checking for grouping errors --------------------------------------------

table2 <- titanic_data %>% 
  add_count(grouping1) %>% 
  rename(grouping1_count= n) %>% 
  filter(!(family_count == grouping1_count)) %>% 
  group_by(grouping1, family_count) %>% 
  count() %>% 
  rename(count_z = n)

view(table2)
table2 %>% 
  filter(count_z < family_count) %>% 
  view()

table2 %>%
  mutate(delta = count_z - family_count)  %>%
  group_by(delta) %>% count()

table2 %>%
  mutate(delta = count_z - family_count)  %>%
  filter(delta<0) %>% 
  group_by(family_count) %>% count()


# creating ticket_head and new grouping -----------------------------------------------

# notes: 
# 1) there are people with same surnames but not from the same family ie. 
#       family_count not matching and/or ticket_id_minus_2char are not matching
# 2) need to come up with some way of creating some flag that creates these groupings
# 3) in table2, if the incorrect_grouping count is greater than family_count, it means that
#       the surname is appearing multiple times and they are not from the same family
# 4) in table2, if the incorrect_grouping count is less than family_count, it might imply data error
#       i.e family_count is high implies other family members are missing
# 


titanic_data <- titanic_data %>% 
  mutate(ticket_head = substr(ticket, 1, nchar(ticket)-1),
         ticket_tail = substr(ticket, nchar(ticket), nchar(ticket)),
         grouping2 = paste0(surname, "_", ticket_head)) 

titanic_data %>% 
  count(grouping2, sort = T) %>% rename(px_in_grouping2 = n) %>% 
  count(px_in_grouping2)


#adding count for grouping2
titanic_data <- titanic_data %>% 
  add_count(grouping2) %>% 
  rename(grouping2_count = n)

# creating flag to understand the relationships here:
# case 1: if (family_count ==1 AND grouping2_count ==1), then flag as "1_solo" (this may include relatives & friends)
# case 2: if (family_count == grouping2_count), then family is complete and flag is "2_family_full"
# case 3: if (family_count < grouping2_count), then family_count is incorrect but they're on the same ticket so flag is "3_family_friends"
# case 4: if (family_count > grouping2_count), then flag as "4_inspect"

debug_df <- titanic_data %>% 
  mutate(flag = case_when ((family_count==1 & grouping2_count==1) ~ "1_solo",
                           family_count == grouping2_count ~ "2_family_full",
                           family_count < grouping2_count ~ "3_family_plus",
                           family_count > grouping2_count ~ "4_inspect",
                           .default = "x")
         )

debug_df <- debug_df %>% 
  select(title, contains("name"),family_count,
         contains("grouping2"), flag,
         sex, age,
         ticket, fare, everything(),-name, -grouping1, -sib_sp, -parch)


table3 <- debug_df %>% 
  select(grouping2, flag) %>% 
  group_by(flag) %>% 
  summarise(unique_g2 = n_distinct(grouping2))

table3 <- debug_df %>% count(flag) %>% rename(unique_pax=n)  %>% select(unique_pax) %>%  bind_cols(table3)

table3
debug_df %>% 
  filter(flag=="4_inspect") %>% count(grouping2, sort = T) %>% print(n = 64)

debug_df %>% 
  filter(flag=="3_family_plus") %>% count(grouping2, sort = T)


# finding nannies, relatives & friends ---------------------------------------------

debug_df <- debug_df %>% 
  add_count(ticket_head) %>% 
  rename(ticket_count = n) %>% 
  select(title, contains("name"),family_count,ticket_count,
         contains("grouping2"), flag,
         sex, age,
         ticket, fare, everything())

ticket_head_uniques <- unique(debug_df$ticket_head)

#how many people within the same ticket have multiple grouping2? 
debug_df <- debug_df %>% 
  group_by(ticket) %>% 
  mutate(number_of_g2 = n_distinct(grouping2)) %>% ungroup()
   
ticket_with_multi_groups <- debug_df %>% 
  filter(!number_of_g2==1) %>% 
  count(ticket, sort = T)

ticket_with_multi_groups <- ticket_with_multi_groups %>% 
  mutate(merge = NA)

data_edit(ticket_with_multi_groups)


debug_df %>% 
  group_by(ticket) %>% 
  summarise(number_of_g2 = n_distinct(grouping2)) %>% 
  count(number_of_g2)


debug_df <- debug_df %>% 
  mutate(grouping3 = if_else(ticket %in% ticket_with_multi_groups$ticket, 
                             ticket, grouping2),
         lable = if_else(ticket %in% ticket_with_multi_groups$ticket,
                         "5_ticket_grouping", flag))

write.csv(debug_df, row.names = F, 
          file = ".\\posts\\2023-03-06-day-14-of-50daysofkaggle\\debug_df.csv")



# rough -------------------------------------------------------------------
titanic_data %>% 
  head(10) %>% 
  select(ticket) %>% 
  separate_wider_position(widths = c(ticketID_minus2 = nchar()-2), 
                          cols = "ticket",
                          too_few = "debug")




titanic_data %>% 
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
                      names_with_1word),  # one word with a space
                    collapse = "|"),      # picks word or words after the comma
      ". ",                               # the dot
      given_name = ".{1,}"),              # picks anything else which occurs at least once
    too_few = "debug") %>% view()




library(stringr)

# Sample data frame with text columns
df <- data.frame(
  id = 1:3,
  text1 = c("Hello World", "Goodbye World", "Hello Again"),
  text2 = c("foo bar", "hello world", "foo baz")
)

# Regular expression patterns to match
patterns <- c("Hello", "foo")

# Extract columns that match any of the patterns
matches <- str_detect(df$text1, str_c(patterns, collapse = "|")) |
  str_detect(df$text2, str_c(patterns, collapse = "|"))
df[matches, ]


regex_test1 <- c("Vande Velde, Mr. Johannes Joseph",
"de Messemaeker, Mrs. Guillaume Joseph (Emma)",
"Frolicher-Stehli, Mr. Maxmillian",
"Duran y More, Miss. Asuncion",
"Heikkinen, Miss. Laina", 
"tom and jerry", 
"Rothes, the Countess. of (Lucy Noel Martha Dyer-Edwards)",
"the countess,a")
regex_test1

surname_regex <- c(paste0("^",names_with_1word,"[,]"), 
                    paste0("^",names_with_3words,"[,]"),
                    paste0("^",names_with_splchar,"[,]"))

regex("^[A-Za-z]+")

title_regex <- c(regex("[,]\\s[A-Za-z]+[.]" ),
                 regex("[,]\\s[A-Za-z]+[\\'\\-\\s]+[A-Za-z]+[.]"))
title_regex2 <- c(paste0("[,]\\s",names_with_2words, "[.]"),
                  paste0("[,]\\s",names_with_1word))



str_detect(regex_test1, str_c(surname_regex, collapse = "|"))
str_view(regex_test1, str_c(surname_regex, collapse = "|"))

str_detect(regex_test1, "[,]\\s[A-Za-z]+[.]")
str_view(regex_test1, "[,]\\s[A-Za-z]+[.]")

str_detect(regex_test1, str_c(title_regex, collapse = "|"))
str_view(regex_test1, str_c(title_regex, collapse = "|"))

str_detect(regex_test1, str_c(title_regex2, collapse = "|"))
str_view(regex_test1, str_c(title_regex2, collapse = "|"))

str_view(regex_test1, paste0("[,]\\s",names_with_2words))

df_xx$y = 7:9
df_xx
supid <- function(df){
    paste0(df$ticket_head, "__", df$ticket_tail)
}
supid(df_xx)
titanic_data %>% head(10) %>% supid(.)
