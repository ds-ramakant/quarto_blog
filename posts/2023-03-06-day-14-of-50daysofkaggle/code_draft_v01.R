library(tidyverse)
library(tidymodels)


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

#as per https://regex101.com/
names_with_splchar <- regex("[A-Za-z]+[\\'\\-\\s]+[A-Za-z]+")
names_with_3words <- regex("[A-Za-z]+\\s[A-Za-z]+\\s[A-Za-z]+")
names_with_1word <- regex("[A-Za-z]+") 
names_with_2words <- regex("[A-Za-z]+\\s[A-Za-z]+")

str_detect("Duran y More, Miss. Asuncion", names_with_splchar)
str_view("Duran y More, Miss. Asuncion", names_with_3words)

str_view("Vande Velde, Mr. Johannes Joseph", names_with_splchar)
str_view(head(titanic_data$name,20), 
           str_c(c(names_with_1word, names_with_3words, names_with_splchar), 
                 collapse = "|"))


debug_df <- titanic_data %>% 
  separate_wider_regex(
    name, 
    patterns = c(
      surname = str_c(c(names_with_splchar, 
                        names_with_1word, 
                        names_with_3words), 
                      collapse = "|"), # picks the first word before comma
      ", ",                  # the comma  
      title =  "[A-Za-z]+",  # picks word or words after the comma
      ". ",                  # the dot
      given_name = ".{1,}"), # picks anything else which occurs at least once
    too_few = "debug") %>% view()

titanic_data %>% 
  separate_wider_regex(
    name, 
    patterns = c(
      surname = str_c(c(names_with_splchar, 
                        names_with_1word, 
                        names_with_3words), 
                      collapse = "|"), # picks the first word before comma
      ", ",                  # the comma  
      title = str_c(c(names_with_1word , # one word with a space
                      names_with_2words), #two words with special char in between like 'the countess'
                    collapse = "|"), # picks word or words after the comma
      ". ",                  # the dot
      given_name = ".{1,}"), # picks anything else which occurs at least once
    too_few = "debug") %>% view()



debug_df %>% 
  group_by(title) %>% count(sort= T)





# rough -------------------------------------------------------------------


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
title_regex2 <- c(paste0("[,]\\s",names_with_1word),
                  paste0("[,]\\s",names_with_2words, "\\s[.]"))



str_detect(regex_test1, str_c(surname_regex, collapse = "|"))
str_view(regex_test1, str_c(surname_regex, collapse = "|"))

str_detect(regex_test1, "[,]\\s[A-Za-z]+[.]")
str_view(regex_test1, "[,]\\s[A-Za-z]+[.]")

str_detect(regex_test1, str_c(title_regex, collapse = "|"))
str_view(regex_test1, str_c(title_regex, collapse = "|"))

str_detect(regex_test1, str_c(title_regex2, collapse = "|"))
str_view(regex_test1, str_c(title_regex2, collapse = "|"))

str_view(regex_test1, paste0("[,]\\s",names_with_2words, "[.]"))
