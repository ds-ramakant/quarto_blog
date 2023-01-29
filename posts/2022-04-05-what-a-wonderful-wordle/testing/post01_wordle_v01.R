library(httr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(scales)
library(tidyr)
theme_set(theme_light())


url <- "https://www.nytimes.com/games/wordle/main.18637ca1.js"
wordle_script_text <- GET(url) %>% 
  content(as = "text", encoding = "UTF-8")
word_list = substr(
  wordle_script_text,
  # cigar is the first word
  str_locate(wordle_script_text, "cigar")[,"start"],
  # shave is the last word
  str_locate(wordle_script_text, "shave")[,"end"]) %>%
  str_remove_all("\"") %>%
  str_split(",") %>%
  data.frame() %>%
  select(word = 1) %>%
  mutate(word = toupper(word))


letter_list <- word_list %>%
  as.character() %>%
  str_split("") %>% 
  as.data.frame() %>% 
  select(w_letter = 1) %>% 
  filter(row_number()!=1) %>%
  filter(w_letter %in% LETTERS) %>% 
  mutate(type = case_when(w_letter %in% c("A","E","I","O","U") ~ "vowel",
                          T ~ "consonant")) %>% 
  group_by(w_letter, type) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

#appearance of all letters 
letter_list %>% ungroup() %>% 
  ggplot(aes(x = reorder(w_letter, -freq), y = freq))+
  geom_col(aes(fill = type))+
  scale_y_continuous(labels = comma)+
  geom_text(aes(label = freq), 
            #position = position_dodge(width = 0.3),
            size = 3)+
  labs(x = "Letter", y = "Frequency",
       title = "Frequency of words in Official Wordle list")


position_word_list <- word_list %>% 
  separate(word, 
           sep = "", 
           into = c("x","p1","p2","p3","p4","p5")) %>% 
  select(-x)
  
vowels <- c("A", "E", "I", "O", "U")


# position of the first word as a vowel -----------------------------------


distribution_p1 <- position_word_list %>% 
  select(p1) %>% 
  mutate(type = case_when(p1 %in% vowels ~ "vowel",
                          T ~ "consonant")) %>% 
  group_by(p1, type) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

distribution_p1 %>% 
  ggplot(aes(x = reorder(p1, -freq), y = freq))+
  geom_col(aes(fill = type))+
  scale_y_continuous(labels = comma)+
  geom_text(aes(label = freq), 
            #position = position_dodge(width = 0.3),
            size = 3)+
  labs(x = "Letter", y = "Frequency",
       title = "Frequency of 1st word in Official Wordle list")

distribution_p1 %>% ungroup() %>% 
  mutate(prop = freq/sum(freq)) %>% 
  head(5) %>% summarise(top5_prop = sum(prop))

# position of the 2nd word as a vowel -----------------------------------
distribution_p2 <- position_word_list %>% 
  select(p2) %>% 
  mutate(type = case_when(p2 %in% vowels ~ "vowel",
                          T ~ "consonant")) %>% 
  group_by(p2, type) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

distribution_p2 %>% 
  ggplot(aes(x = reorder(p2, -freq), y = freq))+
  geom_col(aes(fill = type))+
  scale_y_continuous(labels = comma)+
  geom_text(aes(label = freq), 
            #position = position_dodge(width = 0.3),
            size = 3)+
  labs(x = "Letter", y = "Frequency",
       title = "Frequency of 2nd word in Official Wordle list")

distribution_p2 %>% ungroup() %>% 
  mutate(prop = freq/sum(freq)) %>% 
  head(5) %>% summarise(top5_prop = sum(prop))

# position of the 3rd word as a vowel -----------------------------------
distribution_p3 <- position_word_list %>% 
  select(p3) %>% 
  mutate(type = case_when(p3 %in% vowels ~ "vowel",
                          T ~ "consonant")) %>% 
  group_by(p3, type) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

distribution_p3 %>% 
  ggplot(aes(x = reorder(p3, -freq), y = freq))+
  geom_col(aes(fill = type))+
  scale_y_continuous(labels = comma)+
  geom_text(aes(label = freq), 
            #position = position_dodge(width = 0.3),
            size = 3)+
  labs(x = "Letter", y = "Frequency",
       title = "Frequency of 3rd word in Official Wordle list")

distribution_p3 %>% ungroup() %>% 
  mutate(prop = freq/sum(freq)) %>% 
  head(5) %>% summarise(top5_prop = sum(prop))


# position of the 3rd word as a vowel -----------------------------------
distribution_p4 <- position_word_list %>% 
  select(p4) %>% 
  mutate(type = case_when(p4 %in% vowels ~ "vowel",
                          T ~ "consonant")) %>% 
  group_by(p4, type) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

distribution_p4 %>% 
  ggplot(aes(x = reorder(p4, -freq), y = freq))+
  geom_col(aes(fill = type))+
  scale_y_continuous(labels = comma)+
  geom_text(aes(label = freq), 
            #position = position_dodge(width = 0.3),
            size = 3)+
  labs(x = "Letter", y = "Frequency",
       title = "Frequency of 4th word in Official Wordle list")

distribution_p4 %>% ungroup() %>% 
  mutate(prop = freq/sum(freq)) %>% 
  head(5) %>% summarise(top5_prop = sum(prop))


# position of the 5th word as a vowel -----------------------------------
distribution_p5 <- position_word_list %>% 
  select(p5) %>% 
  mutate(type = case_when(p5 %in% vowels ~ "vowel",
                          T ~ "consonant")) %>% 
  group_by(p5, type) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

distribution_p5 %>% 
  ggplot(aes(x = reorder(p5, -freq), y = freq))+
  geom_col(aes(fill = type))+
  scale_y_continuous(labels = comma)+
  geom_text(aes(label = freq), 
            #position = position_dodge(width = 0.3),
            size = 3)+
  labs(x = "Letter", y = "Frequency",
       title = "Frequency of 5th word in Official Wordle list")

distribution_p5 %>% ungroup() %>% 
  mutate(prop = freq/sum(freq)) %>% 
  head(5) %>% summarise(top5_prop = sum(prop))


# words most likely to be strong openers -----------------------------

top5_p1 <-distribution_p1 %>% ungroup() %>% 
  mutate(prop = freq/sum(freq)) %>% 
  head(5)  

top5_p2 <-distribution_p2 %>% ungroup() %>% 
  mutate(prop = freq/sum(freq)) %>% 
  head(5)  

top5_p3 <-distribution_p3 %>% ungroup() %>% 
  mutate(prop = freq/sum(freq)) %>% 
  head(5)  

top5_p4 <-distribution_p4 %>% ungroup() %>% 
  mutate(prop = freq/sum(freq)) %>% 
  head(5)  

top5_p5 <-distribution_p5 %>% ungroup() %>% 
  mutate(prop = freq/sum(freq)) %>% 
  head(5)  


cbind(top5_p1$p1, top5_p2$p2, top5_p3$p3)

position_word_list %>% 
  filter(p1 %in% top5_p1$p1,
         p2 %in% top5_p2$p2,
         p3 %in% top5_p3$p3,
         p4 %in% top5_p4$p4,
         p5 %in% top5_p5$p5) %>% 
  arrange(p5)
