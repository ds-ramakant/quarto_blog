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


# writing function to build frequency table -------------------------------

#inspired by this post https://towardsdatascience.com/wordle-a-frequency-analysis-approach-9989c3d7be5f

#declaring null table
freq_table <- tibble(alpha = LETTERS)

for(i in 1:5){
  
    test <- position_word_list %>% 
    select(all_of(i)) %>% 
    group_by_at(1) %>% # courtesy: https://www.tutorialspoint.com/how-to-use-column-index-instead-of-column-name-while-using-group-by-of-dplyr-in-r
    summarise(f = n()) %>% 
    arrange(desc(f)) %>% 
      rename(a = 1) #first column is p1, p2.. etc and therefore must change
    
    freq_table <- freq_table %>%
    left_join(test, by = c("alpha" = "a")) 
    
    colnames(freq_table)[1+i] = paste0("p",i)
    rm(test)

}


freq_table[is.na(freq_table)] <- 0 #replacing NA with zero


# creating grid of top 5 letters by position ------------------------------

top5_selection <- function(x)
{x %>% arrange(desc(x[2])) %>% head(5) %>% select(1)}

final_grid <- tibble(ranking = 1:5)

for(i in 2:length(freq_table)){
  t <- top5_selection(select(freq_table,1,all_of(i)))
  
  final_grid <- cbind(final_grid,t)
  
  colnames(final_grid)[i] = paste0("p",i-1)
  
}

topwords <- position_word_list %>% 
filter(p1 %in% final_grid$p1,
       p2 %in% final_grid$p2,
       p3 %in% final_grid$p3,
       p4 %in% final_grid$p4,
       p5 %in% final_grid$p5) 

#finding consolidated rank of each word-----------------

table(topwords$p1 %in% final_grid$p1)

topwords %>% mutate(x = which(p1[3] == final_grid$p1))

topwords %<>% #this operator is same as "topwords <- topwords %>% "
  rowwise() %>% 
  mutate(p1_rank = which(p1 == final_grid$p1),
         p2_rank = which(p2 == final_grid$p2),
         p3_rank = which(p3 == final_grid$p3),
         p4_rank = which(p4 == final_grid$p4),
         p5_rank = which(p5 == final_grid$p5))


topwords2 <- topwords %>% 
  transmute(word = paste0(p1,p2,p3,p4,p5),
         rank = sum(p1_rank, p2_rank,p3_rank, p4_rank, p5_rank)) %>% 
  arrange(rank)
