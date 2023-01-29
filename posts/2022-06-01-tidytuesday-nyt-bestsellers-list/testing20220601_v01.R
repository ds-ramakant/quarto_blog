library(tidyverse)
library(scales)
library(knitr)
library(tidytuesdayR)
library(forcats)
library(lubridate)
library(RColorBrewer)


data <- read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')

glimpse(data)

data %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  filter(best_rank==1, year<2020) %>% 
  group_by(decade) %>% 
  summarise(n = n_distinct(title)) %>% 
  ggplot(aes(x = decade, y = n))+
  geom_bar(stat = "identity")

#longevity; number of weeks spent by 1st rank books on the charts
data %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  filter(best_rank==1, year<2020) %>% 
  group_by(decade) %>% 
  summarise(avg_weeks = mean(total_weeks),
            no_of_rank1 = n_distinct(title))

data %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  filter(year<2020, best_rank<6) %>% 
  group_by(decade) %>% 
  ggplot(aes(x = decade, y = total_weeks))+
  geom_boxplot()


data %>% 
  #mutate(decade = factor(10*year %/% 10)) %>% 
  filter(year> 1989) %>% 
  group_by(year) %>% 
  summarise(n = n_distinct(title)) %>% 
  ggplot(aes(x = year, y = n))+
  geom_line()

rank_all <- data %>% 
  #filter(year> 1989) %>% 
  group_by(year) %>% 
  summarise(all_titles = n_distinct(title))

rank_best <-data %>% 
  filter(best_rank ==1) %>% 
  group_by(year) %>% 
  summarise(best_rank_is_1 = n_distinct(title))

table01 <- rank_all %>% 
  left_join(rank_best, by = "year") %>% 
  mutate(success_ratio = best_rank_is_1/all_titles)

#comparing number of titles to the ones that made it top of the charts
table01 %>% 
  ggplot(aes(x = year))+
  geom_line(aes(y = all_titles), color = "red")+
  geom_line(aes(y = best_rank_is_1), color = "blue")+
  scale_x_continuous(breaks = (seq(from = 1930, to = 2020, by = 10)))

#number of best sellers jumped up significantly in the 2000s
#this means that in the 90s there were an avg of 10 best sellers in each each
#but that has changed to 22 during the 2000s and 31 in the 2010s
table01 %>% 
  filter(year<2020) %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  ggplot(aes(x = decade, y = best_rank_is_1))+
  geom_boxplot()

##draft table
table01 %>% 
  filter(year<2020) %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  group_by(decade) %>% 
  summarise(avg_titles = mean(all_titles),
            avg_best = mean(best_rank_is_1))


#success ratio
table01 %>% 
  ggplot(aes(x = year, y = success_ratio))+
  geom_line()+
  scale_x_continuous(breaks = (seq(from = 1930, to = 2020, by = 10)))

#has this had an impact on the shelf life of books?
data %>%
  mutate(decade = factor(10*year %/% 10)) %>%
  summary()


#what is the shelflife of the books? 
data %>% 
  filter(best_rank<6, year<2020) %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  group_by(decade, best_rank) %>% 
  summarise(n = n_distinct(title)) %>% 
  ggplot(aes(x=decade, y = n))+
  geom_bar(stat = "identity", aes(fill = factor(best_rank)))+
  facet_wrap(.~best_rank)# this plot is not working

#somekind of scatter graph
data %>% 
  filter(best_rank==1, year>1989,year<2020) %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  ggplot(aes(y = debut_rank, x  = total_weeks))+
  geom_point(aes(color = decade), position = "jitter")
  


data %>% 
  filter(best_rank==1, year>1989,year<2020) %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  ggplot(aes(x = debut_rank, y  = total_weeks))+
  geom_boxplot(aes(color = decade, group = debut_rank))+
  facet_grid(~decade)

#best graph yet
data %>% 
  filter(best_rank==1,year<2020, total_weeks<150) %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  ggplot(aes(x = debut_rank, y  = total_weeks))+
  geom_point(aes(color = decade, group = debut_rank), 
             alpha = 0.5)+
  facet_grid(~decade)

#seasonality

data %>% 
  filter(best_rank==1, year>1989,year<2020) %>% 
  mutate(decade = factor(10*year %/% 10),
         week = week(first_week)) %>% 
  group_by(decade,week) %>% 
  summarise(n = n_distinct(title)) %>% 
  ggplot(aes(x = week, y = n))+
  geom_point(aes(color = decade))

data %>% 
  filter(best_rank<10, year> 1949,year<2020) %>% 
  mutate(decade = factor(10*year %/% 10),
         month = month(first_week, label = T),
         stage = case_when(year<1970 ~ "pre 1970",
                           year>=1970 & year<2000 ~ "1970-1990s",
                           year>= 2000 ~ "2000 onwards",
                           T ~ "x")) %>% 
  group_by(stage,decade,month) %>% 
  summarise(n = n_distinct(title)) %>% 
  mutate(all_titles = ave(n, decade, FUN = sum),
         pct = n/all_titles) %>%  #nifty way to find sum in a seperate grouping
  ggplot(aes(x = month, y = pct))+
  geom_line(aes(color = decade, group = decade), size = 1.5)+
  facet_wrap(~stage)

#almost there
data %>% 
  filter(best_rank<11, year> 2010) %>% 
  mutate(month = month(first_week, label = T),
         stage = case_when(year<=2015 ~ "2011-2015",
                           year> 2015 ~ "2016-2020",
                           T ~ "x")) %>% 
  group_by(stage,year,month) %>% 
  summarise(n = n_distinct(title)) %>% 
  mutate(all_titles = ave(n, year, FUN = sum),
         pct = n/all_titles) %>%  #nifty way to find sum in a seperate grouping
  ggplot(aes(x = month, y = pct))+
  geom_point(aes(color = factor(stage), group = year), 
            size = 2, alpha = 1,
            position = "jitter")


#adding geom_smooth. This works just fine!
data %>% 
  filter(best_rank<11, year> 2010) %>% 
  mutate(month = month(first_week, label = T),
         stage = case_when(year<=2015 ~ "2011-2015",
                           year> 2015 ~ "2016-2020",
                           T ~ "x")) %>% 
  group_by(stage,year,month) %>% 
  summarise(n = n_distinct(title)) %>% 
  mutate(all_titles = ave(n, year, FUN = sum),
         pct = n/all_titles) %>%  #nifty way to find sum in a seperate grouping
  ggplot(aes(x = month, y = pct, group = 1))+
  geom_point(size = 2, 
            alpha = 0.5, position = "jitter")+
  # geom_boxplot(aes(x = month, y = pct, group = month), 
  #              inherit.aes = F ,
  #              alpha = 0.5)+
  #for geom_smooth to work, grouping must be done in the ggplot function
  #https://stackoverflow.com/a/40600861/7938068
  geom_smooth(se = T) 



# theming for graph 1------------------------------------------------------------

#changing facet labels as shown here https://ggplot2.tidyverse.org/reference/as_labeller.html
facet_labels <- as_labeller(c(`1930`= "1930 to 1939",
                              `1940`= "1940 to 1949",
                              `1950`= "1950 to 1959",
                              `1960`= "1960 to 1969",
                              `1970`= "1970 to 1979",
                              `1980`= "1980 to 1989",
                              `1990`= "1990 to 1999",
                              `2000`= "2000 to 2009",
                              `2010`= "2010 to 2019"))


#annotations for individual facet as discussed here https://stackoverflow.com/a/11889798/7938068
annot_x <- data.frame(debut_rank = 5, 
                      total_weeks = 111,
                      lab = "Each dot\n is a book",
                      decade = 1940)

graph1 <- data %>% 
  filter(best_rank==1,year<2020) %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  ggplot(aes(x = debut_rank, y  = total_weeks))+
  geom_point(aes(color = decade, group = debut_rank))+
  facet_grid(~decade ,labeller = facet_labels)

showtext_begin()



graph1 <- graph1+
  theme_minimal(base_family = "Open Sans")+
  scale_color_brewer(palette = "Paired")+
  labs(title = "Longevity of NYT bestsellers has been decreasing", 
       subtitle = "Analysis of books that reached highest of #1 on the NYT chart tells us that starting from the 1950s, the bestsellers have reduced their longevity - or time spent on the chart.\nFor instance, the top ranked books released in the 50s spent around 52 weeks on the chart while in contrast by the 2010s, they only spent 10 weeks.",
       caption = "TidyTuesday Week 19, 2022\n Prepared by D.S.Ramakant Raju, www.ds-ramakant.com",
       x = "Rank of title on debut week",
       y = "Number of weeks on the bestsellers list")+
  theme(panel.border = element_rect(color = "#2b2b2b", 
                                    fill = NA), #borders for each facet panel
        legend.position = "none", #removing legend
        strip.text = element_text(face = "italic"),
        panel.grid.major.x = element_line(linetype = "dotted", 
                                          color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(size = 8),
        plot.subtitle = element_text(size = 8)) +
  scale_y_continuous(breaks = seq(from = 25, to = 175, by = 25))+
  #annotations by default is applied to all facets
  #for individual facet annotations, check https://stackoverflow.com/a/11889798/7938068
  geom_text(data = annot_x, 
            aes(x = debut_rank, y = total_weeks, 
                family = "Open Sans", alpha = 0.8,
                hjust = -0.2, vjust = -0.2),
            label = annot_x$lab
            )

showtext_end()  

print(graph1)

#DO NOT USE GGSAVE. unsatisfactory output. use the plots window instead
ggsave(filename = "test.png",
  path = ".\\content\\post\\2022-06-01-tidytuesday-nyt-bestsellers-list",
  plot = graph1, 
  bg = "white")



# theming for graph 2 ------------------------------------------------------

showtext_begin()

graph2 <- data %>% 
  filter(best_rank<11, year> 2010) %>% 
  mutate(month = month(first_week, label = T),
         stage = case_when(year<=2015 ~ "2011-2015",
                           year> 2015 ~ "2016-2020",
                           T ~ "x")) %>% 
  group_by(stage,year,month) %>% 
  summarise(n = n_distinct(title)) %>% 
  mutate(all_titles = ave(n, year, FUN = sum),
         pct = n/all_titles) %>%  
  ggplot(aes(x = month, y = pct, group = 1))+
  geom_point(size = 2, 
             alpha = 0.5, position = "jitter")+
  geom_smooth(se = T) 


graph2 <- graph2+
  theme_minimal(base_family = "Open Sans")+
  scale_color_brewer(palette = "Paired")+
  labs(title = "Monthly seasonality of books that featured in the top 10 of NYT Bestsellers list (2010-2019)", 
       subtitle = "Books launched in Summer (Apr-May) or Fall (Sep-Oct) were more likely to make it feature in the top 10",
       caption = "TidyTuesday Week 19, 2022\n Prepared by D.S.Ramakant Raju, www.ds-ramakant.com",
       x = "Months (2010-2019)",
       y = "%age of books launched within that year")+
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     breaks = seq(from = 0, to = 0.2, by= 0.05), 
                     limits = c(0,0.15))+
  theme(axis.line.x = element_line(color = "grey"),
        panel.grid.minor.y = element_blank())

print(graph2)
  
showtext_end()


# intsalling new fonts ----------------------------------------------------


#extra font package is not working
# #loading fonts 
# #https://stackoverflow.com/a/68642855/7938068
# install.packages("extrafont")
# install.packages("remote")
# library(extrafont)
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# font_import() #this is not working
# loadfonts(device = "win")
# #trying another package called showext
# #https://cran.rstudio.com/web/packages/showtext/vignettes/introduction.html

#only showtext package is working

install.packages("showtext")
library(showtext)
showtext_auto()
font_add(family = "Open Sans", 
         regular = "OpenSans-CondLight.ttf", 
         italic = "OpenSans-CondLightItalic.ttf", 
         bold = "OpenSans-CondBold.ttf")
font_paths()

font_families()
font_families_google()
font_files()




# some random chunk to tst fonts ------------------------------------------



#https://www.r-bloggers.com/2013/12/using-system-fonts-in-r-graphs/
library(showtext)

wd = setwd(tempdir())
download.file("http://fontpro.com/download-family.php?file=35701",
              "merienda-r.ttf", mode="wb")
download.file("http://fontpro.com/download-family.php?file=35700",
              "merienda-b.ttf", mode="wb")
font.add("merienda",
         regular = "merienda-r.ttf",
         bold = "merienda-b.ttf")
setwd(wd)

pdf("showtext-ex2.pdf", 7, 4)
plot(1, type = "n", xlab = "", ylab = "")
showtext.begin()
par(family = "merienda")
text(1, 1.2, "R can use this font!", cex = 2)
text(1, 0.8, "And in Bold font face!", font = 2, cex = 2)
showtext.end()
dev.off()
