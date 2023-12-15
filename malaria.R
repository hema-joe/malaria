
#Packages
install.packages("malariaAtlas")
library(malariaAtlas)

install.packages("tidyverse")
library(tidyverse)

install.packages("mapproj") #for `coord_map
library(mapproj)

tanzania_data <- malariaAtlas::getPR (ISO = "TZA", species = "BOTH") 

ggplot2::map_data("world") %>% tbl_df() %>%
  count(region, sort = TRUE) %>% View()


ggplot2::map_data("world") %>% filter(region == "Tanzania")

#Check points
tanzania_data %>%  filter(is.na(year_start)) %>% View()

autoplot(tanzania_data)

class(tanzania_data)

tibble::as_tibble(tanzania_data)

tanzania_data

tanzania_data %>% arrange(year_start) %>% View()
tanzania_data %>% arrange(year_start) %>% count(method, sort = TRUE)


tanzania_data %>% group_by(year_start) %>% 
  summarise(examined = sum(examined),
            positive = sum(positive))


tanzania_data %>% group_by(year_start) %>% 
  summarise(examined = sum(examined),
            positive = sum(positive),
            studies = n())

#Prevalence
tanzania_data %>% group_by(year_start) %>% 
  summarise(examined = sum(examined),
            positive = sum(positive),
            studies = n())%>% 
  mutate(prevalence = positive/examined)


#Graph prevalence
theme_set(theme_light()) #set the theme

tanzania_data %>% group_by(year_start) %>% 
  summarise(examined = sum(examined),
            positive = sum(positive),
            studies = n())%>% 
  mutate(prevalence = positive/examined) %>% 
  ggplot(aes(year_start, prevalence))+
  geom_line()


tanzania_data %>% 
  ggplot(aes(longitude, latitude, color = year_start)) +
  geom_point()


tanzania_data %>% 
  ggplot(aes(longitude, latitude, color = year_start)) +
  geom_point()+
borders("world", regions = "Tanzania")


tanzania_data %>% 
  ggplot(aes(longitude, latitude, color = year_start)) +
  geom_point()+
  borders("world", regions = "Tanzania") + geom_point()


tanzania_data %>% 
  ggplot(aes(longitude, latitude, color = pr)) +
  geom_point()+
  borders("world", regions = "Tanzania") + geom_point()

tanzania_data %>% 
  arrange(pr) %>% 
  ggplot(aes(longitude, latitude, color = pr)) +
  borders("world", regions = "Tanzania") + geom_point() +
  geom_point()+
  scale_color_gradient2(low = "blue", high = "red", midpoint = 0.5,
                        labels = scales::percent_format() )

tanzania_data %>% 
  arrange(pr) %>% 
  ggplot(aes(longitude, latitude, color = pr)) +
  geom_point()+
  borders("world", regions = "Tanzania") + geom_point() +
  scale_color_gradient2(low = "blue", high = "red", midpoint = 0.5,
                        labels = scales::percent_format() ) +
  theme_void() +
  coord_map()


#Group by decade
tanzania_data %>% 
  mutate(decade = 10* (year_start %/% 10)) %>% 
  arrange(pr) %>% 
  ggplot(aes(longitude, latitude, color = pr)) +
  borders ("world", regions = "Tanzania") +
  geom_point() +
  scale_color_gradient2(low = "blue", high = "red", midpoint = 0.5,
                        labels = scales::percent_format() ) +
  facet_wrap(~decade)
  theme_void() +
  coord_map() +
  lab (color = "Prevalence")

#Removing incomplete columns on years
sum (is.na(tanzania_data$year_start))
tanzania_data <- tanzania_data %>%  drop_na (year_start)

#Group by Decade, again
tanzania_data %>% 
  mutate(decade = 10* (year_start %/% 10)) %>% 
  arrange(pr) %>% 
  ggplot(aes(longitude, latitude, color = pr)) +
  borders ("world", regions = "Tanzania") +
  geom_point() +
  scale_color_gradient2(low = "blue", high = "red", midpoint = 0.5,
                        labels = scales::percent_format() ) +
  facet_wrap(~decade)
theme_void() +
  coord_map() +
  lab (color = "Prevalence")




















