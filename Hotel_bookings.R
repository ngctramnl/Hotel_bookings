#install.packages('tidyverse')
library(dplyr)
library(tidyverse)
#load the dataset
hotel <- read.csv('hotel_bookings.csv')

#filter out the non-canceled stays
hotel_stays <- hotel %>% 
  filter(is_canceled == 0) %>%
  mutate(children=case_when(
    children+babies > 0 ~ "children",
    TRUE ~ "none"),
    required_car_parking_spaces = case_when(required_car_parking_spaces > 0 ~ "parking",
                                             TRUE ~ "none"),
    
    select(-is_canceled, -reservation_status, -babies)

hotel_stays %>%
  count(children)
#get an idea of the big dataset
#install.packages("skimr")
library(skimr)
skim(hotel_stays)

#exploratory plots
#visualise spike of months with chidren proportion
hotel_stays %>%
  mutate(arrival_date_month = factor(arrival_date_month,
                                          levels= month.name)) %>%
  count(hotel, arrival_date_month, children) %>%
  group_by(hotel, children) %>%
  mutate(proportion= n/sum(n)) %>%
  ggplot(aes(arrival_date_month, proportion, fill=children)) +
  geom_col(position="dodge")+
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~hotel, nrow=2) +
  xlab("Month")
#visualise spike of parking demand with chidren proportion
hotel_stays %>%
  count(required_car_parking_spaces, hotel, children) %>%
  group_by(hotel, children) %>%
  mutate(proportion= n/sum(n)) %>%
  ggplot(aes(required_car_parking_spaces, proportion, fill=children)) +
  geom_col(position="dodge")+
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~hotel, nrow=2)

hotel_stays %>%
  count(adr, children) %>%
  group_by(children) %>%
  mutate(proportion= n/sum(n)) %>%
  ggplot(aes(proportion,adr, fill=children)) +
  geom_boxplot()
  






  

#install.packages('GGally')
library(GGally)
hotel_stays %>%
  select(
    children, adr,
    required_car_parking_spaces,
    total_of_special_requests
  ) %>%
  ggpairs(mapping = aes(color = children))



#build the predictive model

hotels_df <- hotel_stays %>%
  select(
    children, hotel, arrival_date_month, meal, adr, adults,
    required_car_parking_spaces, total_of_special_requests,
    stays_in_week_nights, stays_in_weekend_nights
  ) %>%
  mutate_if(is.character, factor)
install.packages("tidymodels")

