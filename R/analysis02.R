# new variable space_rooms

merged_data <- merged_data %>% 
  mutate('extra_rooms' = (num_of_rooms - num_of_bedrooms))

extra_room_by_borough <- merged_data %>% 
  group_by(borough) %>% 
  summarise(mean(extra_rooms, na.rm = TRUE))

extra_room_by_race <- merged_data %>% 
  group_by(race) %>% 
  summarise(mean(extra_rooms, na.rm = TRUE))

extra_room_by_year <- merged_data %>% 
  group_by(data_year) %>% 
  summarise(mean(extra_rooms, na.rm = TRUE))

extra_room_by_hispanic <-  merged_data %>%
  group_by(hispanic_origin) %>% 
  summarise(mean(extra_rooms, na.rm = TRUE))

mean_income_by_race <- merged_data %>% 
  group_by(race) %>% 
  summarise(mean(total_income, na.rm = TRUE))

ggplot(merged_data, aes(x = gross_rent, y = total_income, color = race)) + geom_point(na.rm = TRUE)

rent_by_condition <- merged_data %>%
  group_by(building_condition) %>% 
  summarise(mean(rent, na.rm = TRUE))

merged_data <- merged_data %>% 
  mutate(room_per_person = num_of_rooms / num_of_persons, bedroom_per_person = num_of_bedrooms / num_of_persons)

room_ratio_by_race <- merged_data %>% 
  group_by(race) %>% summarise(mean(room_per_person, na.rm = TRUE))

income_by_demo <- merged_data %>% 
  group_by(sex, borough, hispanic_origin, race) %>% 
  summarise(mean(total_income, na.rm = TRUE)) %>% 
  rename(mean_income = "mean(total_income, na.rm = TRUE)")

bedroom_ratio_by_race <- merged_data %>% 
  group_by(race) %>% summarise(mean(bedroom_per_person, na.rm = TRUE))

ggplot(bedroom_ratio_by_race, aes(x = race, y = bedroom_ratio_by_race$`mean(bedroom_per_person, na.rm = TRUE)`)) + geom_bar(stat = 'identity')

merged_data <- merged_data %>% 
  filter(total_income < 999995)

top_income <- income_by_demo %>% 
  filter(mean_income > 90000)

renters <- merged_data %>% 
  filter(mortgage == 9)

count(renters) / count(merged_data)

## 70 percent of the sample are renters


renters_income <- renters %>% 
  filter(total_income < 400000, gross_rent < 6000, rent < 6000) %>% 
  mutate(income_per_rent = total_income / gross_rent) %>% 
  group_by(data_year) %>% 
  summarise(mean_income_per_rent = mean(income_per_rent, na.rm = TRUE))

renters_extra_rooms <- renters %>% 
  filter(total_income < 400000, gross_rent < 6000, rent < 6000) %>% 
  mutate(income_per_rent = total_income / gross_rent) %>% 
  group_by(data_year) %>% 
  summarise(mean_extra_room = mean(extra_rooms, na.rm = TRUE))

renters_room_per_person <- renters %>% 
  filter(total_income < 400000, gross_rent < 6000, rent < 6000) %>% 
  mutate(income_per_rent = total_income / gross_rent) %>% 
  group_by(data_year) %>% 
  summarise(mean_room_per_person = mean(room_per_person, na.rm = TRUE))

extra_room_by_year <- merged_data %>% 
  group_by(data_year) %>% 
  summarise(mean(extra_rooms, na.rm = TRUE))

ggplot(renters_income, aes(x = data_year, y = mean_income_per_rent)) + geom_bar(stat = 'identity')

ggplot(renters_extra_rooms, aes(x = data_year, y = mean_extra_room)) + geom_bar(stat = 'identity')

# Paying more than they can afford and getting less non-essential rooms


ggplot(renters_room_per_person, aes(x = data_year, y = mean_room_per_person)) + geom_bar(stat = 'identity')

# But not sacrificing rooms per person
