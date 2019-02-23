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

bedroom_ratio_by_race <- merged_data %>% 
  group_by(race) %>% summarise(mean(bedroom_per_person, na.rm = TRUE))

ggplot(bedroom_ratio_by_race, aes(x = race, y = bedroom_ratio_by_race$`mean(bedroom_per_person, na.rm = TRUE)`)) + geom_bar(stat = 'identity')
