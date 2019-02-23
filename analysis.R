# new variable space_rooms

merged_data <- merged_data %>% 
  mutate('extra_rooms' = (num_of_rooms - num_of_bedrooms))

extra_room_by_borough <- merged_data %>% 
  group_by(borough) %>% 
  summarise(mean(extra_rooms, na.rm = TRUE))

extra_room_by_race <- merged_data %>% 
  group_by(race) %>% 
  summarise(mean(extra_rooms, na.rm = TRUE))
