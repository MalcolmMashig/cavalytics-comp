
# Load --------------------------------------------------------------------

library(tidyverse)

# read in csv files

csv91 <- "/Users/malcolm_mashig/Downloads/NYCHVS 1991 Occupied File for ASA Challenge_CSV.csv"
csv93 <- "/Users/malcolm_mashig/Downloads/NYCHVS 1993 Occupied File for ASA Challenge_CSV.csv"
csv96 <- "/Users/malcolm_mashig/Downloads/NYCHVS 1996 Occupied File for ASA Challenge_CSV.csv"
csv99 <- "/Users/malcolm_mashig/Downloads/NYCHVS 1999 Occupied File for ASA Challenge_CSV.csv"
csv02 <- "/Users/malcolm_mashig/Downloads/NYCHVS 2002 Occupied File for ASA Challenge_CSV.csv"
csv05 <- "/Users/malcolm_mashig/Downloads/NYCHVS 2005 Occupied File for ASA Challenge_CSV.csv"
csv08 <- "/Users/malcolm_mashig/Downloads/NYCHVS 2008 Occupied File for ASA Challenge_CSV.csv"
csv11 <- "/Users/malcolm_mashig/Downloads/NYCHVS 2011 Occupied File for ASA Challenge_CSV.csv"
csv14 <- "/Users/malcolm_mashig/Downloads/NYCHVS 2014 Occupied File for ASA Challenge_CSV.csv"
csv17 <- "/Users/malcolm_mashig/Downloads/NYCHVS 2017 Occupied File for ASA Challenge_CSV.csv"

# Select ------------------------------------------------------------------

# Select desired columns

relevant_cols_extra <- c('borough' = 'Borough', 'building_condition' = 'Condition of building', 'age' = "Householder's Age Recode", 'sex' = "Householder's Sex", 'race' = "Householder's Race", 'hispanic_origin' = "Householder's Hispanic Origin", "move_in_year" = 'Year Householder Moved into Unit', 'birth_place' = "Place of Householder's Birth", 'mbirth_place' = "Place of Householder's Mother's Birth", 'fbirth_place' = "Place of Householder's Father's Birth", 'mortgage' = "Mortgage Status", 'num_of_rooms' = "Number of rooms", 'num_of_bedrooms' = "Number of bedrooms", 'rent' = "Monthly contract rent", 'immigrant' = "Moved to the U.S. as immigrant", 'immigrant_year' = "Year moved to U.S. as an immigrant", 'affordable' = "Housing Unit Affordability: Apartment (House) is Affordable to Me", 'condition_expensive' = "Housing Unit Affordability: Apartment (House) Too Expensive Given Its Condition", 'location_expensive' = "Housing Unit Affordability: Apartment (House) Too Expensive Given Its Location", 'num_of_persons' = "Number of Persons Recode", 'gross_rent' = "Monthly Gross Rent", 'total_income' = "Total Household Income Recode") # 20 Total

# However, only these desired variables belong to all datasets

relevant_cols <- c('borough' = 'Borough', 'building_condition' = 'Condition of building', 'age' = "Householder's Age Recode", 'sex' = "Householder's Sex", 'race' = "Householder's Race", 'hispanic_origin' = "Householder's Hispanic Origin", "move_in_year" = 'Year Householder Moved into Unit', 'mortgage' = "Mortgage Status", 'num_of_rooms' = "Number of rooms", 'num_of_bedrooms' = "Number of bedrooms", 'rent' = "Monthly contract rent", 'num_of_persons' = "Number of Persons Recode", 'gross_rent' = "Monthly Gross Rent", 'total_income' = "Total Household Income Recode") # 15 Total

# Read --------------------------------------------------------------------

# Read them all in with column for year variable

data91 <- read_csv(csv91, skip = 1) %>% 
  as_tibble() %>% 
  select(relevant_cols) %>% 
  mutate('data_year' = 1991)

data93 <- read_csv(csv93, skip = 1) %>% 
  as_tibble() %>% 
  select(relevant_cols) %>% 
  mutate('data_year' = 1993)

data96 <- read_csv(csv96, skip = 1) %>% 
  as_tibble() %>% 
  select(relevant_cols) %>% 
  mutate('data_year' = 1996)

data99 <- read_csv(csv99, skip = 1) %>% 
  as_tibble() %>% 
  select(relevant_cols) %>% 
  mutate('data_year' = 1999)

data02 <- read_csv(csv02, skip = 1) %>% 
  as_tibble() %>% 
  select(relevant_cols) %>% 
  mutate('data_year' = 2002)

data05 <- read_csv(csv05, skip = 1) %>% 
  as_tibble() %>% 
  select(relevant_cols) %>% 
  mutate('data_year' = 2005)

data08 <- read_csv(csv08, skip = 1) %>% 
  as_tibble() %>% 
  select(relevant_cols) %>% 
  mutate('data_year' = 2008)

data11 <- read_csv(csv11, skip = 1) %>% 
  as_tibble() %>% 
  select(relevant_cols) %>% 
  mutate('data_year' = 2011)

data14 <- read_csv(csv14, skip = 1) %>% 
  as_tibble() %>% 
  select(relevant_cols) %>% 
  mutate('data_year' = 2014)

data17 <- read_csv(csv17, skip = 1) %>% 
  as_tibble() %>% 
  select(relevant_cols) %>% 
  mutate('data_year' = 2017)

# Merge -------------------------------------------------------------------

# Merge them all in twos (function for this?)

merge1 <- full_join(data91, data93)
merge2 <- full_join(data96, data99)
merge3 <- full_join(data02, data05)
merge4 <- full_join(data08, data11)
merge5 <- full_join(data14, data17)
merge6 <- full_join(merge1, merge2)
merge7 <- full_join(merge3, merge4)
merge8 <- full_join(merge6, merge7)
merged_data <- full_join(merge8, merge5)

# check to see if all data is represented (function for this?)

count(data91) + count(data93) + count(data96) + count(data99) + count(data02) + count(data05) + count(data08) + count(data11) + count(data14) + count(data17) == count(merged_data)

## TRUE

# Decoding ----------------------------------------------------------------

# borough

merged_data[merged_data$borough == 1, 'borough'] <- 'bronx'
merged_data[merged_data$borough == 2, 'borough'] <- 'brooklyn'
merged_data[merged_data$borough == 3, 'borough'] <- 'manhattan'
merged_data[merged_data$borough == 4, 'borough'] <- 'queens'
merged_data[merged_data$borough == 5, 'borough'] <- 'staten_island'

# building condition

merged_data[merged_data$building_condition == 1, 'building_condition'] <- 'dilapidated'
merged_data[merged_data$building_condition == 2, 'building_condition'] <- 'sound'
merged_data[merged_data$building_condition == 3, 'building_condition'] <- 'detoriating'
merged_data[merged_data$building_condition == 8, 'building_condition'] <- NA

# Sex

merged_data[merged_data$sex == 1, 'sex'] <- 'male'
merged_data[merged_data$sex == 2, 'sex'] <- 'female'

# Hispanic

merged_data[merged_data$hispanic_origin == 1, 'hispanic_origin'] <- 'non-hispanic'
merged_data[merged_data$hispanic_origin == 2, 'hispanic_origin'] <- 'puerto_rican'
merged_data[merged_data$hispanic_origin == 3, 'hispanic_origin'] <- 'dominican'
merged_data[merged_data$hispanic_origin == 4, 'hispanic_origin'] <- 'cuban'
merged_data[merged_data$hispanic_origin == 5, 'hispanic_origin'] <- 'so_ce_american'
merged_data[merged_data$hispanic_origin == 6, 'hispanic_origin'] <- 'mexican_am_chic'
merged_data[merged_data$hispanic_origin == 7, 'hispanic_origin'] <- 'other_span'
merged_data[merged_data$hispanic_origin == 8, 'hispanic_origin'] <- NA

# Race

merged_data[merged_data$race == 01, 'race'] <- 'white'
merged_data[merged_data$race == 02, 'race'] <- 'black'
merged_data[merged_data$race == 03, 'race'] <- 'native'
merged_data[merged_data$race == 04, 'race'] <- 'chinese'
merged_data[merged_data$race == 05, 'race'] <- 'filipino'
merged_data[merged_data$race == 06, 'race'] <- 'korean'
merged_data[merged_data$race == 07, 'race'] <- 'vietnamese'
merged_data[merged_data$race == 08, 'race'] <- 'asian_indian'
merged_data[merged_data$race == 09, 'race'] <- 'other_asian'
merged_data[merged_data$race == 10, 'race'] <- 'other'
merged_data[merged_data$race == 98, 'race'] <- NA

# Mortgage

merged_data[merged_data$mortgage == 1, 'mortgage'] <- 'yes'
merged_data[merged_data$mortgage == 2, 'mortgage'] <- 'owner'
merged_data[merged_data$mortgage == 8, 'mortgage'] <- NA

# Number of rooms

merged_data[merged_data$num_of_rooms == 9, 'num_of_rooms'] <- NA

# rent amount
merged_data[merged_data$rent == 99998, 'rent'] <- NA

# gross rent

merged_data[merged_data$gross_rent == 99998, 'gross_rent'] <- NA
merged_data[merged_data$gross_rent == 9999, 'gross_rent'] <- NA

# income

merged_data[merged_data$total_income == 999998, 'total_income'] <- NA
merged_data[merged_data == 999999] <- NA
merged_data[merged_data == 9999999] <- NA

# number of bedrooms

merged_data[merged_data$num_of_bedrooms == 98, 'num_of_bedrooms'] <- NA

# in general

merged_data[merged_data == 99999] <- NA
merged_data[merged_data == 9998] <- NA

# Merge them all in twos (function for this?)

merge01 <- full_join(data91, data93)
merge02 <- full_join(data96, data99)
merge03 <- full_join(merge01, merge02)
merged_data <- full_join(data17, merge03)


# check to see if all data is represented (function for this?)

count(data91) + count(data93) + count(data96) + count(data99) + count(data17) == count(merged_data)

