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

# Select desired columns

relevant_cols_extra <- c('borough' = 'Borough', 'building_condition' = 'Condition of building', 'age' = "Householder's Age Recode", 'sex' = "Householder's Sex", 'race' = "Householder's Race", 'hispanic_origin' = "Householder's Hispanic Origin", "move_in_year" = 'Year Householder Moved into Unit', 'birth_place' = "Place of Householder's Birth", 'mbirth_place' = "Place of Householder's Mother's Birth", 'fbirth_place' = "Place of Householder's Father's Birth", 'value' = "Value", 'mortgage' = "Mortgage Status", 'num_of_rooms' = "Number of rooms", 'num_of_bedrooms' = "Number of bedrooms", 'rent' = "Monthly contract rent", 'immigrant' = "Moved to the U.S. as immigrant", 'immigrant_year' = "Year moved to U.S. as an immigrant", 'affordable' = "Housing Unit Affordability: Apartment (House) is Affordable to Me", 'condition_expensive' = "Housing Unit Affordability: Apartment (House) Too Expensive Given Its Condition", 'location_expensive' = "Housing Unit Affordability: Apartment (House) Too Expensive Given Its Location", 'num_of_persons' = "Number of Persons Recode", 'gross_rent' = "Monthly Gross Rent", 'total_income' = "Total Household Income Recode", 'sb' = "Sub-Borough Area", 'bsb' = "Borough and Sub-Borough Area") # 23 Total

# However, only these desired variables belong to all datasets

relevant_cols <- c('borough' = 'Borough', 'building_condition' = 'Condition of building', 'age' = "Householder's Age Recode", 'sex' = "Householder's Sex", 'race' = "Householder's Race", 'hispanic_origin' = "Householder's Hispanic Origin", "move_in_year" = 'Year Householder Moved into Unit', 'birth_place' = "Place of Householder's Birth", 'mbirth_place' = "Place of Householder's Mother's Birth", 'fbirth_place' = "Place of Householder's Father's Birth", 'value' = "Value", 'mortgage' = "Mortgage Status", 'num_of_rooms' = "Number of rooms", 'num_of_bedrooms' = "Number of bedrooms", 'rent' = "Monthly contract rent", 'num_of_persons' = "Number of Persons Recode", 'gross_rent' = "Monthly Gross Rent", 'total_income' = "Total Household Income Recode", 'sb' = "Sub-Borough Area", 'bsb' = "Borough and Sub-Borough Area") # 20 Total

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

# Merge them all in twos

merge1 <- full_join(data91, data93)
merge2 <- full_join(data96, data99)
merge3 <- full_join(data02, data05)
merge4 <- full_join(data08, data11)
merge5 <- full_join(data14, data17)
merge6 <- full_join(merge1, merge2)
merge7 <- full_join(merge3, merge4)
merge8 <- full_join(merge6, merge7)

merged_data <- full_join(merge8, merge5)

# check to see if all data is represented

count(data91) + count(data93) + count(data96) + count(data99) + count(data02) + count(data05) + count(data08) + count(data11) + count(data14) + count(data17) == count(merged_data)

## TRUE
