# SHELDUS Fire Data 

sheldus = read_csv("sheldus_3.1.25.csv")

# 1) count fires by state, county, year 
fires = sheldus %>% 
  group_by(state, county, fips, year, month, Fatalities, Injuries, CropDmg_ADJ_2023, PropertyDmg, PropertyDmg_ADJ_2023, Duration_Days) %>%
  summarise(fire_count = n(), .groups = "drop")


#=======================================================================================================================

# ZORI data 
rent_wide = read.csv("zori.csv")
# Convert to long format and fix date format 
rent_long = rent_wide %>% 
  pivot_longer(
    cols = -c(county, State),
    names_to = "date",
    values_to = "rent"
  ) %>%
  mutate(
    date = sub("^X", "", date),
    date = gsub("\\.", "/", date),
    date = as.Date(date, "%m/%d/%Y")
  )

# -----------------------------------------------------------------------------------------------------

rent_long = rent_long %>%
  rename(
    state = State
  )
# extract year from date column in "fires" 
library(lubridate)

rent_long = rent_long %>%
  mutate(year = year(date))

# extract month column from rent_long
rent_long = rent_long %>%
  mutate(
    month = month(date)    
  )

# join rent_long with FIPS codes 
fips = read.csv("fips.csv")
fips <- fips %>%
  mutate(county = str_trim(county)) # trim trailing spaces

rent_long = rent_long %>% 
  left_join(fips, by = c("state", "county")) 

rent_long$fips.x = NULL #remove extra column


# convert state names to abbreviations
library(stringr)

fires <- fires %>%
  mutate(
    state = str_to_title(state)  # change states from all caps 
  )

state_map <- data.frame(
  full_name = state.name,
  abbrev    = state.abb,
  stringsAsFactors = FALSE
) # vector of state names 

fires <- fires %>%
  left_join(state_map, by = c("state" = "full_name")) %>%
  # rename or create a new column for consistent naming
  
  mutate(state = abbrev) %>% # fix duplicate
  select(-abbrev)


# remove trailing spaces
rent_long <- rent_long %>%
  mutate(county = str_trim(county))
#=================================================================================================
#Merge ZORI and SHELDUS datasets 

rent_fire_df = rent_long %>% 
  left_join(
    fires, 
    by = c("state", "county", "year", "month")
  )   

rent_fire_df = rent_fire_df %>%
  rename(fipscode = fips.y)

rent_fire_df$fips = NULL

rent_fire_df = rent_fire_df %>%
  rename(fips = fipscode)

write.csv(rent_fire_df, file = "rent_fires_updated_df.csv")

#=================================================================================================
# Code time period and first treatment

rent_fire_df = rent_fire_df %>%
  mutate(period = year * 12 + month) # adding monotonic periods (1 unit increase = one month)

group = rent_fire_df %>%
  filter(fire_count == 1) %>%
  group_by(state, county) %>%
  summarize(first_treatment = min(period, na.rm = T), .groups = "drop")

rent_fire_df = rent_fire_df %>%
  left_join(group, by = c("state", "county"))


#=================================================================================================
# Import and join urban-rural continuum codes
urb_rur = read.csv("rural_urban_code.csv")

# trim trailing spaces 
urb_rur <- urb_rur %>%
  mutate(county = str_trim(county))

rent_fire_df = rent_fire_df %>%
  left_join(urb_rur, by = c("state", "county", "fips"))

#rent_fire_df = rent_fire_df %>% 
 # rename(fips = fips.x)
#rent_fire_df$rur_urb_code = NULL

# create dummy for urban vs rural based on continuum 

rent_fire_df = rent_fire_df %>%
  mutate(urban_metro = ifelse(rur_urb_code == 1, 1, 0))

write.csv(rent_fire_df, file = "rent_fire_df_3.3.25.csv")
