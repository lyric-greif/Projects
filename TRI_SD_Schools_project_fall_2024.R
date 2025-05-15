

setwd("C:/Users/C2KSD/Desktop/GIS Assignments and Labs/GIS Final Project")
library(tidyverse)
library(sf)
library(spdep)
library(plotly)
library(spatstat)
library(lfe)
library(ncf)
library(dplyr)
library(readr)
library(tmap)
library(spgwr)
library(sf)
library(sp)
install.packages("rgeos")
install.packages("rtools")
library(rgeos)
library(tmaptools)
library(grid)
library(gridExtra)

#-------------------------------------------------------------------------------------------------------------------------------------

# Median Income Shapefile: clean and prep data 
median_income_data = read.csv("acs_median_income_shapefile.csv")

# Load SD TRI facilities shapefile
sd_tri_facilities = st_read("C:/Users/C2KSD/Desktop/GIS Assignments and Labs/GIS Final Project/sd_tri.shp")

# Load demographic census data
dem_csv = read_csv("dem_data.csv")

# Load TRI csv
tri_csv = read_csv("C:/Users/C2KSD/Desktop/GIS Assignments and Labs/GIS Final Project/sd_tri_facilities.csv")
tri_csv = rename(tri_csv, GEO_ID = GEOID)

# Join demographic and TRI data by census tract geo id
testjoin = left_join(dem_csv, tri_csv, by = "GEO_ID")

# Aggregating data to count facilities per GEOID
tract_summary <- testjoin %>%
  group_by(GEO_ID, totalpop, white, black, hisp_lat, native, asian, assoc, bach, mast, doc_deg, hs_dip, ged, no_uni, english, spanish, sp_englim, med_val, snap, med_inc12, inc_percap, rent_occ) %>%
  summarise(
    fac_total = sum(!is.na(facility_n)),  # Count only non-NA facility names
    Facilities = paste(na.omit(facility_n), collapse = ", "),
    .groups = 'drop'
  )


tract_summary <- mutate(tract_summary, fac_dummy = ifelse(fac_total > 0, 1, 0))

#dummy indicating if there is at least 1 facility
testjoin <- testjoin %>%
  mutate(fac_dummy = ifelse(is.na(facility_n), 0, 1)) # this one is right, NA and 0 if no facility, 1 if there is 

#-------------------------------------------------------------------------------------------------------------------------------------
# Add land use data 
land_use = read_csv("landuse_tract.csv")

# demographic data joined with land use data and TRI sites 
dem1 = right_join(dem_csv, land_use, by = "TL_GEO_ID")


#-------------------------------------------------------------------------------------------------------------------------------------

# Add school data 
schools <- read_csv("sd_schools_and_distance.csv")
scores = read_csv("school_scores_final.csv")

# Create a column for count of chronically absent students
schools$Chron_Ab = schools$Chron_Ab * schools$Cum_enr


model1 = lm(Chron_Ab ~ HubDist + Cum_enr + Hispanic + White + Black, data = schools)
summary(model1) 

# join school and score data

merged_schools = left_join(schools, scores, by = "school_name")

write.csv(merged_schools,"C:/Users/C2KSD/Desktop/GIS Assignments and Labs/GIS Final Project/merged_schools.csv")
st_write(schools_final, "C:/Users/C2KSD/Desktop/GIS Assignments and Labs/GIS Final Project/merged_schools_final1.shp")

schools_final = read_csv("merged_schools_final.csv")


#-------------------------------------------------------------------------------------------------------------------------------------
#convert schools to SF object

school_coords = read.csv("school_coords.csv")

final_schools = left_join(schools_final, school_coords, by = "schoold")

schools_sf_object <- st_as_sf(final_schools, coords = c("x", "y"), crs = 2230)

st_write(schools_sf_object, "C:/Users/C2KSD/Desktop/GIS Assignments and Labs/GIS Final Project/final_schools2.shp")

#-------------------------------------------------------------------------------------------------------------------------------------
# reupload school data to merge with census data 

geo_schools = read.csv("schools_geocoded.csv")
geo_schools2 <- read.csv("schools_geocoded_over100stu.csv")

######### Full data set: census info, school info ###########
# important variables: median income = med_inc_12 ; per capita income = inc_percap ; pop = totalpop
all_data = left_join(geo_schools, dem_csv, by = "TL_GEO_ID")


# convert to shp 
st_write(all_data, "C:/Users/C2KSD/Desktop/GIS Assignments and Labs/GIS Final Project/all_data.shp")

# Create dummies for logit regression
all_data$tri_half_mi = ifelse(all_data$HubDist <= 2640,1,0 ) # half mile away
all_data$tri_one_mi = ifelse(all_data$HubDist <= 5280,1,0 ) # 1mi away
all_data$tri_2_mi = ifelse(all_data$HubDist <= 10560,1,0 ) #2 mile away
all_data$tri_5_mi = ifelse(all_data$HubDist <= 26400,1,0 ) #5 mile away

#-------------------------------------------------------------------------------------------------------------------------------------
#Regressions for schools, test scores, and demographic data

m1 = lm(HubDist ~ med_inc12, data = all_data)
summary(m1) 
plot(m1)

m2 = lm(HubDist ~ med_inc12 + totalpop + white + black + hisp_lat, data = all_data)
summary(m2) 
plot(m2)

# Math standards not met
model2 = lm(mthstnm ~ HubDist + cum_enr + hisp_pr + wht_prc + blck_pr + med_inc12 + totalpop, data = all_data)
summary(model2) 


# Math standards exceeded: TRI 0.5/1/2 miles away
math4 = lm(math4ex ~ tri_half_mi + hisp_pr + wht_prc + blck_pr + med_inc12, data = all_data)
summary(math4, diagnostics = TRUE) 

# normality of resids
residuals = math4$residuals
hist(math4$residuals, breaks = 20)
shapiro.test(residuals)


#ELA standards not met

ela1 = lm(ela1nm ~ HubDist + cum_enr + hisp_pr + wht_prc + blck_pr + med_inc12 + totalpop, data = all_data)
summary(ela1) #not significant 


# ELA standards exceeded

ela4 = lm(ela4ex ~ HubDist + cum_enr + hisp_pr + wht_prc + blck_pr + med_inc12 + totalpop, data = all_data)
summary(ela4)

# chronic absence

chron1 = lm(Chrn_Ab ~ HubDist + cum_enr + hisp_pr + wht_prc + blck_pr + med_inc12, data = all_data)
summary(chron1) 


#-------------------------------------------------------------------------------------------------------------------------------------
# GWR on all_data 

#join with coordinates
schoolcoords = read_csv("school_coords.csv")
all_data_geo = left_join(all_data, schoolcoords, by = "schoold")

# turn to sf object
all_data_sf = st_as_sf(all_data_geo, coords = c("x", "y"), crs = 2230)


sum(is.na(all_data_geo$med_inc12)) 
#imputing NA's in med_inc12 with median data 

medianinc = median(all_data_sf$med_inc12, na.rm = T)
print(medianinc)

all_data_sf$med_inc12[is.na(all_data_sf$med_inc12)] <- median(all_data_geo$med_inc12, na.rm = TRUE)

# Find rows with any NAs across all columns
# Find rows where 'med_inc12' is NA
na_rows_med_inc12 <- which(is.na(all_data_geo$med_inc12))
na_rows_med_inc12


# impute median income value for missing value
all_data_sf <- all_data_sf %>%
  mutate(med_inc12 = if_else(is.na(med_inc12) & schoold == "167", 42722, med_inc12))

# Linear model to gauge relationship

lin1 = lm(math4ex ~ tri_half_mi, data = all_data_sf)
residuals = residuals(lin1)

# residuals
all_data_sf$residuals = lin1$residuals

# join coords to sf data again 
all_data_sf <- all_data_sf %>%
  rename(schoolid = schoold)

data_sf = left_join(all_data_sf, school_coords, by = "schoolid")


# kernel bandwidth 
GWRbandwidth <- gwr.sel(math4ex ~ tri_half_mi + totalpop + med_inc12 + wht_prc + hisp_pr + blck_pr , data = datasf, coords = cbind(datasf$x, datasf$y), adapt = T)


# GWR model 
gwr_model1 = gwr(math4ex ~ tri_half_mi + totalpop + med_inc12 + wht_prc + hisp_pr + blck_pr ,
                 data = datasf, 
                 coords = cbind(datasf$x, datasf$y), 
                 adapt = GWRbandwidth)

summary(gwr_model1)

