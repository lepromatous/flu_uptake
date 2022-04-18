library(RSocrata)
library(tidyverse)
library(janitor)
library(sf)
library(albersusa)
library(Hmisc)
library(vroom)
library(tidycensus)

# get population by county from tidycensus ----
get_estimates(
  geography="county",
  product = "population"
) %>%
  filter(
    variable == "POP"
  ) %>%
  select(
    -c("NAME")
    ) %>%
  rename(
    fips = "GEOID"
    )-> pops


# get provider locations from CDC----
urlz <- "https://data.cdc.gov/Flu-Vaccinations/Vaccines-gov-Flu-vaccinating-provider-locations/bugr-bbfr"
tokenz<-'chCxsk4zel6QXbaemotF65C9L'

df <- read.socrata(
  urlz,
  app_token = tokenz,
  #####
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!" 
)

# turn provider into sf and project ----
df <- df[!is.na(df$latitude),]
df2 <- sf::st_as_sf(df, coords=c(x = "longitude", y = "latitude"), crs=4326)
df2 <- st_transform(df2, crs='ESRI:102671')

# Get map and project ----
map <- tigris::counties(cb=T, resolution = "20m")
map <- st_transform(map, crs='ESRI:102671')

# spatial join map and data but drop non US States ----
df3 <- sf::st_join(df2, map)
df3 %>%
  filter(loc_admin_state %nin% c("PR", "VI") ) -> df4

# drop geometry, count total providers per county and create rate after merging with county pop ----
df5 <- st_set_geometry(df4, NULL)
names(df5)
df5 %>%
  rename(
    fips = "GEOID"
  ) %>%
  group_by(as.character(fips)) %>%
  summarise(
    n()
  ) %>%
  left_join(
    pops, by=c(`as.character(fips)` = "fips")
  ) %>%
  mutate(
    rate = `n()` / value *10000
  ) %>%
  rename(
    fips = `as.character(fips)`
    )-> df6
  

# pull albers map ----
map2 <- counties_sf()
map2 <- st_transform(map2, crs='ESRI:102671')

mapme <- merge(map2, df6, by="fips", all.x=T)

# make rate per census area ----
mapme %>%
  mutate(
    rate_area = `n()` / census_area *1000
  ) -> mapme

# bin that rate ----
source("~/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/zz useful R code/map binning.R")
mapme2 <- map_breaks(.data = mapme, var = "rate_area", newvar = "rate_area_cat", classes = 4, style = "headtails", clean_labels = TRUE, dig_lab = 10)
  

# map test ----
ggplot() +
  geom_sf(data=mapme2, aes(fill=rate_area_cat)) +
  scale_fill_manual(values=RColorBrewer::brewer.pal(n=6, "Set1"))



# uptake data ---- 
urlz2 <- "https://data.cdc.gov/resource/vh55-3he6.csv"

tokenz<-'chCxsk4zel6QXbaemotF65C9L'

read.socrata(
  urlz2,
  app_token = tokenz,
  #####
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!" 
) %>%
  filter(
    geography_type == "Counties",
    dimension == "≥18 Years",
    year_season == 2019
  ) -> df_uptake

# pad the fips ----
df_uptake$fips <- stringr::str_pad(df_uptake$fips, width=5, pad="0", side="left")

# merge with map ----
df <- merge(mapme2, df_uptake, by="fips", all.x=T)

# clean environment ----

rm(list=setdiff(ls(), c("df")))
gc()
