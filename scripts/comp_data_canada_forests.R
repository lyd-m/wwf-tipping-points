library(sf)
library(tidyverse)
library(janitor)
library(mapview)

shapefile_path <- "can_logging.shp"
shp_logging <- st_read(shapefile_path)
shp_logging <- shp_logging %>%  # add unique identifiers for each concession
  mutate(id = row_number())

boreal_shapefile_path <- "/Users/ucliipp/Library/CloudStorage/OneDrive-SharedLibraries-UniversityCollegeLondon/CEP-IIPP P4NE grant 2019-2021 - Documents/General/WWF tipping points/Main research/Empirical paper/Company research/Boreal-Canada/Raw-data/boreal extent shapefiles/NABoreal.shp"
shp_boreal_hemi <- st_read(boreal_shapefile_path)
attributes_boreal_df <- st_drop_geometry(shp_boreal)
shp_boreal <- shp_boreal_hemi %>% filter(COUNTRY == "CANADA",
                                    TYPE == c("BOREAL","B_ALPINE"))

# transform logging to same CRS
shp_logging <- st_transform(shp_logging, st_crs(shp_boreal))

# visualise on map
mapview(st_simplify(shp_boreal, dTolerance = 1000), alpha = 0.4, zcol = "TYPE") +
  mapview(st_simplify(shp_logging, dTolerance = 1000), col.regions = "red", alpha = 0.4)

## Method 1: calculate overlap -----------
overlap <- st_intersection(shp_logging, shp_boreal)

overlap <- overlap %>%
  mutate(overlap_area = st_area(.)) %>% # calculate overlap with each polygon
  group_by(id) %>%
  summarise(overlap_area = sum(overlap_area)) %>%
  ungroup()

overlap_tabular <- overlap %>%
  st_drop_geometry()

# add to original logging file
shp_logging_w_overlap <- shp_logging %>%
  left_join(overlap_tabular, by = "id") %>%
  mutate(overlap_area = replace_na(overlap_area, units::set_units(0, "m^2")))

### manipulating tabular data
shp_logging_w_overlap_tabular <- shp_logging_w_overlap %>%
  st_drop_geometry() %>%
  mutate(overlap_area_km2 = as.numeric(units::set_units(overlap_area, "km^2")))

# when there are multiple companies, pivot longer and split the area between them equally
shp_logging_w_overlap_tabular_long <- shp_logging_w_overlap_tabular %>%
  pivot_longer(cols = starts_with("COMPANY"), 
               names_to = "company_col", 
               values_to = "company") %>%
  filter(!is.na(company)) %>%
  group_by(id) %>%
  mutate(company_count = n(),
         Area_km = Area_km/company_count,
         Area_ha = Area_ha/company_count) %>%
  select(-company_col, -company_count) %>%
  clean_names %>% # clean column names
  mutate(company = str_to_lower(company),
         company = str_replace_all(company, "\\.", ""))
  
# test that done consistently
sum((shp_logging %>% filter(!is.na(COMPANY1)))$Area_ha)
sum(shp_logging_w_overlap_tabular_long$area_ha)

shp_logging_w_overlap_tabular_long %>%
  group_by(company) %>%
  summarise(area_km = sum(area_km),
            overlap_area_km2 = sum(overlap_area_km2)) %>%
  ungroup() %>% View()

## Method 2: ---------------------

shp_logging$near_boreal <- sapply(1:nrow(shp_logging), function(i) {
  any(st_is_within_distance(shp_logging[i, ], shp_boreal, dist = 10000))
})

shp_logging %>% View(
)
