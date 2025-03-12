library(sf)
library(tidyverse)
library(janitor)
library(mapview)
library(terra)

## Import and clean boreal files ###########################
# sf package
boreal_shapefile_path <- "./input-data/company-data/boreal extent shapefiles"
shp_boreal_hemi <- st_read(boreal_shapefile_path)
shp_boreal <- shp_boreal_hemi %>% filter(COUNTRY == "CANADA",
                                         TYPE == c("BOREAL","B_ALPINE"))

# make boreal all one layer
shp_boreal <- shp_boreal %>% mutate(TYPE = if_else(TYPE == "B_ALPINE", "BOREAL", TYPE))

# terra package
terra_shp_boreal <- makeValid(vect(boreal_shapefile_path))
terra_shp_boreal <- terra_shp_boreal[terra_shp_boreal$COUNTRY=="CANADA"]
terra_shp_boreal <- terra_shp_boreal[terra_shp_boreal$TYPE != "HEMIBOREAL", ]
terra_shp_boreal <- terra_shp_boreal[terra_shp_boreal$TYPE != "H_ALPINE", ]
terra_shp_boreal$TYPE[terra_shp_boreal$TYPE=="B_ALPINE"] <- "BOREAL"

## Logging --------------

### Import files --------------
shapefile_path <- "./input-data/company-data/can_logging"
shp_logging <- st_read(shapefile_path)
shp_logging <- shp_logging %>%  # add unique identifiers for each concession
  mutate(id = row_number())

### Transform coordinate reference system --------------

# check CRS against boreal
st_crs(shp_logging) # WGS 84 (EPSG:4326) - degrees
st_crs(shp_boreal) # Brandt Albers (NAD83) - m^2

# transform logging to Brandt Albers equal area projection, in m^2
shp_logging <- st_transform(shp_logging, st_crs(shp_boreal))

### Visualise overlap interactively and statically --------------
# just logging
mapview(st_simplify(shp_logging, dTolerance = 1000), col.regions = "red", alpha = 0)

# overlap
mapview(st_simplify(shp_boreal, dTolerance = 1000), alpha = 0.4, zcol = "TYPE") +
  mapview(st_simplify(shp_logging, dTolerance = 1000), col.regions = "red", alpha = 0.4)

# create map figure
canada_bbox <- c(xmin = -141, xmax = -52, ymin = 41, ymax = 83)

ggplot() +
  geom_sf(data = st_simplify(shp_boreal, dTolerance = 1000), aes(fill = TYPE), alpha = 0.4) +
  geom_sf(data = st_simplify(shp_logging, dTolerance = 1000), fill = "red", alpha = 0.4) +
  theme_minimal() +
  labs(fill = "Boreal Type") +
  scale_fill_viridis_d()

### Removing duplicates and tidying logging data --------------

#### All internal overlap ------------
# takes far too long to run
#shp_logging$overlap_internal <- sapply(1:nrow(shp_logging), function(i) {
  #sum(st_area(st_intersection(shp_logging[i, ], shp_logging[-i, ])), na.rm = TRUE)
#})

#shp_logging$overlap_pct <- (shp_logging$overlap_internal / st_area(shp_logging)) * 100

#### Precise duplicates (i.e., same name, same geometry) --------------
logging_duplicates <- shp_logging %>%
  group_by(geometry) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  select(NAME:COMPANY1, Area_ha, Area_km, geometry, id, count) %>%
  filter(count>1)

sum(unique(logging_duplicates$Area_km)) / sum(unique(shp_logging$Area_km)) # 5.7% of area is duplicates

mapview(st_simplify(shp_logging, dTolerance = 1000), col.regions = "red", alpha = 0.4)
mapview(st_simplify(logging_duplicates, dTolerance = 1000), col.regions = "red", alpha = 0.4)

# store duplicates for records
write_csv(st_drop_geometry(logging_duplicates), "./input-data/company-data/canada_logging_duplicate_concessions.csv")

# filter out exact duplicates from main dataset
logging_duplicates_to_keep <- logging_duplicates %>%
  distinct(COMPANY1, Area_ha, .keep_all = TRUE)

logging_duplicates_to_remove <- logging_duplicates %>%
  filter(!id %in% logging_duplicates_to_keep$id)

shp_logging_wo_duplicates <- shp_logging %>%
  filter(!id %in% logging_duplicates_to_remove$id) # remove exact duplicates, while keeping shared concessions
  
sum(unique(shp_logging_wo_duplicates$Area_km)) / sum(unique(shp_logging$Area_km)) # sum to the same area

#### Recalculate concession area in m^2 and from this projection ----------
shp_logging_wo_duplicates <- shp_logging_wo_duplicates %>%
  mutate(area_m2_recalculated = st_area(.),
         Area_km = units::set_units(Area_km, "km^2"),
         area_m2_original = units::set_units(Area_km, "m^2"), # don't need to convert
         area_m2_diff = area_m2_original - area_m2_recalculated) %>%
  rename(area_km2_original = Area_km,
         area_ha_original = Area_ha)

### Calculate overlap --------------

#### sf package --------------

overlap <- st_intersection(shp_logging_wo_duplicates, shp_boreal)

overlap <- overlap %>%
  mutate(overlap_area = st_area(.)) %>% # calculate overlap with each polygon
  group_by(id) %>%
  summarise(overlap_area = sum(overlap_area)) %>%
  ungroup() # NOTE: concesssions with zero overlap area will not appear in this output because st_intersection( st_intersection() does not return rows for zero overlap

# tabulate and join on to original dataset 
df_overlap <- overlap %>% st_drop_geometry()
df_logging_wo_duplicates <- shp_logging_wo_duplicates %>% st_drop_geometry()

df_logging_w_overlap <- df_logging_wo_duplicates %>%
  left_join(df_overlap, by = "id") %>%
  mutate(overlap_area = if_else(is.na(overlap_area),units::set_units(0, "m^2"), overlap_area)) %>%
  rename(overlap_area_m2 = overlap_area) %>% # keep track of units
  mutate(overlap_area_pct_w_recalculated = overlap_area_m2/area_m2_recalculated,
         overlap_area_pct_w_original = overlap_area_m2/area_m2_original)
  
write_csv(df_logging_w_overlap, "./intermediate-results/20250311_canada_logging_overlap.csv")

# overlap statistics - around 63% of logging area overlaps with boreal forest
pct_overlap_total_w_recalculated <- sum(df_logging_w_overlap$overlap_area_m2)/sum(df_logging_w_overlap$area_m2_recalculated)
pct_overlap_total_w_original <- sum(df_logging_w_overlap$overlap_area_m2)/sum(df_logging_w_overlap$area_m2_original)

#### terra package --------------
# check results with terra package to compare processing time and results
terra_shp_logging <- makeValid(vect(shapefile_path))
terra_shp_logging <- project(terra_shp_logging, crs(terra_shp_boreal)) # reproject to equal-area projection
# Recalculate area in square meters
terra_shp_logging$area_m2_recalculated <- expanse(terra_shp_logging, unit="m")
# Convert original Area_km from km² to m² manually
terra_shp_logging$area_m2_original <- terra_shp_logging$Area_km * 1e6
# Compute the difference between the original and recalculated areas
terra_shp_logging$area_m2_diff <- terra_shp_logging$area_m2_original - terra_shp_logging$area_m2_recalculated
# Rename columns
names(terra_shp_logging)[names(terra_shp_logging) == "Area_km"] <- "area_km2_original"
names(terra_shp_logging)[names(terra_shp_logging) == "Area_ha"] <- "area_ha_original"
# add id columns to be able to group later
terra_shp_logging$id <- seq_len(nrow(terra_shp_logging))

# calculate overlap using a clipped version of the logging db to save time
terra_logging_overlap <- intersect(terra_shp_boreal, crop(terra_shp_logging, ext(terra_shp_boreal)))
terra_logging_overlap_area <- expanse(terra_logging_overlap)
df_terra_overlap <- as.data.frame(terra_logging_overlap)
df_terra_overlap$overlap_area <- terra_logging_overlap_area
# group by id to only have one row per concession (i.e., for the different boreal polygons)
df_terra_overlap <- as_tibble(df_terra_overlap, .name_repair = "unique") %>%
  group_by(id) %>%
  summarise(overlap_area = sum(overlap_area, na.rm = TRUE))

df_terra_logging_w_overlap <- as.data.frame(terra_shp_logging) %>%
  left_join(df_terra_overlap, by = "id") %>%
  mutate(overlap_area = if_else(is.na(overlap_area),units::set_units(0, "m^2"), units::set_units(overlap_area, "m^2"))) %>%
  rename(overlap_area_m2 = overlap_area) %>% # keep track of units in names
  mutate(area_km2_original = units::set_units(area_km2_original, "km^2"),
         area_ha_original = units::set_units(area_ha_original, "ha"),
         area_m2_recalculated = units::set_units(area_m2_recalculated), "m^2", 
         area_m2_original = units::set_units(area_m2_original, "m^2")) %>% # set units
  mutate(overlap_area_pct_w_recalculated = overlap_area_m2/area_m2_recalculated,
         overlap_area_pct_w_original = overlap_area_m2/area_m2_original)

# remove duplicates from the dataset
df_terra_logging_w_overlap <- df_terra_logging_w_overlap %>%
  filter(!id %in% logging_duplicates_to_remove$id)

#### comparing terra and sf -------------
terra_pct_overlap_total_w_recalculated <- sum(df_terra_logging_w_overlap$overlap_area_m2)/sum(df_terra_logging_w_overlap$area_m2_recalculated)
terra_pct_overlap_total_w_original <- sum(df_terra_logging_w_overlap$overlap_area_m2)/sum(df_terra_logging_w_overlap$area_m2_original)

terra_pct_overlap_total_w_original - pct_overlap_total_w_original
terra_pct_overlap_total_w_recalculated - pct_overlap_total_w_recalculated

for (df in list(df_logging_w_overlap, df_terra_logging_w_overlap)) {
  top_ten <- as_tibble(df, .name_repair = "unique") %>%
    group_by(COMPANY1) %>%
    summarise(overlap_area_m2 = sum(overlap_area_m2, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(overlap_area_m2))
  print(head(top_ten, n=10))
}

### Tidy up companies ------------

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

## Method 2: yes/no if concession overlaps with boreal ---------------------

shp_logging$near_boreal <- sapply(1:nrow(shp_logging), function(i) {
  any(st_is_within_distance(shp_logging[i, ], shp_boreal, dist = 10000))
})

shp_logging %>% View(
)
