rm(list = ls())

# libraries ------------------
library(tidyverse)

# parameters ------------------

# import ------------------

df_shrimp <- read_csv(list.files(path = "./input-data/company-data/",
                                 pattern = "indonesia-shrimp.*\\.csv",
                                 full.names = TRUE),
                      col_types = cols(
                        year = col_double(),
                        country_of_production = col_character(),
                        province_of_production = col_character(),
                        kabupaten_of_production = col_character(),
                        logistics_hub = col_character(),
                        port_of_export = col_character(),
                        exporter = col_character(),
                        importer = col_character(),
                        country_of_destination = col_character(),
                        economic_bloc = col_character(),
                        volume = col_double(),
                        fob = col_double(),
                        province_of_production_trase_id = col_character(),
                        province_of_production_node_sub_type = col_character(),
                        kabupaten_of_production_trase_id = col_character(),
                        country_of_production_trase_id = col_character(),
                        country_of_destination_trase_id = col_character()
                      ))

# cleaning ------------------

# eda ------------------

## how have variables changed through time?
variable_by_year <- function(data, var){
  df <- data %>%
      group_by(year) %>%
      summarise(var = sum(!!sym(var)), na.rm = TRUE)
  
  plot <- ggplot(df,
         mapping = aes(x = year,y = var)) +
    geom_bar(stat = "identity")
  
  print(plot)
}

for(variable in c("volume","fob")) {
  variable_by_year(df_shrimp, var = variable)
}


# findings + visualisation ------------------

# wrap-up + export ------------------


