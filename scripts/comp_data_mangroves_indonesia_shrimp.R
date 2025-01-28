rm(list = ls())

# libraries ------------------
library(tidyverse)
library(testthat)
library(purrr)

# parameters ------------------

# functions ------------------

group_var_by_category <- function(data, category_column, sum_var) {
  data %>%
    group_by(!!sym(category_column)) %>%
    summarise(sum_var = sum(!!sym(sum_var), na.rm = TRUE))
}

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

vars_grouping <- c(
  "country_of_production", "year", "province_of_production", 
  "kabupaten_of_production", "logistics_hub", "port_of_export", 
  "exporter", "importer", "country_of_destination", "economic_bloc"
)

vars_values <- c("volume", "fob")


group_var_by_category <- function(data, category_column, sum_var) {
  data %>%
    group_by(!!sym(category_column)) %>%
    summarise(total = sum(!!sym(sum_var), na.rm = TRUE), .groups = "drop")
}

## check internal consistency of dataset by checking all groups sum to the same

test_that("Group totals match grand total",{
  results <- map(vars_values, ~ {
    # loop over each variable (volume, fob)
    var_value <- .x
    grand_total <- sum(df_shrimp[[var_value]], na.rm = TRUE)
    
    # make sure total value is numeric
    expect_is(grand_total, "numeric")
    
    # within this, loop over each grouping variable (country, exporter, etc)
    map(vars_grouping, ~ {
      var_group <- .x
      grouped_values <- group_var_by_category(df_shrimp, var_group, var_value)
      
      # make sure grouped values are numeric
      expect_is(grouped_values$total, "numeric")
      
      grouped_total <- sum(grouped_values$total, na.rm = TRUE)
      expect_equal(grouped_total, grand_total)
    })
  })
})

# eda ------------------

for (var_ in vars_grouping) {
  print(var_)
  print(summary(df_shrimp[[var_]]))
}

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


