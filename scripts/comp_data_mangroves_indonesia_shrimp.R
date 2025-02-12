rm(list = ls())

# libraries ------------------
library(tidyverse)
library(testthat)
library(purrr)
library(scales)
library(janitor)

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

# cleaning + checking data ------------------

vars_grouping <- c(
  "country_of_production", "year", "province_of_production", 
  "kabupaten_of_production", "logistics_hub", "port_of_export", 
  "exporter", "importer", "country_of_destination", "economic_bloc"
)

vars_values <- c("volume", # traded volume (tonnes) 
                 "fob") # freight/free on board value (USD)

vars_years <- c(2015, 2016, 2017, 2018)

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

## missing values

anyNA(df_shrimp)

# eda ------------------

## general structure
for (var_ in vars_grouping) {
  print(var_)
  print(summary(df_shrimp[[var_]]))
}

## how have variables changed through time?

plot_variable_over_time <- function(data, var){
  df <- data %>%
      group_by(year) %>%
      summarise(total = sum(!!sym(var)), na.rm = TRUE)
  
  plot <- ggplot(df,
         mapping = aes(x = year,y = total)) +
    geom_bar(stat = "identity") +
    labs(title = paste(var),
         y = paste(var))
  
  print(plot)
}

for(var_ in vars_values) {
  plot_variable_over_time(df_shrimp, var = var_)
}

## top 10 for each grouping variable, pct, and total pct of top 10

top_ten_list_all_yrs_total <- list()

for(var_group in vars_grouping) {
  for(var_value in vars_values) {
    col_name <- var_value
    df <- group_var_by_category(df_shrimp, var_group, var_value) %>%
      arrange(desc(total)) %>%
      mutate(rank = row_number(),
             pct = total/sum(total)) %>%
      filter(rank <= 10) %>%
      rename(!!col_name := total)
    
    top_ten_list_all_yrs_total[[paste(var_group, var_value, sep = "_")]] <- df
    
    print(df)
    print(sum(df$pct))
  }
} 

## list for each grouping variable of all factors accounting for min 1% of the value variable

min_1pct_list_all_yrs_total <- list()

for(var_group in vars_grouping) {
  for(var_value in vars_values) {
    col_name <- var_value
    df <- group_var_by_category(df_shrimp, var_group, var_value) %>%
      arrange(desc(total)) %>%
      mutate(rank = row_number(),
             pct = total/sum(total)) %>%
      filter(pct >= 0.01) %>%
      rename(!!col_name := total)
    
    min_1pct_list_all_yrs_total[[paste(var_group, var_value, sep = "_")]] <- df
    
    print(df, n=25)
    print(sum(df$pct))
  }
} 

# Let's check whether the top ten changes if you look at annual data

top_ten_list_by_yr <- list()

for(var_group in vars_grouping) {
  for(var_value in vars_values) {
    for(year_ in vars_years) {
    col_name <- var_value
    df <- df_shrimp %>%
      filter(year == year_) %>%
      group_var_by_category(var_group, var_value) %>%
      arrange(desc(total)) %>%
      mutate(rank = row_number(),
             pct = total/sum(total)) %>%
      filter(rank <= 10) %>%
      rename(!!col_name := total)
    
    top_ten_list_by_yr[[paste(var_group, var_value, year_, sep = "_")]] <- df
    
    print(year_)
    print(df)
    print(sum(df$pct))
    }
  }
}

## comparing the top ten exportersusing cosine similarity

library(lsa)
list1 <- top_ten_list_by_yr[["exporter_volume_2015"]]$exporter
list2 <- top_ten_list_by_yr[["exporter_volume_2016"]]$exporter
list3 <- top_ten_list_by_yr[["exporter_volume_2017"]]$exporter
list4 <- top_ten_list_by_yr[["exporter_volume_2018"]]$exporter

# Jaccard Similarity function
jaccard_similarity <- function(listA, listB) {
  length(intersect(listA, listB)) / length(union(listA, listB))
}

# Compare the lists pairwise
pairwise_comparisons <- list(
  c1c2 = jaccard_similarity(list1, list2),
  c1c3 = jaccard_similarity(list1, list3),
  c1c4 = jaccard_similarity(list1, list4),
  c2c3 = jaccard_similarity(list2, list3),
  c2c4 = jaccard_similarity(list2, list4),
  c3c4 = jaccard_similarity(list3, list4)
)

pairwise_comparisons

# findings + visualisation ------------------

indonesia_shrimp_top_ten <- top_ten_list_by_yr[["exporter_volume_2018"]] %>%
  mutate(exporter = str_to_title(exporter),
         pct_neat = percent(pct, accuracy = 0.1))
save(indonesia_shrimp_top_ten, file = "./intermediate-results/companies_indonesia_shrimp.RData")

# wrap-up + export ------------------


