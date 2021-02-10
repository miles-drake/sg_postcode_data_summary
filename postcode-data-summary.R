# Libraries ---------------------------------------------------------------

library(janitor)
library(readxl)
library(tidyverse)

# Read data sets ----------------------------------------------------------

data_sets <- as.list(NULL)

data_sets[[1]] <- as.list(NULL)
data_sets[[1]]$data$raw <- read_excel("data/postcode-based-geography-2021-1-release-column-profile-comparison.xlsx", sheet = 1)
data_sets[[1]]$append <- "2020_2"

data_sets[[2]] <- as.list(NULL)
data_sets[[2]]$data$raw <- read_excel("data/postcode-based-geography-2021-1-release-column-profile-comparison.xlsx", sheet = 2)
data_sets[[2]]$append <- "2021_1"

# Create pivot tables -----------------------------------------------------

pivot_compare <- function(df, append = NULL){
  df %>% 
    clean_names() %>% 
    # Coerce each column into a character vector
    # Needed to combine numeric and character vectors into a single column
    mutate_if(is.integer, as.character) %>% 
    mutate_if(is.numeric, as.character) %>% 
    pivot_longer(
      cols = -1,
      names_to = "measure",
      values_to = str_c("value", append, sep = "_")
    ) %>% 
    return()
}

for(i in 1:length(data_sets)){
  
  max <- length(data_sets)
  df <- data_sets[[i]]$data$raw
  append <- data_sets[[i]]$append
  
  data_sets[[i]]$data$pivot_long <- pivot_compare(df, append)
  
  # Remove temporary variables
  if(i == max){
    rm(i, max, df, append)
  }
  
}

# Create joined table -----------------------------------------------------

df_join <- NULL

for(i in 1:length(data_sets)){
  
  max <- length(data_sets)
  df <- data_sets[[i]]$data$pivot_long
  
  if(i == 1){
    df_join <- df
  } else {
    df_join <- full_join(
      x = df_join,
      y = df
    )
  }
  
  # Remove temporary variables
  if(i == max){
    rm(i, max, df)
  }
  
}

# Comparisons -------------------------------------------------------------

df_join <- df_join %>% 
  mutate(
    values_equal = case_when(
      # TODO Add support for comparing more than two tables
      is.na(df_join[3]) == TRUE & is.na(df_join[4]) == TRUE ~ TRUE,
      df_join[3] == df_join[4] ~ TRUE,
      TRUE ~ FALSE
    )
  )

df_diff <- df_join %>% 
  filter(
    values_equal == FALSE
  ) %>% 
  select(
    !values_equal
  )

# Export Data Frames to CSV -----------------------------------------------

df_join %>% 
  write_csv(
    file = "export/postcode-data-joined.csv"
  )

df_diff %>% 
  write_csv(
    file = "export/postcode-data-differences-only.csv"
  )
