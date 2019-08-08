library(tidyverse)

ground_truth <- fs::dir_ls(here::here("data-raw", "Z-example-csv")) %>% 
  str_subset("ground") %>% 
  vroom::vroom() %>% 
  janitor::clean_names()

player_1 <- fs::dir_ls(here::here("data-raw", "Z-example-csv")) %>% 
  str_subset("Helios.+_1-moving")


fs::dir_ls(here::here("data-raw", "Z-example-csv")) %>% 
  str_subset("HELIOS.+HELIOS.+_1-moving")
