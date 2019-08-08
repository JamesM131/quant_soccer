library(tidyverse)

ground_truth <- fs::dir_ls(here::here("data-raw", "Z-example-csv")) %>% 
  str_subset("ground") %>% 
  vroom::vroom() %>% 
  janitor::clean_names()
