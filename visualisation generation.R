library(tidyverse)
fs::dir_map(here::here("R"), source)

# Data Prep ---------------------------------------------------------------
ground_truth1_3 <- read_rds(here::here("data", "test_data.rds")) %>% 
  select(x, y)

ground_truth4 <- read_rds(here::here("data", "test_data_line.rds")) %>% 
  select(x, y)

ground_truth5 <- read_rds(here::here("data", "test_data_line.rds")) %>% 
  select(x, y) %>% 
  tail(-1)

ground_truth6_7<- read_rds(here::here("data", "test_data.rds")) %>% 
  select(x, y) %>% 
  tail(-1)

ground_truth <- bind_rows(ground_truth1_3, ground_truth1_3, ground_truth1_3,
                          ground_truth4, ground_truth5, ground_truth6_7, ground_truth6_7)

long_ground_truth <- map_df(1:7, ~ground_truth)

model_data <- fs::dir_ls(here::here("Results Data")) %>% 
  str_subset("[1-9]_") %>% 
  imap_dfr(~{
    read_rds(.x) %>% 
      select(contains("x"), contains("y")) %>% 
      set_names(c("x", "y")) %>%
      mutate(model = .y)
  }) %>% 
  as_tibble()


total_data <- bind_cols(model_data, ground_truth) %>% 
  mutate(x_error = (x - x1)^2, y_error = (y - y1)^2, error = sqrt(x_error + y_error), total_mse = x_error + y_error)


# Image Prep --------------------------------------------------------------
img.file <- here::here("general assets", "field_background.jpg")
img <- jpeg::readJPEG(img.file)


# Where the agents actually are by the error
actual_geometric <- total_data %>% 
  filter(model == 1) %>%
  ggplot(aes(x = x1, y = y1, colour = error)) + 
  annotation_raster(img, xmin = -52.5, x = 52.5, ymin = -34, y = 34) + 
  stat_summary_hex(aes(z = error), fun = "mean", alpha = 0.9) +
  scale_fill_viridis_c("Error", option = "magma") + 
  facet_wrap(~model) +
  ggthemes::theme_fivethirtyeight() 
  

ggsave(here::here("Results Plots", "actual_geometric.jpeg"), actual_geometric)

actual_model <- total_data %>% 
  filter(model != 1) %>%
  ggplot(aes(x = x1, y = y1, colour = error)) + 
  annotation_raster(img, xmin = -52.5, x = 52.5, ymin = -34, y = 34) + 
  stat_summary_hex(aes(z = error), fun = "mean", alpha = 0.9) +
  scale_fill_viridis_c("Error", option = "magma") + 
  facet_wrap(~model) +
  ggthemes::theme_fivethirtyeight() 

ggsave(here::here("Results Plots", "actual_model.jpeg"), actual_model)


# This shows where the agent thinks they are.
predicted_geometric <- total_data %>% 
  filter(model == 1) %>%
  ggplot(aes(x = x, y = y, colour = error)) + 
  annotation_raster(img, xmin = -52.5, x = 52.5, ymin = -34, y = 34) + 
  stat_summary_hex(aes(z = error), fun = "mean", alpha = 0.9) +
  scale_fill_viridis_c("Error", option = "magma") + 
  facet_wrap(~model)+
  ggthemes::theme_fivethirtyeight()

ggsave(here::here("Results Plots", "predicted_geometric.jpeg"), predicted_geometric)


predicted_model <- total_data %>% 
  filter(model != 1) %>%
  ggplot(aes(x = x, y = y, colour = error)) + 
  annotation_raster(img, xmin = -52.5, x = 52.5, ymin = -34, y = 34) + 
  stat_summary_hex(aes(z = error), fun = "mean", alpha = 0.9) +
  scale_fill_viridis_c("Error", option = "magma") + 
  facet_wrap(~model)+ 
  ggthemes::theme_fivethirtyeight()

ggsave(here::here("Results Plots", "predicted_model.jpeg"), predicted_model)


total_data %>% 
  filter(model == 7) %>%
  ggplot(aes(x = x, y = y, colour = error)) + 
  annotation_raster(img, xmin = -52.5, x = 52.5, ymin = -34, y = 34) + 
  stat_summary_hex(aes(z = error), fun = "mean", alpha = 0.9) +
  scale_fill_viridis_c("Error", option = "magma") + 
  facet_wrap(~model)+ 
  ggthemes::theme_fivethirtyeight()