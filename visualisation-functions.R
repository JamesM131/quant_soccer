
# install.packages("png")
library(tidyverse)

# Import the image
img.file <- here::here("general assets", "field_background.jpg")

img <- jpeg::readJPEG(img.file)

# Plot with background image
ggplot(iris, aes(Species, Sepal.Length))+
  ggpubr::background_image(img)+
  geom_boxplot(aes(fill = Species), color = "white")


sim_data <- tibble(
  time = 1:1000, 
  x = sample(-70:70, 1000, replace = TRUE), 
  y =  sample(-40:40, 1000, replace = TRUE), 
  error = rnorm(1000, 30, 10)
)

sim_data %>% 
  ggplot(aes(x = x, y = y, colour = error)) + 
  ggpubr::background_image(img)+
  geom_point() + 
  viridis::scale_color_viridis(option="magma")


sim_data %>% 
  ggplot(aes(x = x, y = y, colour = error)) + 
  ggpubr::background_image(img)+
  geom_point(size = 3, alpha = 0.7) + 
  viridis::scale_color_viridis(option="inferno")

# library(gganimate)
# sim_data %>% 
#   ggplot(aes(x = x, y = y, colour = error)) + 
#   ggpubr::background_image(img)+
#   geom_point(size = 3, alpha = 0.7) + 
#   viridis::scale_color_viridis(option="inferno") + 
#   gganimate::transition_time(time = time)

sim_data %>% 
  ggplot(aes(x = x, y = y, colour = error, fill = error)) + 
  ggpubr::background_image(img) +
  geom_density() +
  # geom_density2d(aes(fill = error))
  scale_fill_viridis_c(option = "magma")


# Something interesting to check would be if the left team won the majority of
# the games in the data. This is because that would indicate that they were a
# stronger team and the play would be spend mostly in the right half. The
# visualisations show that the left side of the field was harder to predict than
# the right side of the field.

ground_truth %>% 
  mutate(error =  sqrt((x - predictions$x)^2) + sqrt((y - predictions$y)^2)) %>% 
  ggplot(aes(x = x, y = y, colour = error)) + 
  ggpubr::background_image(img) +
  stat_summary_hex(aes(z = error), fun = "mean", alpha = 0.9) +
  scale_fill_viridis_c(option = "magma")


 test_data %>% 
  mutate(error =  sqrt((x - predictions$x)^2) + sqrt((y - predictions$y)^2)) %>% 
  ggplot(aes(x = x, y = y, colour = error)) + 
  ggpubr::background_image(img) +
  stat_summary_hex(aes(z = error), fun = "mean", alpha = 0.9) +
  scale_fill_viridis_c(option = "inferno")

ggsave(here::here("Results Plots", "lagged_line_nn.jpeg"))



ground_truth %>% 
  mutate(error =  sqrt((x - predictions$x)^2)) %>% 
  ggplot(aes(x = x, y = y, colour = error)) + 
  ggpubr::background_image(img) +
  stat_summary_hex(aes(z = error), fun = "mean", alpha = 0.9) +
  scale_fill_viridis_c(option = "magma")

ground_truth %>% 
  mutate(error =  sqrt((y - predictions$y)^2)) %>% 
  ggplot(aes(x = x, y = y, colour = error)) + 
  ggpubr::background_image(img) +
  stat_summary_hex(aes(z = error), fun = "mean", alpha = 0.9) +
  scale_fill_viridis_c(option = "magma")

  geom_hex()
  geom_point(size = 3, alpha = 0.7)
  
  geom_density() +
  # geom_density2d(aes(fill = error))



ground_truth %>% 
    mutate(error =  sqrt((x - predictions$x)^2) + sqrt((y - predictions$y)^2)) %>% 
    ggplot(aes(x = x, y = y, colour = error)) + 
    ggpubr::background_image(img) +
    stat_density_2d(aes(z = error), fun = "mean", alpha = 0.9) +
    scale_fill_viridis_c(option = "magma")
    stat_density_2d(aes(fill = stat(level)), geom = "polygon")