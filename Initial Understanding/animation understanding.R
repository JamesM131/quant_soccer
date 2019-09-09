library(gganimate)
# Basic ball
ball_anim <- ground_truth %>% 
  slice(100:nrow(.)) %>% #select(ball_x, ball_y) %>% View()
  ggplot(aes(x = ball_x, y = ball_y)) + 
  geom_point()+ 
  ggtitle("mode: {playmode} at time: {number_time}") + 
  transition_time(number_time)

animate(ball_anim, nframes = 700, fps = 10)

# Ball with pitch
ball_anim <- ground_truth %>% 
  slice(100:400) %>% #select(ball_x, ball_y) %>% View()
  ggplot(aes(x = ball_x, y = ball_y)) + 
  geom_point(aes(group = number_time)) + 
  xlim(-55, 55) + 
  ylim(-35, 35) + 
  geom_segment(x = -50, xend = 50, y = 30, yend = 30, colour = "red") +
  geom_segment(x = -50, xend = 50, y = -30, yend = -30, colour = "red") +
  geom_segment(x = 50, xend = 50, y = -30, yend = 30, colour = "red") +
  geom_segment(x = -50, xend = -50, y = -30, yend = 30, colour = "red") +
  geom_segment(x = 0, xend = 0, y = 30, yend = -30, colour = "blue") +
  transition_time(number_time)+
  theme_bw() +
  ggtitle("mode: {ground_truth[frame_time, 2]} at time: {round(frame_time)}")


animate(ball_anim, nframes = 400, fps = 10)


ground_truth %>% 
  skimr::skim(ball_x, ball_y)
