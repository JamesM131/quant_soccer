# https://math.stackexchange.com/questions/2424610/find-3rd-point-of-triangle-knowing-a-and-b-coordinates-and-all-the-angles?noredirect=1&lq=1


# These are the correct numbers, but should be negative
-1 * cos(-1.5) - 3 * sin(-1.5)

-1 * sin(-1.5) + 3 * cos(-1.5)


# Set up example data
# 𝐴=(1,4),𝐵=(2,1),𝛼=1.5,𝛽=0.5

A <- c(1, 4)
B <- c(2, 1)
alpha <- 1.5 # angle cab
beta <- 0.5
c_distance <- 2

## Shift by A
A_shift <- A - A
B_shift <- B - A

rotate_mat <- function(theta) {
  matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2, byrow = TRUE)
}

B_2_shift <- B_shift %*% rotate_mat(alpha)

pt_df <- tibble(x = c(A_shift[[1]], B_shift[[1]]), y = c(A_shift[[2]], B_shift[[2]]))
pt_df2 <- tibble(x = c(A_shift[[1]], B_2_shift[[1]]), y = c(A_shift[[2]], B_2_shift[[2]]))

line_1 <- line(pt_df)
line_2 <- line(pt_df2)

pt_df %>%
  ggplot(aes(x = x, y = y)) +
  xlim(-5, 5) +
  ylim(-5, 5) +
  geom_point() +
  geom_abline(slope = coef(line_1)[[2]], intercept = coef(line_1)[[1]]) +
  geom_abline(slope = coef(line_2)[[2]], intercept = coef(line_2)[[1]]) +
  geom_point(x = B_2_shift[[1]], y = B_2_shift[[2]], colour = "red")


b_2_shift_unit <- B_2_shift/sqrt(B_2_shift[[1]]^2 + B_2_shift[[2]]^2)


c_shift <- b_2_shift_unit * c_distance

C <- c_shift + A ####################

pt_df <- tibble(x = c(A[[1]], B[[1]], C[[1]]), y = c(A[[2]], B[[2]], C[[1]]))



pt_df %>%
  ggplot(aes(x = x, y = y)) +
  xlim(-5, 5) +
  ylim(-5, 5) +
  geom_point()

A_2 <- A %*% rotate_mat(1.5)


pt_df %>%
  ggplot(aes(x = x, y = y)) +
  xlim(-5, 5) +
  ylim(-5, 5) +
  geom_point() +
  geom_abline(slope = coef(line_1)[[2]], intercept = coef(line_1)[[1]]) +
  geom_abline(slope = coef(line_2)[[2]], intercept = coef(line_2)[[1]]) +
  geom_point(x = c_shift[[1]], y = c_shift[[2]], colour = "red")



pt_df <- tibble(x = c(A[[1]], B[[1]]), y = c(A[[2]], B[[2]]))
pt_df2 <- tibble(x = c(A[[1]], A_2[[1]]), y = c(A[[2]], A_2[[2]]))

line_1 <- line(pt_df)
line_2 <- line(pt_df2)

pt_df %>%
  ggplot(aes(x = x, y = y)) +
  xlim(-5, 5) +
  ylim(-5, 5) +
  geom_point() +
  geom_abline(slope = coef(line_1)[[2]], intercept = coef(line_1)[[1]]) +
  geom_abline(slope = coef(line_2)[[2]], intercept = coef(line_2)[[1]]) +
  geom_point(x = A_2[[1]], y = A_2[[2]], colour = "red")


atan(
  abs(
    (coef(line_1)[[2]] - coef(line_2)[[2]]) / (1 + coef(line_1)[[2]] * coef(line_2)[[2]])
  )
)


tan(
  atan(coef(line_1)[[2]]) + 1.5
)