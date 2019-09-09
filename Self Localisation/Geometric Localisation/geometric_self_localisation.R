cosine_side <- function(c_angle, a_side, b_side) {
  sqrt(
    a_side^2 + b_side^2 - 2 * a_side * b_side * cos(c_angle)
  )
}
sin_angle <- function(a_side, a_angle, b_side) {
  asin(
    b_side*(sin(a_angle)/a_side)
  )
}

rotate_mat <- function(theta) {
  matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2, byrow = TRUE)
}
 
get_position <- function(l1_angle, l1_dist, l2_angle, l2_dist, l1, l2) {
  # Get l1 and l2 positions 
  if(missing(l1) | missing(l2)) {
    stop("The landmark coordinates need to be specified.
         (This should be fixed by either including a list column with 
         these values or by calculating the information in the function)")
  }
  
  l1_l2_angle <- abs(l1_angle - l2_angle)
  
  l1_l2_dist <- cosine_side(c_angle = l1_l2_angle, a_side = l1_dist, b_side = l2_dist)
  
  # The b_side here needs to be the one opposite to <(P L1 L2)
  player_l1_l2_angle <- sin_angle(a_side = l1_l2_dist, a_angle = l1_l2_angle, b_side = l2_dist)
  
  # Temporarily set origin to l1 (This needs to be l1, because it was the centre of the angle found)
  l1_shift = l1 - l1
  l2_shift = l2 - l1
  
  # pt_df <- tibble(x = c(l1_shift[[1]], l2_shift[[1]]), y = c(l1_shift[[2]], l2_shift[[2]]))
  # l1_l2_line <- line(pt_df)
  
  # Get the shifted player
  player_shift_direction <- l2_shift %*% rotate_mat(-player_l1_l2_angle)
  player_shift_unit <- player_shift_direction/sqrt(player_shift_direction[[1]]^2 + player_shift_direction[[2]]^2)
  player_shift <- player_shift_unit * l1_dist
  
  player <- player_shift + l1
  
  return(player)
}

