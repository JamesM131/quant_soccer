get_clean_data <- function(data, n_flags = 3) {

  long_landmarks <- data %>% 
    filter(number_time != 0) %>% 
    distinct(number_time, .keep_all = TRUE) %>% 
    pivot_longer(-number_time, names_to  = "landmark") %>% 
    filter(is.nan(value) == FALSE) %>% 
    separate(landmark, sep = "_(?=[a-z]{2,})", into = c("landmark", "metric")) %>% 
    filter(metric %in% c("dist", "angle")) %>% 
    pivot_wider(names_from = metric, values_from = value) %>% 
    group_by(number_time) %>% 
    top_n(n_flags, dist) %>% 
    slice(1:n_flags) %>% 
    left_join(read_csv(here::here("data-raw", "landmarks.csv")))
  
  long_independant_variables <- long_landmarks %>% 
    mutate(flag_num = as.character(glue::glue("flag_{1:n()}"))) %>%
    ungroup() %>%
    pivot_longer(dist:y) %>% 
    unite("flag_num", flag_num:name) %>% 
    select(-landmark) %>% 
    pivot_wider(names_from = c(flag_num)) %>% 
    na.omit()
  
  if(attr(data, 'number') == 1) {
    number <- "g1"
  } else {
    number <- attr(data, 'number')
  }
  
  x_coords <- glue::glue("{attr(data, 'side')}{number}_x") %>% 
    rlang::parse_expr()
  
  y_coords <- glue::glue("{attr(data, 'side')}{number}_y") %>% 
    rlang::parse_expr()
  
  long_dependant_variables <- read_ground_truth() %>% 
    select(number_time, x = !!x_coords, y = !!y_coords) %>% 
    distinct(number_time, .keep_all = TRUE) %>% 
    na.omit()
  
  
  connected_data <- long_independant_variables %>% 
    left_join(long_dependant_variables)
  
  return(connected_data)
}
