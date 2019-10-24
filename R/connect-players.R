get_all_landmarks <- function(n_flags = 3, game = 1, line = FALSE){
  # browser()
   sample_file <- fs::dir_ls(here::here("data-raw","Evaluation-Games")) %>%
      str_subset(glue::glue("\\/{game}-")) %>% 
      fs::dir_ls() %>% 
      pluck(1) 
   team_1 <- sample_file %>% 
      str_extract("(?<=\\d{14}-)([A-Za-z-\\d*]+)")
   
   team_2 <- sample_file %>% 
      str_extract("(?<=_\\d-vs-)([A-Za-z-\\d*]+)")

   # left_team <- sample_file %>% 
   #    stringr::str_extract("(?<=data-raw\\/X-top5-csv\\/\\d-)[a-zA-Z0-9]+")
   
   
   
   teams <- c(team_1, team_2)
   
   numbers <- 1:11
   map_length <- length(cross2(teams, numbers))
   pb <- progress::progress_bar$new(total = map_length)
   cross2(teams, numbers) %>% 
     map_df(~{
        # browser()
       pb$tick()
        if(line) {
           suppressMessages(get_clean_data_line(read_landmarks(.x[[1]], .x[[2]], game = game), n_flags = n_flags, game = game))
        } else {
           suppressMessages(get_clean_data(read_landmarks(.x[[1]], .x[[2]], game = game), n_flags = n_flags, game = game))
        }
     }) %>% 
      mutate(game = game)
}



