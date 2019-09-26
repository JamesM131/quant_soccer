get_all_landmarks <- function(){
  
   teams <- c("HELIOS", "Gliders")
   numbers <- 1:11
   map_length <- length(cross2(teams, numbers))
   pb <- progress::progress_bar$new(total = map_length)
   cross2(teams, numbers) %>% 
     map_df(~{
       pb$tick()
         suppressMessages(get_clean_data(read_landmarks(.x[[1]], .x[[2]]), n_flags = 3))
     })
}



