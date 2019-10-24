#' Read moving file for a player
#'
#' @description This views the file that contains all of the moving objects
#'   visible to a given player at each time throughout the match.
#'
#' @param team Team name as a string (case sensitive)
#' @param player Player number as an integer.
#'
#' @details This will be used to identify the position of other players on the
#' field relative to the subject player. Once the subject player location has
#' been identified using triangulation, this file will then be used to locate
#' other players on the field.
#'
#' @return
#' @export
#'
#' @examples
#' read_moving("Oxsy", 2)
read_moving <- function(team, player, game = 1) {
  
  all_files <- fs::dir_ls("data-raw/X-top5-csv") %>% 
    str_subset(glue::glue("^\\D*\\d+\\D+({game}+)")) %>% 
    fs::dir_ls()
  
  
  # all_files <- fs::dir_ls(here::here("data-raw", "Z-example-csv")) 
  
  relevant_file <- all_files %>% 
    stringr::str_subset(glue::glue("{team}.+{team}_{player}-moving")) 
  
  left_team <- relevant_file %>% 
    stringr::str_extract("(?<=data-raw\\/X-top5-csv\\/\\d-)[a-zA-Z0-9]+")
  
  if(team == left_team) {
    side <- "l"
  } else {
    side <- "r"
  }
  
  return_file <- relevant_file %>% 
    readr::read_csv() %>% 
    janitor::clean_names()
  attr(return_file, "side") <- side
  attr(return_file, "team") <- team
  attr(return_file, "number") <- player
  
  return(return_file)
}

#' Read landmarks for a player
#'
#' @description This reads the file that contains the landmarks that are visible
#'   to a given player at each time in the match.
#'
#' @param team Team name as a string (case sensitive)
#' @param player Player number as an integer.
#' 
#' @details 
#' This will be used to triangulate the subject player's position on the field. 
#' 
#' @return
#' @export
#'
#' @examples
#' read_landmarks("Oxsy", 2)
read_landmarks <- function(team, player, game = 1) {
  # browser()
  # all_files <- fs::dir_ls(here::here("data-raw", "Z-example-csv")) 
  all_files <- fs::dir_ls(here::here("data-raw","Evaluation-Games")) %>%
    str_subset(glue::glue("\\/{game}-")) %>% 
    fs::dir_ls()
  
  
  
  # relevant_file <- all_files %>% 
  #   # stringr::str_subset(glue::glue("{team}.+{team}_{player}-moving")) 
  #   stringr::str_subset(glue::glue("{team}.+{team}\\d*_{player}-landmarks"))
  
  relevant_file <- all_files %>% 
    # stringr::str_subset(glue::glue("{team}.+{team}_{player}-moving")) 
    stringr::str_subset(glue::glue("{team}.+{team}_*[A-Za-z]*_{player}-landmarks"))
  
  left_team <- relevant_file %>% 
    stringr::str_extract("(?<=data-raw\\/Evaluation-Games\\/\\d{1,2}-)[a-zA-Z0-9]+")
  
  if(team == left_team) {
    side <- "l"
  } else {
    side <- "r"
  }
  
  return_file <- relevant_file %>% 
    readr::read_csv() %>% 
    janitor::clean_names()
  attr(return_file, "side") <- side
  attr(return_file, "team") <- team
  attr(return_file, "number") <- player
  
  return(return_file)
}


#' Read Game Ground Truth
#'
#' @description This is the ground truth positions for each of the moving
#' objects in a game (i.e. players and ball)
#'
#' @return
#' @export
#'
#' @examples
read_ground_truth <- function(game) {
  
  all_files <- fs::dir_ls(here::here("data-raw","Evaluation-Games")) %>%
    str_subset(glue::glue("\\/{game}-")) %>% 
    fs::dir_ls()
  
  relevant_file <- all_files %>% 
    stringr::str_subset(glue::glue("groundtruth"))
  
  relevant_file %>% 
    readr::read_csv() %>% 
    janitor::clean_names()
}

read_game <- function(game, line = FALSE) {
  get_all_landmarks(game = game, line = line)
}

read_all_games <- function(n_games, from_game = 1, line = FALSE) {
  # browser()
  games <- from_game:n_games
  
  games %>% 
      map_df(~{
        read_game(.x, line = line)
      })
}
