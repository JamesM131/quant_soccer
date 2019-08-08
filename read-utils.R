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
#' read_moving("Gliders", 2)
read_moving <- function(team, player) {
  # browser()
  all_files <- fs::dir_ls(here::here("data-raw", "Z-example-csv")) 
  
  all_files %>% 
    stringr::str_subset(glue::glue("{team}.+{team}.+_{player}-moving")) %>% 
    readr::read_csv() %>% 
    janitor::clean_names()
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
#' read_landmarks("Gliders", 2)
read_landmarks <- function(team, player) {
  all_files <- fs::dir_ls(here::here("data-raw", "Z-example-csv")) 
  
  all_files %>% 
    stringr::str_subset(glue::glue("{team}.+{team}.+_{player}-landmarks")) %>% 
    readr::read_csv() %>% 
    janitor::clean_names()
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
read_ground_truth <- function() {
  all_files <- fs::dir_ls(here::here("data-raw", "Z-example-csv")) 
  
  all_files %>% 
    stringr::str_subset("groundtruth") %>% 
    readr::read_csv() %>% 
    janitor::clean_names()
}