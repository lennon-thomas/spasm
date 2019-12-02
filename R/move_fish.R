#' \code{move_fish} moves fish via a movement function
#'
#' @param here_pop
#'
#' @return relocated adult fish
#' @export
#'
move_fish <- function(here_pop, num_patches, fish, move_matrix){

  # from Siegal et al. 2003

  # move_foo <- function(numbers, move_matrix) {
  #
  #   moved <- as.numeric(numbers %*% move_matrix)
  #
here_pop[is.na(here_pop)]<-0
move_matrix[is.na(move_matrix)]<-0
  there_pop <- here_pop %>%
    dplyr::group_by(age) %>%
    # mutate(numbers = eigen_mat_mult(matrix(numbers) %>% t(), move_matrix) %>% as.numeric()) %>%
   dplyr:: mutate(numbers = crossprod(numbers, move_matrix) %>% as.numeric()) %>%
    # mutate(numbers = move_foo(numbers, move_matrix)) %>%
    dplyr::ungroup()

  return(there_pop)

}
