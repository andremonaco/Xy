# PULL --------------------------------------------------------------------
#' @rdname pulls
#' @name pulls
#' @title Extract the simulated effects out of a cooked simulation recipe
#' @details The pull functions enable a convenient extraction of certain simulated
#'          effects according to the desire of the user.
#'          With `pull_*` you can simply specify which effects you want to
#'          extract out of the simulation
#' @param object an object of class [`xy_sim`][simulate()]
#' @import dplyr tibble
#' @importFrom rlang abort
#' @return Returns the desired effects in a tibble.
#' @examples
#' # create a simulation
#' linear_sim <- Xy() %>%
#' add_linear(p = 5) %>%
#' simulate(n=100)
#' # fetch the target
#' y <- linear_sim %>% pull_y()
#' # fetch all features
#' X <- linear_sim %>% pull_x()
#' # fetch the features and the target
#' xy <- linear_sim %>% pull_xy()
#' # fetch the residuals of the simulation
#' e <- linear_sim %>% pull_e()
NULL

#' @export
#' @name pulls
pull_x <- function(object) {
  
  # check input
  if (!inherits(object, "xy_sim")) {
    rlang::abort("object must be of class xy_sim")
  }
  
  out <- object$data %>%
    select(-y, -e)
  return(out)
}

#' @export
#' @name pulls
pull_y <- function(object) {
  
  # check input
  if (!inherits(object, "xy_sim")) {
    rlang::abort("object must be of class xy_sim")
  }
  
  out <- object$data %>%
    select(y)
  return(out)
}

#' @export
#' @name pulls
pull_e <- function(object) {
  
  # check input
  if (!inherits(object, "xy_sim")) {
    rlang::abort("object must be of class xy_sim")
  }
  
  out <- object$data %>%
    select(e)
  return(out)
}

#' @export
#' @name pulls
pull_xy <- function(object) {
  
  # check input
  if (!inherits(object, "xy_sim")) {
    rlang::abort("object must be of class xy_sim")
  }
  
  out <- object$data %>%
    select(-e)
  return(out)
}

#' @export
#' @name pulls
pull_xye <- function(object) {
  
  # check input
  if (!inherits(object, "xy_sim")) {
    rlang::abort("object must be of class xy_sim")
  }
  
  out <- object$data
  return(out)
}