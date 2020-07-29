#' Add effects to a Xy simulation
#'
#' With the helping function below you can alter a simulation by simply
#' adding the desired effects to the simulation object from [Xy()].
#' @rdname add_effects
#' @name add_effects
#' @param object an object of class [`xy_recipe`][Xy()].
#' @param p an integer specifying the number of effects to simulate
#' @param family a distributional family (see [families][xy_normal()])
#' @param nlfun a function which transforms the simulated variable
#' @param collinearity a boolean specifying whether there is collinearity between
#'                     the features and uninformative variables
#' @param levels an integer specifying the number of levels within the simulated
#'               discrete variable
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom purrr when
#' @importFrom rlang abort
#' @importFrom stringr str_extract str_detect
#' @return an object of class [`xy_recipe`][Xy()]
#' @examples
#' xy_recipe <- Xy(task = "regression") %>%
#' # add linear features
#' add_linear(p = 5)
#' 
#' # add cubic nonlinear features
#' xy_recipe <- xy_recipe %>%
#' add_nonlinear(p = 3, nlfun = function(x) x^3)
#' 
#' # add discrete features with four unique factor levels
#' xy_recipe <- xy_recipe %>%
#' add_discrete(p = 2, levels = 4)
#' 
#' # add uninformative features (they do not influence the target generating process)
#' xy_recipe <- xy_recipe %>%
#' add_uninformative(p = 5)
#' 
#' # add features from the cauchy distribution
#' xy_recipe <- xy_recipe %>%
#' add_linear(p=2, family = xy_cauchy(location = 3, scale = 5))
#' 
#' # add random interactions between all informative features
#' xy_recipe <- xy_recipe %>%
#' add_interactions()
#' 
#' # add a specific form of noise to your process, e.g. poisson distributed
#' # noise
#' xy_recipe <- xy_recipe %>%
#' add_noise(family = xy_poisson(lambda = 3))
#' 
NULL

#' @export
#' @rdname add_effects
add_linear <- function(object, p, family = xy_normal()) {
  
  object$book <- tibble(type = "linear", 
         name = "x",
         nlfun = list(function(x) x),
         collinearity = NULL,
         params = family %>% pull(params),
         family = family %>% pull(family),
         ) %>%
    # repeat with n
    slice(rep(1:n(), each = p)) %>%
    # bind the current book
    bind_rows(object$book, .)
  
  return(object)
}

#' @export
#' @rdname add_effects
add_nonlinear <- function(object, p, nlfun, family = xy_normal()) {
  
  # fetch nonlinear book entries
  b_nl <- object$book %>%
    filter(type == "nonlinear")
  
  # check whether the body of the function has a usable name
  nlfun_name <- paste0("f(", deparse(body(nlfun)), ")")
  
  # set the name of the nonlinear function
  # CASE 1:
  # try to set the name by the body of the function (works for short functions)
  if (nchar(nlfun_name) < 10) {
    
    name <- nlfun_name
  
  # CASE 2:
  # the function body is too long to use as a suitable name just enumerize
  } else {
  
  # fetch current names
  current_index <- b_nl %>% 
                   # filter down enumerized nonlinear functions
                   filter(str_detect(name, "f\\d")) %>%
                   # pull out names
                   pull(name) %>%
                   # extract digits
                   str_extract(., "\\d+") %>%
                   # coerce to numeric
                   as.numeric() %>%
                   # fetch the maximum
                   purrr::when(length(.) == 0 ~ 0, ~ max(.))
  
  # enumerize
  name <- paste0("f", current_index+1, "(x)")
    
  }
  
    
  # test nlfun
  x <- runif(100)
  if (length(x) != length(nlfun(x))) {
    rlang::abort(paste("Tested your specified function and the output length",
                        "does not match the input length ->",
                        "`length(nlfun(runif(100))!=100`"))
  }
  object$book <- tibble(type = "nonlinear", 
                name = name,
                nlfun = list(nlfun),
                collinearity = NULL,
                params = family %>% pull(params),
                family = family %>% pull(family),
  ) %>%
    # repeat with n
    slice(rep(1:n(), each = p)) %>%
    # bind the current book
    bind_rows(object$book, .)
  return(object)
}

#' @export
#' @rdname add_effects
add_discrete <- function(object, p, levels = 2) {
  
  family <- xy_binom(size = levels, prob = 1/levels)
  
  object$book <- tibble(type = "discrete", 
                name = "discrete",
                nlfun = list(function(x) x),
                collinearity = NULL,
                params = family %>% pull(params),
                family = family %>% pull(family),
  ) %>%
    # repeat with n
    slice(rep(1:n(), each = p)) %>%
    # bind the current book
    bind_rows(object$book, .)
  
  return(object)
  
}

#' @export
#' @rdname add_effects
add_uninformative <- function(object, p, collinearity = FALSE, family = xy_normal()) {
  
  object$book <- tibble(type = "random", 
                name = "random",
                nlfun = list(function(x) x),
                collinearity = collinearity,
                params = family %>% pull(params),
                family = family %>% pull(family),
  ) %>%
    # repeat with n
    slice(rep(1:n(), each = p)) %>%
    # bind the current book
    bind_rows(object$book, .)
  
  return(object)
}

#' @export
#' @rdname add_effects
add_intercept <- function(object) {
  object$intercept <- TRUE
  return(object)
}

#' @export
#' @rdname add_effects
add_noise <- function(object, collinearity = FALSE, family = xy_normal()) {
  
  object$book <- tibble(type = "noise", 
                        name = "e",
                        nlfun = list(function(x) x),
                        collinearity = collinearity,
                        params = family %>% pull(params),
                        family = family %>% pull(family),
  ) %>%
    # bind the current book
    bind_rows(object$book, .)
  
  return(object)
  
}

#' @export
#' @rdname add_effects
add_interactions <- function(object) {
  object$interactions <- TRUE
  return(object)
}