#' Xy
#'
#' The Xy function invokes a framework to simulate supervised learning tasks. With this function
#' you can specify the learning task (regression or classification) and you can
#' adjust specifics such as link functions and cutoff values (e.g. for classification).
#' The outcoming object serves as a mere simulation recipe and has to be enhanced
#' by functions which add effects to the simulation (see [`add_effects`]). Once
#' you have specified the recipe, you can cook the recipe with the [simulate()]
#' method.
#' @param task a character specifying the supervised learning task (e.g. "regression" or "classification").
#' @param link a link function to be used to transform the target.
#' @param cutoff a cutoff function. This function is applied after the link function.
#' 
#' @exportClass xy_recipe
#' @importFrom magrittr "%>%"
#' @importFrom tibble tibble
#' @importFrom purrr set_names
#' @importFrom rlang abort
#' 
#' @author Andre Bleier (\email{andre.bleier@@statworx.com})
#' 
#' @return Returns an object of class [`xy_recipe`][Xy()]. It is a simulation recipe.
#'         which has to be cooked with [simulate()] to render a simulation.
#' @export
#' 
#' @examples
#' # create a recipe
#' recipe <- Xy() %>%
#' add_linear(p = 5, family = xy_normal())
Xy <-       function(task = "regression",
                     link = NULL,
                     cutoff = NULL) {
  
  task_def <- list()
  if (!is.null(link)) {
    if (!is.function(link)) 
      rlang::abort(paste0(sQuote("link"), " has to be a function.")) 
    
    # Test userspecified link function
    test_vec <- rnorm(100, 0, 1)
    res <- link(test_vec)
    if (!is.vector(res)) 
      rlang::abort(paste("Reconsider your link function.",
                 "Your function did not return a vector."))
    if (length(res) != length(test_vec))
      rlang::abort(paste("Reconsider your link function.",
                 "I've tested your function and got an vector of reduced/increased size."))
    
    task_def$link <- link
  }
  if (!is.null(cutoff)) {
    if (!is.function(link)) rlang::abort(paste0(sQuote("cutoff"), " has to be a function.")) 
    task_def$cutoff <- cutoff
    test_vec <- rnorm(100, 0, 1)
    res <- cutoff(test_vec)
    if (!is.vector(res)) 
      rlang::abort(paste("Reconsider your cutoff function.",
                 "Your function did not return a vector."))
    if (length(res) != length(test_vec))
      rlang::abort(paste("Reconsider your cutoff function.",
                 "I've tested your function and got an vector of reduced/increased size."))
    
  }
  
  switch(task, 
         regression = {
           task_def$type <- "regression"
           # use link
           if (is.null(link)) {
             task_def$link <- function(x) x  
           }
           # use cutoff
           if (is.null(cutoff)) {
             task_def$cutoff <- function(x) x 
           }
           
         },
         classification = {
           task_def$type <- "classification"
           # use link
           if (is.null(link)) {
             task_def$link <- function(x) return(exp(x) / (1 + exp(x)))
           }
           # use cutoff
           if (is.null(cutoff)) {
             task_def$cutoff <- function(x) return(ifelse(x >= 0.5, 1, 0))
           }
         },
         {
           task_def$name <- name
           null_check <- sapply(list(cutoff, link), is.null)
           if(any(null_check)) { 
             rlang::abort(paste0("You have to specify a ",
                         paste0(c("cutoff", "link")[null_check], collapse = " and a "),
                         " function for your custom task ",
                         "If you do not want to use this specify: ",
                         sQuote(paste0(paste0(c("cutoff", "link")[null_check], collapse = " = "), " = function(x) x"))))
           }
         }
  )
  
  # create output
  out <- list()
  
  # safe simulation task
  out$task <- task_def
  
  # define empty simulation book
  out$book <- tibble::tibble(type = character(),
                             name = character(),
                             nlfun = list(),
                             collinearity = logical(),
                             params = list(NULL) %>% purrr::set_names(""),
                             family = character())
  
  # define interactions (defaults to no interaction)
  out$interactions <- FALSE
  
  # define intercept (defaults to no intercept)
  out$intercept <- FALSE
  
  class(out) <- "xy_recipe"
  return(out)
}

