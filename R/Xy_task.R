#' Xy Task 
#'
#' This function creates a task list. Functions within this list are used to
#' transform the target variable of
#' @param name the name of the learning task
#' @param link a link function to be used to transform the target
#' @param cutoff a cutoff function. This function is applied after the link function.
#'               
#' @details The cutoff function is applied after transforming the target with the link function.
#'          It can be any function, however, in a classification learning task this function is used
#'          to classify the linktransformed target into zeroes and ones.
#'          
#' @return The function returns a list with a learning name and two transformation functions
#' @export
#'
#' @examples
#' # Regression
#' # In the regression case the link function is the identity link which
#' # means no transformation at all
#' my_regression_task <- Xy_task(name = "regression")
Xy_task <- function(name = "regression",
                      link = NULL,
                      cutoff = NULL) {
  OUT <- list()
  if (!is.null(link)) {
    if (!is.function(link)) 
      stop(paste0(sQuote("link"), " has to be a function.")) 
    
    # Test userspecified link function
    test_vec <- rnorm(100, 0, 1)
    res <- link(test_vec)
    if (!is.vector(res)) 
      stop(paste("Reconsider your link function.",
                 "Your function did not return a vector."))
    if (length(res) != length(test_vec))
      stop(paste("Reconsider your link function.",
           "I've tested your function and got an vector of reduced/increased size."))
    
    OUT$link <- link
  }
  if (!is.null(cutoff)) {
    if (!is.function(link)) stop(paste0(sQuote("cutoff"), " has to be a function.")) 
    OUT$cutoff <- cutoff
    test_vec <- rnorm(100, 0, 1)
    res <- cutoff(test_vec)
    if (!is.vector(res)) 
      stop(paste("Reconsider your cutoff function.",
                 "Your function did not return a vector."))
    if (length(res) != length(test_vec))
      stop(paste("Reconsider your cutoff function.",
                 "I've tested your function and got an vector of reduced/increased size."))
    
  }
  
  switch(name, 
         regression = {
           OUT$name <- "regression"
           # use link
           if (is.null(link)) {
             OUT$link <- function(x) x  
           }
           # use cutoff
           if (is.null(cutoff)) {
             OUT$cutoff <- function(x) x 
           }
           
         },
         classification = {
           OUT$name <- "classification"
           # use link
           if (is.null(link)) {
             OUT$link <- function(x) return(exp(x) / (1 + exp(x)))
           }
           # use cutoff
           if (is.null(cutoff)) {
             OUT$cutoff <- function(x) return(ifelse(x >= 0.5, 1, 0))
           }
         },
         {
           OUT$name <- name
           null_check <- sapply(list(cutoff, link), is.null)
           if(any(null_check)) { 
              stop(paste0("You have to specify a ",
                          paste0(c("cutoff", "link")[null_check], collapse = " and a "),
                          " function for your custom task. ",
                          "If you do not want to use this specify: ",
                          sQuote(paste0(paste0(c("cutoff", "link")[null_check], collapse = " = "), " = function(x) x"))))
           }
         }
  )
  return(OUT)
}