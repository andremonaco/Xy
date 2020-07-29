#' Print method for an object of class `xy_recipe`
#' @param x an object of class [`xy_recipe`][Xy()]
#' @param ... arguments to be passed to the method
#' @import dplyr
#' @examples
#' # print a simulation recipe
#' Xy() %>%
#' add_linear(p = 5) %>%
#' print()
#' @export
print.xy_recipe <- function(x, ...) {
  
  # fetch the simulation recipe
  book <- x$book
  
  # extract interactions
  interactions <- ifelse(x$interactions, 2, 1)
  
  # extract quantity
  n_vars <- book %>%
    group_by(type) %>%
    summarize(n = n(), .groups = "keep")
    
  # extract noise
  book_e <- book %>% filter(type == "noise")
  if (nrow(book_e) == 0) {
    print_e <- "e ~ ?(?)"
  } else {
    print_e <- book %>% pull(name)
  }
  
  # summary ----
  cat(paste0("Xy Simulation Recipe\n"))
  cat(paste0(" \t | \n"))
  cat(paste0(" \t | + task ", x$task$name, "\n"))
  cat(paste0(" \t | + interactions ", interactions, "D", "\n"))
  
  # effects -----
  cat(paste0(" \t | + effects \n"))
  cat(paste0(" \t   | - linear ", n_vars %>% 
               filter(type == "linear") %>% 
               pull(n), "\n"))
  cat(paste0(" \t   | - nonlinear ", n_vars %>% 
               filter(type == "nonlinear") %>% 
               pull(n), "\n"))
  cat(paste0(" \t   | - discrete ", n_vars %>% 
               filter(type == "discrete") %>% 
               pull(n), "\n"))
  cat(paste0(" \t   | - noise ", print_e, "\n"))
  
  return(invisible())
}