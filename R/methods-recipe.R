#' Print method for an object of class `xy_recipe`
#' @param x an object of class [`xy_recipe`][Xy()]
#' @param ... arguments to be passed to the method
#' @import dplyr
#' @importFrom stringr str_pad
#' @importFrom crayon bold red green yellow
#' @examples
#' # print a simulation recipe
#' Xy() %>%
#'   add_linear(p = 5) %>%
#'   print()
#' @export
print.xy_recipe <- function(x, ...) {

  # fetch the simulation recipe
  book <- x$book

  # define variable style
  var_style <- bold$yellow

  # extract quantity
  n_vars <- book %>%
    group_by(type) %>%
    summarize(n = n(), .groups = "keep")

  # check screen width
  screen_width <- as.integer(getOption("width") / 3)

  # extract noise
  book_e <- book %>% filter(type == "noise")
  if (nrow(book_e) == 0) {
    print_e <- "e ~ ?(?)"
    prefix_e <- red("\u2718")
  } else {
    params <- book_e %>% pull(params)
    fam_name <- book_e %>% pull(family)
    print_e <- paste0(
      "e ~ ", fam_name, "(",
      params[[1]] %>%
        paste0(names(.),
          " = ",
          .,
          collapse = ", "
        ),
      ")"
    )
    prefix_e <- prefix_e <- green("\u2714")
  }

  # summary ----
  # summary ----
  cat(bold(paste0("Xy Simulation \n")))
  cat(paste0("\n"))
  cat(bold("recipe\n"))
  cat(paste0(rep("\u25AC", screen_width), collapse = ""), "\n")
  cat(paste0("\u2023 task ", var_style(x$task$type), "\n"))
  interactions <- ifelse(x$interactions, green("\u2714"), red("\u2718"))
  cat(paste(interactions, "interactions", "\n"))

  # intercept
  pre_int <- ifelse(x$intercept, green("\u2714"), red("\u2718"))

  # print intercept
  cat(paste(pre_int, "intercept", "\n"))

  # extract maximum characters for console output
  nchar_max_var <- nchar("uninformative")

  # linear
  lin <- n_vars %>%
    filter(type == "linear") %>%
    pull(n)
  pre_lin <- ifelse(lin == 0, red("\u2718"), green("\u2714"))

  # print lin
  cat(paste(pre_lin, str_pad("linear", nchar_max_var, side = "right"), var_style(lin), "\n"))

  # non linear
  nonlin <- n_vars %>%
    filter(type == "nonlinear") %>%
    pull(n)
  pre_nonlin <- ifelse(nonlin == 0, red("\u2718"), green("\u2714"))

  # print nonlinear
  cat(paste(pre_lin, str_pad("nonlinear", nchar_max_var, side = "right"), var_style(nonlin), "\n"))

  # discrete
  discr <- n_vars %>%
    filter(type == "discrete") %>%
    pull(n)
  pre_discr <- ifelse(nonlin == 0, red("\u2718"), green("\u2714"))

  # print discrete
  cat(paste(pre_discr, str_pad("discrete", nchar_max_var, side = "right"), var_style(discr), "\n"))

  # uninformative
  rand_vars <- n_vars %>%
    filter(type == "random") %>%
    pull(n)
  pre_rand_vars <- ifelse(rand_vars == 0, red("\u2718"), green("\u2714"))

  # print uninformative
  cat(paste(pre_rand_vars, "uninformative", var_style(rand_vars), "\n"))

  # print noise
  cat(paste(green("\u2714"), str_pad("noise", nchar_max_var, side = "right"), var_style(print_e), "\n\n"))

  return(invisible())
}
