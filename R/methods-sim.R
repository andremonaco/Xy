#' @title Methods of the class [`xy_sim`][Xy()]
#' @details With the help of these methods you can further manipulate a cooked
#'          simulation.
#' * `print.xy_sim()`: Gives you an overview of the simulation
#' * `coef.xy_sim()`: Extracts the beta coeffecients of the simulation from
#'                    \eqn{y = X\beta + e}
#' * `plot.xy_sim()`: Will plot the true effects of the simulation, e.g. X vs y
#' * `transform.xy_sim()`: Will return the adjusted simulated data, i.e. it will
#'                         apply all nonlinear transformations to the raw simulated
#'                         effects and multiply the X by its beta coefficient.
#'                         This function is mostly used internally, however,
#'                         exposed to the user as it could be needed in edge
#'                         cases.
#' * `equation.xy_sim()`: Will return a formula object which can be forwarded
#'                        to the machine learning algorithm. Note: Uninformative
#'                        features are added as well.
#' @rdname xy_sim
#' @param x an object of class [`xy_sim`][Xy()]
#' @param object an object of class [`xy_sim`][Xy()]
#' @param _data an object of class [`xy_sim`][Xy()]
#' @param ... additional parameters
#' @import ggplot2 dplyr tibble
#' @importFrom tidyr gather
#' @importFrom purrr map2
#' @importFrom rlang abort
#'
#' @examples
#' # create a simulation
#' linear_sim <- Xy() %>%
#'   add_linear(p = 5) %>%
#'   simulate(n = 100)
#'
#' # print the simulation
#' simulation_info <- linear_sim %>% print()
#'
#' # get the coefficients of the features
#' simulation_coefs <- linear_sim %>% coef()
#'
#' # plot the underlying true effect of X
#' simulation_plot <- linear_sim %>% plot()
#'
#' # transform the data of the simulation such that the features are transformed
#' # e.g. nonlinear features are scaled by their functions.
#' transformed_simulation <- linear_sim %>% transform()
#'
#' # fetch the formula
#' eqn <- linear_sim %>% equation()
#' @name xy_sim
#'
NULL


# PRINT -------------------------------------------------------------------

#' @export
#' @name xy_sim
print.xy_sim <- function(x, ...) {

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
  }

  # summary ----
  cat(paste0("Xy Simulation \n"))
  cat(paste0(" \t | \n"))
  cat(paste0(" \t | + task ", x$task, "\n"))
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
  if ("intercept" %in% (book %>% pull(name))) {
    cat(paste0(" \t   | - intercept\n"))
  }

  cat(paste0(" \t   | - noise ", print_e, "\n"))

  # simulation
  cat(paste0(" \t | + simulation \n"))
  cat(paste0(" \t   | - n ", nrow(x$data), "\n"))
  cat(paste0(" \t   | - r-squared ", x$r_sq, "\n"))
  cat(paste0(" \t   | - correlation interval ", "[", min(x$cor), ", ", max(x$cor), "]", "\n"))

  # data generating process ----
  cat("\n")
  cat("\n")
  cat(paste0(x$tgp))
  cat("\n")

  return(invisible())
}

# COEF --------------------------------------------------------------------

#' @export
#' @name xy_sim
coef.xy_sim <- function(object, ...) {
  effects <- !grepl("^y|^e", colnames(object$psi))
  out <- diag(object$psi)[effects]
  names(out) <- colnames(object$psi)[effects]
  return(out)
}

# PLOT --------------------------------------------------------------------

#' @name xy_sim
#' @export
plot.xy_sim <- function(x, ...) {

  # get data
  plt_df <- x %>%
    pull_xy() %>%
    select(-matches("intercept|xd|random")) %>%
    tidyr::gather(key = "effect", value = "value", -y)

  effects_plt <- ggplot(plt_df, aes_string(x = "value", y = "y"))

  if (nrow(x$data) > 1000) {
    effects_plt <- effects_plt + geom_hex(colour = "#13235B")
  } else {
    effects_plt <- effects_plt + geom_point(colour = "#13235B")
  }

  effects_plt <- effects_plt +
    facet_wrap(formula(paste0("~ effect")), scales = "free") +
    theme_minimal(base_size = 14) +
    xlab("") +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    ggtitle("True effects X vs y")

  if (nrow(x$data) <= 1000) {
    effects_plt <- effects_plt +
      geom_smooth(formula = y ~ s(x, bs = "tp"), method = "gam", colour = "#00C792")
  }

  print(effects_plt)
  return(effects_plt)
}

# TRANSFORM ---------------------------------------------------------------

#' @export
#' @name xy_sim
transform.xy_sim <- function(`_data`, ...) {

  # get data
  sim_mat <- `_data` %>% pull_xye()

  # fetch nonlinear functions
  nlfuns <- `_data`$book %>%
    pull(nlfun)

  # transform nonlinear
  nl_mat <- sim_mat %>%
    purrr::map2(.x = ., .y = nlfuns, .f = ~ .y(.x)) %>%
    as.data.frame() %>%
    as.matrix()

  # add weights
  out <- nl_mat %*% `_data`$psi
  return(out)
}

# EQUATION ----------------------------------------------------------------

#' @export
#' @name xy_sim
equation <- function(object, ...) {
  out <- object$eq
  return(out)
}
