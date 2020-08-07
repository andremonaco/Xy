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
#' @importFrom crayon bold red green yellow
#' @importFrom stringr str_pad
#' @importFrom glue glue
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

  # define variable style
  var_style <- bold$yellow

  # check screen width
  screen_width <- as.integer(getOption("width") / 3)

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

  # prepare target generating process
  # generate the equation for the target generating process
  tgp_main_effects <- function(x) {

    # fetch data
    data <- x$psi

    # early exit if there are no main effects
    if (sum(diag(x$psi)) == 0) {
      return(character())
    }

    idx <- str_detect(colnames(data), "random|^y$|^e$")
    col_names <- colnames(data)[!idx]
    raw_weights <- diag(data)[!idx]
    col_names[which(col_names == "intercept")] <- ""
    weights <- ifelse(raw_weights < 0,
      paste("{red(\"-\")}", paste("{red(", prettyNum(signif(abs(raw_weights), 2)), ")}")),
      paste("+", prettyNum(signif(raw_weights, 2)))
    )
    out <- paste(weights, paste0("{\"", col_names, "\"}"))

    out[seq(1, length(out), 3)] <- paste0(out[seq(1, length(out), 3)], "\n")
    out <- paste0(out, collapse = "\t\t")
    return(out)
  }

  tgp_interactions <- function(x) {

    # fetch the interaction
    data <- x$psi

    # early exit if there are no interactions
    if (!x$interactions) {
      return(character())
    }
    # overwrite main effects
    diag(data) <- 0
    # fetch names
    col_names <- colnames(data)

    # fetch interactions
    tgp_single_interaction <- function(i, m, k) {

      # find interactions
      interactions <- which(m[, i] != 0)

      # exit if there are no interactions
      if (length(interactions) == 0) {
        return(character())
      }

      # prettify the weights
      pretty_weights <- ifelse(m[interactions, i] < 0,
        paste0("red(- ", signif(abs(m[interactions, i]), 2), ")"),
        paste0("+ ", signif(m[interactions, i], 2))
      )

      # paste together weights (feature_1 * feature_2)
      out <- paste0(pretty_weights,
        "(",
        "bold(, ", k[i], ") * bold(",
        k[interactions],
        "))",
        sep = ""
      ) %>%
        paste0(., collapse = " ")
      return(out)
    }

    out <- sapply(seq_len(ncol(data)),
      FUN = tgp_single_interaction,
      m = data,
      k = col_names
    )

    out[seq(1, length(out), 3)] <- paste0(out[seq(1, length(out), 3)], "\n")

    out <- paste0(out, collapse = " ")
    return(out)
  }

  # filter out random vars, noise and target

  # the main effects
  tgp_main <- tgp_main_effects(x = x)

  # the interaction effects
  tgp_interact <- tgp_interactions(x = x)

  tgp_error <- paste0(" + {bold(print_e)}")

  tgp_start <- "{bold(\"y\")} = "

  tgp <- paste(tgp_start, tgp_main, tgp_interact, "\n", tgp_error)

  # summary ----
  cat(bold(paste0("Xy Simulation \n")))
  cat(paste0("\n"))
  cat(bold("recipe\n"))
  cat(paste0(rep("\u25AC", screen_width), collapse = ""), "\n")
  cat(paste0("\u2023 task ", var_style(x$task), "\n"))
  interactions <- ifelse(x$interactions, green("\u2714"), red("\u2718"))
  cat(paste(interactions, "interactions", "\n"))

  # intercept
  pre_int <- ifelse("intercept" %in% (x$book %>% pull(name)), green("\u2714"), red("\u2718"))

  # print intercept
  cat(paste(pre_int, "intercept", "\n"))

  # effects -----

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

  # simulation
  max_sim_chars <- nchar("correlation interval")
  cat(bold("simulation\n"))
  cat(paste(rep("\u25AC", screen_width), collapse = ""), "\n")
  cat(paste("\u2023", str_pad("n", max_sim_chars, side = "right"), var_style(nrow(x$data)), "\n"))
  cat(paste("\u2023", str_pad("r-squared", max_sim_chars, side = "right"), var_style(x$r_sq), "\n"))
  cat(paste("\u2023 correlation interval", var_style(paste0("[", min(x$cor), ", ", max(x$cor), "]")), "\n"))
  cat(paste("\u2023", str_pad("noise", max_sim_chars, side = "right"), var_style(print_e), "\n"))

  # data generating process ----
  cat("\n")
  cat(bold("Target generating process:"), "\n")

  # check if output is printable
  # do not print if there are more than 50 variables
  if (x$book %>% nrow() > 50) {
    cat(italic("Too many variables output omitted. (use object %>% coef())"))
  } else {
    cat(glue(tgp))
  }
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
