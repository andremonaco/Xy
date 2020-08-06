#' Cook a simulation recipe of [Xy()]
#'
#' A function which simulates linear and nonlinear X and a corresponding
#' target. The composition of the target is highly customizable.
#' Furthermore, the polynomial degree as well as the functional shape of
#' nonlinearity can be specified by the user. Additionally coviarance structure
#' of the X can either be sampled by the function or specifically
#' determined by the user.
#' @rdname simulate
#' @param object an object of class [`xy_recipe`][Xy()].
#' @param n an integer specifying the desired number of obervations.
#' @param r_squared a numeric value between 0 and 1.
#' @param cor_interval a vector of length 2, which provides the correlation
#'                     sample interval for the correlation matrix.
#' @param cor_matrix a (positive semidefinite) correlation matrix.
#'                   This matrix inherits the desired correlation structure of
#'                   the simulated effects. Defaults to `NULL`,
#'                   which means the structure will be sampled from `cor`
#' @param ... additional parameters
#' @details The simulation uses a copula backend and can simulate effects from
#'          various distributions.
#' * `r_squared`: The simulation adjusts the noise of the target generating
#'                process according to the user specified input of the R^2.
#'                Hence, if the user chooses to set a high R^2 then the noise
#'                will have little to no effect on the target generating
#'                process. Choosing lower R^2 yields to a stronger noise.
#'                Thus, when fitting an ML model, the `r_squared` is the
#'                upper bound of achievable adjusted r_squared.
#'
#' @import dplyr tibble
#' @importFrom Matrix bdiag
#' @importFrom stringr str_detect str_replace
#' @importFrom rlang abort
#' @importFrom purrr map2 when as_vector possibly map_dbl map_int set_names
#' @importFrom copula p2P ellipCopula mvdc rMvdc
#' @exportClass xy_sim
#'
#' @author Andre Bleier (\email{andre.bleier@@statworx.com})
#'
#' @return An object of class [`xy_sim`][simulate()], which contains meta information about the
#'         simulation. The user can manipulate this object with methods from
#'         the class [`xy_sim`][simulate()], e.g. [importance()].
#' @export
#' @name simulate
#' @examples
#' # create a recipe
#' recipe <- Xy() %>%
#'   add_linear(p = 5, family = xy_weibull())
#' # cook the recipe
#' recipe %>% simulate(n = 10)
#' @export
simulate <- function(object,
                     n = 1000,
                     r_squared = 0.8,
                     cor_interval = c(-.5, .5),
                     cor_matrix = NULL,
                     ...) {

  # input check
  if (!inherits(object, "xy_recipe")) {
    rlang::abort("object must be of class xy_recipe")
  }

  # check for empty simulation book
  if (nrow(object$book) == 0) {
    rlang::abort(paste(
      "the simulation book is empty. add some simulation",
      "effects (see help(add_linear)"
    ))
  }

  # check if noise is added and add noise if none was specified
  if (nrow(object$book %>% dplyr::filter(type == "noise")) == 0) {
    object <- object %>%
      add_noise()
  }

  # fix naming of the variables
  object$book <- object$book %>%
    group_by(name) %>%
    mutate(
      name = paste(name, 1:n(), sep = "_"),
      name = replace(name, type == "noise", "e")
    ) %>%
    arrange(type)

  # extract book
  book <- object$book

  # functions -----
  # adds interaction terms to the process
  sample_interactions <- function(x) {
    int_total <- ncol(x)

    random_weights <-
      round(
        runif(
          int_total,
          # min value to sample interaction weights from
          -10 * 1e-3,
          # max value to sample interaction weights from
          10 * 1e-3
        ),
        3
      )

    int_weights <- sample(c(0, random_weights),
      size = int_total * (int_total - 1) / 2,
      replace = TRUE,
      # TODO: Check probabilities might need to do 0.9 0.1
      prob = c(0.9, rep(0.1 / int_total, int_total))
    )

    # get interaction effects
    out <- copula::p2P(int_weights)

    # restore main effects
    diag(out) <- diag(x)

    # delete pure effect if there is an interaction
    diag_vals <- sapply(seq_len(ncol(out)),
      function(i, int_mat) {
        if (sum(int_mat[, i] != 0) > 1) {
          return(0)
        } else {
          return(int_mat[i, i])
        }
      },
      int_mat = out
    )

    # set diagonal to initial weights
    diag(out) <- diag_vals

    # remove main effects if there is an interaction with this variable
    # fetch indices
    int_idx <- apply(out,
      MARGIN = 2, FUN = function(x) sum(x != 0) > 1
    )

    # adjust main effects
    diag(out)[int_idx] <- 0

    # discard doubling effects
    out[upper.tri(out)] <- 0

    return(out)
  }

  # function to create continuous variables
  sim_copula <- function(n_obs,
                         sim_book,
                         cor_int,
                         cor_mat) {

    # check if there are any numerical variables
    total <- sim_book %>%
      nrow(.)

    # handle correlation matrix ----
    # CASE 1:
    # User specified correlation matrix
    if (!is.null(cor_mat)) {

      # force to matrix
      sigma <- tryCatch(
        {
          as.matrix(cor_mat)
        },
        error = function(e) {
          return(NA)
        }
      )

      # INPUT HANDLING: matrix coercion failed
      if (all(is.na(sigma))) {
        rlang::abort(paste0(
          "Tried to coerce", sQuote("cor_matrix"),
          " to a matrix, but could not succeed."
        ))
      }

      # INPUT HANDLING: check whether the user specified matrix has the right
      #                 dimensions.
      if (ncol(sigma) != total) {
        rlang::abort(paste0(
          "The user-specified correlation matrix",
          " has insufficient columns: ",
          ncol(sigma), " for ",
          total, " variables. Reconsider ",
          sQuote("cor_matrix"), "."
        ))
      }

      # force correlation matrix to be symmetric
      sigma <- Matrix::forceSymmetric(sigma, uplo = "L")

      cor_vec <- copula::P2p(sigma)

      # CASE 2:
      # no user specified matrix. will sample from the given correlation bounds
    } else {

      # simulate correlations
      cor_vec <- stats::runif(
        total * (total - 1) / 2,
        min(cor_int), max(cor_int)
      )

      # delete correlation as specified by the user
      tmp_mat <- copula::p2P(cor_vec)
      # find variables without correlation
      no_cor <- which(sim_book$collinearity)
      # overwrite correlation with zero
      tmp_mat[no_cor, ] <- 0
      # coerce to lower tri vector again
      cor_vec <- copula::P2p(tmp_mat)
    }

    # fetch margins
    margins <- sim_book %>%
      pull(family)

    # fetch parameters of the margins
    param_margins <- sim_book %>%
      pull(params)

    # create the copula
    mod_copula <- copula::ellipCopula(
      family = "normal",
      param = cor_vec,
      dim = total,
      dispstr = "un"
    ) %>%
      copula::mvdc(
        copula = .,
        margins = margins,
        paramMargins = param_margins
      )

    # draw a random sample from the copula
    out <- suppressWarnings(copula::rMvdc(n_obs, mod_copula))

    return(out)
  }

  tgp_main_effects <- function(mat, cnames) {
    ind <- which(diag(mat) != 0)
    int_mat_subset <- diag(mat)[ind]
    weights <- ifelse(int_mat_subset < 0,
      paste0("- ", abs(int_mat_subset)),
      paste0("+ ", int_mat_subset)
    )
    out <- paste0(weights, cnames[ind], collapse = " ")
    return(out)
  }

  tgp_interactions <- function(mat, cnames) {
    # overwrite main effects
    diag(mat) <- 0

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
        paste0(" - ", abs(m[interactions, i])),
        paste0(" + ", m[interactions, i])
      )

      # paste together weights (feature_1 * feature_2)
      out <- paste0(pretty_weights,
        "(",
        k[i], " * ",
        k[interactions],
        ")",
        sep = ""
      ) %>%
        paste0(., collapse = " ")
      return(out)
    }

    out <- sapply(seq_len(ncol(mat)),
      FUN = tgp_single_interaction,
      m = mat,
      k = cnames
    ) %>%
      do.call("paste0", .)
  }

  # simulate from copula
  sim_mat <- sim_copula(
    n_obs = n,
    sim_book = book,
    cor_int = cor_interval,
    cor_mat = cor_matrix
  ) %>%
    # coerce to tibble
    as_tibble(., .name_repair = "minimal") %>%
    # set the names of the simulation matrix
    purrr::set_names(book %>% pull(name))

  # fetch discrete variables
  discr_idx <- which(book$type == "discrete")

  # expand discrete variables (dummy matrix)
  expand_discrete <- function(mat, intercept, indices, book) {
    coerce_factor <- function(x) {
      factor(x, labels = paste0("__", as.character(1:length(unique(x)))))
    }

    discr_mat <- mat %>%
      select(indices) %>%
      mutate_all(., .funs = list(coerce_factor))

    dum_eqn <- paste("~", ifelse(intercept, 1, -1), "+ .")
    dummies <- model.matrix(as.formula(dum_eqn), data = discr_mat) %>%
      as_tibble() %>%
      select(-matches("Intercept"))

    out <- list()
    out$mat <- mat %>%
      select(-indices) %>%
      bind_cols(dummies, .)

    discr_book <- book %>%
      filter(type == "discrete") %>%
      ungroup()

    reps <- names(dummies) %>%
      gsub("(.*)__.*", "\\1", .) %>%
      table()

    new_discr_book <- lapply(1:nrow(discr_book),
      FUN = function(x, y, discr_book) {
        discr_book %>%
          slice(rep(x, y[x]))
      }, discr_book = discr_book,
      y = reps
    ) %>%
      bind_rows() %>%
      mutate(name = names(dummies))



    # change book
    out$new_book <- book %>%
      filter(type != "discrete") %>%
      bind_rows(new_discr_book, .)
    return(out)
  }

  if (length(discr_idx) > 0) {
    exp_discr <- expand_discrete(
      mat = sim_mat,
      intercept = object$intercept,
      indices = discr_idx,
      book = book
    )

    book <- exp_discr$new_book
    sim_mat <- exp_discr$mat
  }

  # save pre transformed numerical matrix
  trans_mat <- sim_mat

  # fetch all functions
  nlfuns <- book %>%
    pull(nlfun)

  # apply nonlinear functions
  trans_mat <- trans_mat %>%
    purrr::map2(.x = ., .y = nlfuns, .f = ~ .y(.x)) %>%
    as_tibble()

  # TODO: expand discrete features

  # manage interactions ----
  # interaction matrix raw (no interactions)
  # sample weights (from [-5;5])
  weights <- round(runif(ncol(trans_mat), -5, 5), 2)

  # adjust nlin weights
  nl_idx <- which(book$type == "nonlinear")
  weights[nl_idx] <- weights[nl_idx] * 1e-2

  # set weights of uninformative or noise to 0
  no_weight_idx <- book %>%
    pull(type) %>%
    stringr::str_detect("noise|random", .)

  weights[no_weight_idx] <- 0

  # create index for which interactions/weights are defined
  weight_idx <- which(!no_weight_idx)

  # build interaction matrix
  int_mat <- diag(weights)

  # sample interactions
  if (object$interactions) {
    int_mat[!no_weight_idx, !no_weight_idx] <-
      sample_interactions(int_mat[!no_weight_idx, !no_weight_idx])
  }

  # generate the equation for the target generating process
  # the main effects
  tgp_main <- tgp_main_effects(mat = int_mat, cnames = colnames(sim_mat))

  # the interaction effects
  tgp_interact <- tgp_interactions(mat = int_mat, cnames = colnames(sim_mat))

  # create target ----
  target <- trans_mat %>%
    as.matrix() %*%
    int_mat %*%
    rep(1, ncol(int_mat)) %>%
    purrr::as_vector(.)

  # add target to the interaction matrix
  int_mat <- Matrix::bdiag(int_mat, 1) %>%
    as.matrix()

  # add intercept ----
  if (object$intercept) {
    # simulate intercept
    # use a percentage of the maximum value of the target
    intercept <- max(abs(target)) * sample(seq(-2e-1, 2e-1, 11e-2), size = 1)
    # prepare the stringt for the target generating process (intercept)
    tgp_intercept <- paste0("y = ", round(intercept, 3))
    # add the intercept to the process
    target <- target + intercept

    # add intercept to the interaction matrix
    int_mat <- Matrix::bdiag(intercept, int_mat) %>%
      as.matrix()

    # add intercept to the book
    book_intercept <- tibble(
      type = "intercept",
      name = "intercept",
      nlfun = list(function(x) x),
      collinearity = NA,
      params = list(NULL) %>% purrr::set_names(""),
      family = NA_character_
    )

    book <- bind_rows(book, book_intercept)

    # add intercept to the simulation matrix
    sim_intercept <- tibble(intercept = rep(1, nrow(sim_mat)))
  } else {
    # prepare the stringt for the target generating process (no intercept)
    tgp_intercept <- "y = "

    # add intercept to the simulation matrix
    sim_intercept <- NULL
  }

  # add noise ----

  # optimize the noise within the simulation such that it fulfills the
  # user-specified adjusted r-squared representation
  optimize_r_squared <- function(w,
                                 error,
                                 yhat,
                                 n,
                                 p,
                                 r_sq_target) {

    # calculate regular r-squared
    calc_r_sq <- function(w, yhat, error) {
      y <- yhat + w * error
      1 - (sum((w * error)^2) / sum((y - mean(y))^2))
    }

    # calculate r-squared
    r_sq <- calc_r_sq(w = w, error = error, yhat = yhat)

    # calculate adjusted r-squared
    adj_r_sq <- 1 - ((1 - r_sq) * ((n - 1) / (n - p - 1)))

    # define loss function
    loss <- abs(adj_r_sq - r_sq_target)

    # return loss
    return(loss)
  }

  # optimize weights
  error_weights <- optim(
    par = runif(1),
    lower = -1e+3,
    upper = 1e+3,
    fn = optimize_r_squared,
    error = sim_mat %>% pull(e),
    yhat = target,
    n = length(target),
    p = length(weight_idx),
    r_sq_target = r_squared,
    method = "Brent"
  )
  # scale the error
  e_vec <- trans_mat %>%
    mutate(e = e * error_weights$par) %>%
    pull(e)

  # add the noise to the target generating process
  target <- target + e_vec

  # possibly link and cutoff functions
  possible_link <- purrr::possibly(object$task$link, otherwise = NA_real_)
  possible_cutoff <- purrr::possibly(object$task$cutoff, otherwise = NA_real_)

  # apply link and cutoff
  data <- bind_cols(sim_intercept, sim_mat, y = target) %>%
    # apply link function
    mutate(
      y = purrr::map_dbl(y, ~ possible_link(.x)),
      y = purrr::map_dbl(y, ~ possible_cutoff(.x))
    ) %>%
    purrr::when(
      object$task$type == "regression"
      ~ mutate(., y = y),
      ~ mutate(., y = as.integer(y))
    )

  # build tgp part for the error
  book_e <- book %>% filter(type == "noise")
  params <- book_e %>% pull(params)
  fam_name <- book_e %>% pull(family)
  name_e <- paste0(
    "e ~ ", fam_name, "(",
    params[[1]] %>%
      paste0(names(.),
        " = ",
        .,
        collapse = ", "
      ),
    ")"
  )
  tgp_error <- paste0("+ ", "(", as.character(round(error_weights$par, 2)), name_e, ")")

  # finish target generating process
  tgp <- paste(tgp_intercept, tgp_main, tgp_interact, tgp_error)

  # setting names
  colnames(int_mat) <- colnames(data)

  # overwrite noise with its weight
  diag(int_mat)[which(colnames(int_mat) == "e")] <- error_weights$par

  # create a formula object
  features <- colnames(data)
  # omit target, noise and intercept
  features <- features[-stringr::str_detect("y|intercept", features)]
  # handle nonlinear features
  features <- stringr::str_replace(features, "(^f.*)", "`\\1`")
  # fix intercept term
  features <- c(ifelse("intercept" %in% colnames(data), "-1", "1"), features)
  # build equation
  eq <- formula(paste0("y ~ ", paste0(features, collapse = " + ")))

  # add class
  out <- list(
    data = data,
    psi = int_mat %>% as.matrix(),
    task = object$task$type,
    eq = eq,
    interactions = object$interactions,
    book = bind_rows(book, tibble(
      type = "target",
      name = "y",
      nlfun = list(function(x) x),
      collinearity = NA,
      params = list(NULL) %>% purrr::set_names(""),
      family = NA_character_
    )),
    r_sq = r_squared,
    cor = cor_interval,
    tgp = tgp
  )

  class(out) <- "xy_sim"

  # return ----
  return(out)
}
