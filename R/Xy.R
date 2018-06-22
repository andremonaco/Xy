#                        ░░░░░░░█▐▓▓░████▄▄▄█▀▄▓▓▓▌█ very useful
#                        ░░░░░▄█▌▀▄▓▓▄▄▄▄▀▀▀▄▓▓▓▓▓▌█ 
#                        ░░░▄█▀▀▄▓█▓▓▓▓▓▓▓▓▓▓▓▓▀░▓▌█ 
#                        ░░█▀▄▓▓▓███▓▓▓███▓▓▓▄░░▄▓▐█▌ such flexibility
#                        ░█▌▓▓▓▀▀▓▓▓▓███▓▓▓▓▓▓▓▄▀▓▓▐█ 
#                        ▐█▐██▐░▄▓▓▓▓▓▀▄░▀▓▓▓▓▓▓▓▓▓▌█▌ 
#                        █▌███▓▓▓▓▓▓▓▓▐░░▄▓▓███▓▓▓▄▀▐█ much simulate
#                        █▐█▓▀░░▀▓▓▓▓▓▓▓▓▓██████▓▓▓▓▐█ 
#                        ▌▓▄▌▀░▀░▐▀█▄▓▓██████████▓▓▓▌█▌ 
#                        ▌▓▓▓▄▄▀▀▓▓▓▀▓▓▓▓▓▓▓▓█▓█▓█▓▓▌█▌ 
#                        █▐▓▓▓▓▓▓▄▄▄▓▓▓▓▓▓█▓█▓█▓█▓▓▓▐█ wow
#
#
#' Xy
#'
#' A function which simulates linear and nonlinear X and a corresponding
#' target. The composition of the target is highly customizable.
#' Furthermore, the polynomial degree as well as the functional shape of
#' nonlinearity can be specified by the user. Additionally coviarance structure
#' of the X can either be sampled by the function or specifically 
#' determined by the user.
#' 
#' @param n an integer specifying the number of observations
#' @param numvars a numeric vector specifying the number of linear and nonlinear
#'                X For instance, \code{c(5, 10)} corresponds to
#'                five linear and ten non-linear X.
#' @param catvars a numeric vector determining the amount of categorical predictors.
#'                With this vector you can choose how many categorical predictors should
#'                enter the equation and secondly the respective amount of categories.
#'                For instance, \code{catvars = c(2,5)} would correspond to creating
#'                two categorical variables with five categories.
#' @param noisevars an integer determining the number of noise variables
#' @param nlfun a function transforming nonlinear variables
#' @param interactions a vector of integer specifying the interaction depth of
#'                    of regular X and autoregressive X if
#'                    applicable.
#' @param family a Xy family object as created with \code{\link{Xy_family}}
#' @param sig a vector c(min, max) indicating the scale parameter to sample from
#' @param cor a vector c(min, max) determining correlation to sample from.
#' @param weights a vector c(min, max) specifying 
#'              the multiplication magnitude to sample from
#' @param sigma a covariance matrix for the linear and nonlinear simulation.
#'                Defaults to \code{NULL} which means the structure
#'                will be sampled from \code{cor}
#' @param stn a numeric value determining the signal to noise ratio. Should be within the interval [0, 1].
#'            Higher values lead to more signal and less noise.
#' @param noise.coll a boolean determining noise collinearity with X
#' @param intercept a boolean indicating whether an intercept should enter the model
#' 
#' @import data.table ggplot2 gridExtra Matrix
#' @importFrom stats model.matrix na.omit quantile rnorm runif sd formula
#' @importFrom Matrix .bdiag
#' 
#' @exportClass Xy_sim
#' 
#' @author Andre Bleier (\email{andre.bleier@@statworx.com})
#' 
#' @return a list with the following entries
#' \itemize{
#' \item \code{data} - the simulated data.table
#' \item \code{dgp} - the data generating process as a string
#' \item \code{psi} - psi is a transformation matrix which transforms the raw data
#'                    (stored in $data) to the true effects. However, you have to
#'                    apply the nonlinear functions upfront. If you want to transform
#'                    the data, please use \code{\link{transform}}
#' \item \code{control} - a list matching the call
#' }
#' @export
#' 
#' @examples
#' 
#' set.seed(1337)
#' my_simulation <- Xy()
#' 
Xy <-       function(n = 1000, 
                     numvars = c(2, 2),
                     catvars = c(1, 2),
                     noisevars = 5,
                     family = Xy_family(),
                     nlfun = function(x) x^2,
                     interactions = 1,
                     sig = c(1,10), 
                     cor = c(0.1,0.3),
                     weights = c(5,10),
                     sigma = NULL,
                     stn = 0.95,
                     noise.coll = FALSE,
                     intercept = TRUE
                     ) {
  
  # save input
  input <- as.list(environment())
  
  # functions -----
  # extracts the name out of 'INT'
  ext_name <- function(i, x, var) {
    OUT <- paste0(paste0(round(x[x[, i] != 0, i], 2),
                         var[x[, i] != 0]),
                  collapse = " * ")
    return(OUT)
  }
  
  # adds interaction terms to the process
  add_interactions <- function(x, weights, interaction) {
    for (c in seq_len(NCOL(x))) {
      # sample interaction
      sample.value <- sample(c(0, round(runif(
        interaction - 1,
        min(weights),
        max(weights)
      ), 2)),
      replace = FALSE,
      size = interaction - 1)
      
      # sample position
      sample.pos <- sample(seq_len(NCOL(x))[-c],
                           replace = FALSE,
                           size = interaction - 1)
      # overwrite interaction matrix
      x[sample.pos, c] <- sample.value
    }
    return(x)
  }
  
  # setup the correct name
  set_var_name <- function(x, full) {
    OUT <- c()
    for (i in seq_along(x)) {
    if (x[i] == 0) next
     OUT <- c(OUT, paste0(names(x)[i], "_", formatC(seq_len(x[[i]]),
                                                 width = nchar(max(do.call("c", full))),
                                                 flag = "0")))
    }
    return(OUT)
  }
  
  # issue warnings ----
  # n
  if(!is.numeric(n) | length(n) != 1) {
    stop(paste0(sQuote("n"), " has to be a numeric value."))
  }
  
  # numvars character
  if(!is.numeric(numvars)) {
    stop(paste0(sQuote("numvars"), " has to be a numeric vector."))
  }
  
  # insufficient length
  if(length(numvars) != 2) {
    if (length(numvars) > 2) {
    numvars <- numvars[1:2]
    } else {
    numvars <- c(numvars, 0)
    }
    warning(paste0(sQuote("numvars"), " has to be of length two. Following settings ",
                  "are used: Linear (", numvars[1], ") and nonlinear (", numvars[2], ")"))
  }
  
  # noisevars
  if(!is.numeric(noisevars) | length(noisevars) != 1) {
    stop(paste0(sQuote("noisevars"), " has to be a numeric value."))
  }
  
  # interaction
  if(!is.numeric(interactions) | length(interactions) != 1) {
    stop(paste0(sQuote("interactions"), " has to be a numeric value."))
  }
  
  # categorical variables
  if(length(catvars) != 2 | 
     !is.numeric(catvars)) {
    stop(paste0(sQuote("catvars"), "has to be a vector of length two which specifies",
                " first the number of categorical features and second their", 
                " respective number of classes."))
  }
  
  # signal to noise
  if(!is.numeric(stn) | 
     any(stn > 1) | 
     any(stn < 0)) {
    stop(paste0(sQuote("stn"), "has to be a vector specifying a numeric range",
                " (in [0,1]) or a single numeric."))
  }
  
  # nlfun
  if(!is.function(nlfun)) {
    stop(paste0(sQuote("nlfun"), " has to be a function"))
  }
  
  # sig
  if(!length(sig) %in% c(1,2)) {
    stop(paste0(sQuote("sig"), " has to be either a vector of numeric values",
                               " specifying variance boundries or a numeric value."))
  }
  
  # weights
  if(!length(weights) %in% c(1,2) | 
     !is.numeric(weights)) {
    stop(paste0(sQuote("weights"), "has to be a vector specifying a numeric range",
                               " or a single numeric."))
  }
  
  # weights
  if(!length(cor) %in% c(1,2) | 
     !is.numeric(cor) | 
     any(cor > 1) | 
     any(cor < 0)) {
    stop(paste0(sQuote("cor"), "has to be a vector specifying a numeric range",
                               " (in [0,1]) or a single numeric."))
  }
  
  # noise.coll
  if(!is.logical(noise.coll)) {
    stop(paste0(sQuote("noise.coll"), " has to be a boolean."))
  }
  
  # preliminaries ----

  # X_TRANS ----
  # handle noise collinearity
  if (noise.coll) {
    # dictionary
    mapping <- list("NLIN" = numvars[2], 
                    "LIN" = numvars[1],
                    "NOISE" = noisevars)
    
    # handle wrong dimensionality due to noise variables
    n.coll <- noisevars
  } else {
    # dictionary
    mapping <- list("NLIN" = numvars[2], 
                    "LIN" = numvars[1])
    # handle wrong dimensionality due to noise variables
    n.coll <- 0
  }
 
  # total number of variables
  vars <-  Reduce("+", mapping)
 
  # issue warning sigma ----
  if(!is.null(sigma)) {
    
    SIGMA <- tryCatch({as.matrix(sigma)},
                        error = function(e) return(NA))
    
    if(is.na(SIGMA)) {
      stop(paste0("Tried to coerce", sQuote("sigma"),
                  " to a matrix, but could not succeed."))
    }
    
    if(NCOL(SIGMA) != vars) {
      stop(paste0("The user-specified covariance matrix",
                  " has insufficient columns: ",
                  NCOL(SIGMA), " for ",
                  vars, " variables. Reconsider ", 
                  sQuote("sigma"), "."))
    }
  }
  
  # handle covariance matrix
  if (is.null(sigma)) {
    # covariance
    SIGMA <- matrix(runif(vars^2, min(cor), max(cor)),
                      nrow = vars,
                      ncol = vars)
    # variance
    diag(SIGMA) <- runif(NCOL(SIGMA),
                           min(sig),
                           max(sig))
  }
  
  # sample and rotate X
  X <- matrix(rnorm(n = n * vars, 
                    mean = 0,
                    sd = runif(1, min(sig), max(sig))),
              ncol = vars, 
              nrow = n) %*% chol(SIGMA)

  # create dummmy X_TRANS
  if (catvars[1] > 0) {

  X_DUM_RAW <- do.call("data.frame", lapply(rep(list(seq_len(catvars[2])), 
                                          catvars[1]), 
                                          FUN = sample, 
                                          replace = TRUE,
                                          size = nrow(X), 
                                          prob = runif(catvars[2], 0, 1)))
  
  # save names
  names(X_DUM_RAW) <- paste0("DUMMY_", 1:ncol(X_DUM_RAW))
  
  # factorize
  X_DUM <- data.frame(sapply(X_DUM_RAW, factor))
  colnames(X_DUM) <- paste0("DUMMY_", formatC(seq_len(catvars[1]),
                                                max(nchar(c(noisevars,
                                                            do.call("c", mapping), 
                                                            catvars[1])))-1,
                                             flag = "0"))
  
  # bind model matrix
  X_DUM <- do.call("data.frame", lapply(seq_along(X_DUM), FUN = function(i,x) {
                                          the_name <- names(x)[i]
                                          OUT <- model.matrix(~ . -1, data = data.frame(x[, i]))
                                          colnames(OUT) <- paste0(the_name, "__", 1:ncol(OUT))
                                          return(OUT)
                                          }, x = X_DUM))
  # draw weights
  DW <- diag(round(runif(ncol(X_DUM), min(weights), max(weights)), 2))
  }

  # set X_TRANS as data.table
  X_TRANS <- X <- data.table(X)
  
  # set names
  names(X_TRANS) <- names(X) <- set_var_name(mapping, c(mapping, noisevars))
  
  # transform nonlinear part
  if (numvars[2] > 0) {
    nlins <- grep("NLIN", names(X_TRANS), value = TRUE)
    X_TRANS[, c(nlins) := lapply(.SD, nlfun), .SDcols = nlins]
  }
  
  # manage interactions ----
  # interaction matrix raw (no interactions)
  INT <- diag(round(runif(vars-n.coll, min(weights), max(weights)), 2))
  
  # sample interactions
  if (interactions[1] > 1) {
    INT <- add_interactions(INT, weights, interactions[1])
  }
  
  # extract the target generating process
  int_raw <- sapply(seq_len(NCOL(INT)),
                    FUN = ext_name,
                    x = INT,
                    var = names(X_TRANS)[seq_len(NCOL(INT))])
  
  # fix negative terms
  int_raw[grep("-", int_raw)] <- gsub("(.*)", "\\(\\1\\)", int_raw[grep("-", int_raw)])
  # create target ----
  X_TRANS[, c(names(X_TRANS)) := lapply(.SD, scale,
                                      center = TRUE, 
                                      scale = TRUE), .SDcols = names(X_TRANS)]

  target <- as.matrix(X_TRANS[, c(!grepl("NOISE", names(X_TRANS))), with = FALSE]) %*%
                      INT %*%
                      rep(1, NCOL(INT))
  
  # add dummy effects
  if (catvars[1] > 0) {
    ref_class <- !grepl("*__1", names(X_DUM))
    
    target <- target + as.matrix(X_DUM[, ref_class]) %*% 
                                          DW[ref_class, ref_class] %*% 
                                                    rep(1, sum(ref_class))  
    
    # set reference classes to zero
    DW[!ref_class, !ref_class] <- 0
    # create effect description
    dw_raw <- paste0(diag(DW)[ref_class], names(X_DUM)[ref_class])
    # fix negative terms
    dw_raw[grep("-", dw_raw)] <- gsub("(.*)", "\\(\\1\\)", dw_raw[grep("-", dw_raw)])
    X <- data.table(cbind(X, X_DUM))
  } else {
    dw_raw <- NULL
  }
  
  # noise ----
  if (noisevars > 0 && !noise.coll) {
    S <- matrix(runif(noisevars ^ 2, min(cor), max(cor)),
                nrow = noisevars,
                ncol = noisevars)
    
    # fix diagonal
    diag(S) <- runif(NCOL(S), min(sig), max(sig))
    
    E <-  data.table(matrix(rnorm(n * noisevars),
                            ncol = noisevars, nrow = n) %*%  chol(S))
    
    names(E) <- paste0("NOISE_",
                       formatC(seq_len(noisevars), 
                               width = nchar(max(do.call("c", mapping), noisevars)),
                               flag = "0"))
    
    X <- cbind(X, E)
  }
  
  # add intercept
  if (intercept) {
  i_cept <-  runif(1, quantile(target, 0.25), quantile(target, 0.75))
  i_cept_paste <- paste0("y = ", round(i_cept, 3), " + ")
  target <- target + i_cept
  } else {
  i_cept_paste <- "y = "
  }
  
  # add noise
  noise <- sd(target)*(1-stn)
  target <- target + rnorm(n, 0, noise)
  
  # add to X_TRANS
  X_TRANS[, y := target]
  X[, y := target]
  
  # transform target according to family
  X[, y := family$link(y)]
  X[, y := family$cutoff(y)]

  # describe y
  dgp <- paste0(i_cept_paste, paste0(c(int_raw, dw_raw), collapse = " + "))
  
  # fix - terms
  dgp <- gsub(" \\+ \\(-", " - ", dgp)
  
  # fix brackets
  dgp <- gsub("\\)|\\(", "", dgp)
  
  # add error
  dgp <- paste0(dgp, " + e ~ N(0,", round(noise, 2),")")
  
  # create the transformation matrix
  psi <- list()
  # add intercept
  if (intercept) {
    psi[[1]] <- i_cept
  } 
  # linear and nonlinear variabels
  psi[[2]] <- INT
  # catigorical variables
  if (catvars[1] > 0) {
  psi[[3]] <- DW
  }
  # noise variables
  if (noisevars > 0) {
  psi[[4]] <- diag(noisevars)
  }
  # target
  psi[[5]] <- 1
  
  # include intercept
  if (intercept) {
    X <- cbind(data.table("(Intercept)" = 1), X)
  }

  # create the block diagonal transformation matrix
  psi <- Matrix::.bdiag(psi[!sapply(psi, is.null)])
  
  # setting names
  colnames(psi) <- names(X)
  
  # add class
  OUT <- list(data = na.omit(X), 
              psi = psi,
              family = family,
              dgp = dgp,
              control = input)
  
  class(OUT) <- "Xy_sim"
  
  # return ----
  return(OUT)
}
  