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
#' A function which simulates linear and nonlinear features and a corresponding
#' target. The composition of the target is highly customizable.
#' Furthermore, the polynomial degree as well as the functional shape of
#' nonlinearity can be specified by the user. Additionally coviarance structure
#' of the features can either be sampled by the function or specifically 
#' determined by the user.
#' 
#' @param n an integer specifying the number of observations
#' @param xvars a numeric vector specifying the number of linear and nonlinear
#'              features. For instance, \code{c(5, 10)} corresponds to
#'              five linear and ten non-linear features.
#' @param noisevars an integer determining the number of noise variables
#' @param nlfun a function transforming nonlinear variables
#' @param interactions a vector of integer specifying the interaction depth of
#'                    of regular features and autoregressive features if
#'                    applicable. For instance, \code{c(3, 2)} would translate to
#'                    using xvars interactions of at most of degree three and
#'                    autoregressive interactions at most of degree two.
#' @param sig a vector c(min, max) indicating the scale parameter to sample from
#' @param cor a vector c(min, max) determining correlation to sample from.
#' @param weights a vector c(min, max) specifying 
#'              the multiplication magnitude to sample from
#' @param cov.mat a covariance matrix for the linear and nonlinear simulation.
#'                Defaults to \code{NULL} which means the structure
#'                will be sampled from \code{cor}
#' @param stn a numeric value determining the signal to noise ratio
#' @param noise.coll a boolean determining noise collinearity with X
#' @param plot a boolean indicating whether true effects should be plotted
#' 
#' @return a list with the following entries
#' \item \code{data} - the simulated data.table
#' \item \code{dgp} - the data generated process as a string
#' \item \code{control} - a list matching the call
#' \item \code{plot} - a ggplot if applicable
#'
#' @examples
#' 
#' set.seed(1337)
#' sim_data <- Xy()$data
#' 
Xy <-       function(n = 1000, 
                     xvars = c(2, 2),
                     lags = NULL,
                     noisevars = 5,
                     nlfun = function(x) x^2,
                     interactions = c(1, 1),
                     sig = c(1,4), 
                     cor = c(0.1,0.3),
                     weights = c(-5,5),
                     cov.mat = NULL,
                     stn = 0.01,
                     noise.coll = FALSE,
                     plot = TRUE
                     ) {
  
  # dependencies
  library(data.table)
  library(ggplot2)
  
  # save input9
  input <- as.list(environment())
  
  # functions -----
  # extracts the name out of 'int.mat'
  ext.name <- function(i, x, var) {
    OUT <- paste0(paste0(round(x[x[, i] != 0, i], 2),
                         var[x[, i] != 0]),
                  collapse = " * ")
    return(OUT)
  }
  
  # adds interaction terms to the process
  add.interactions <- function(x, weights, interaction) {
    for (COL in seq_len(NCOL(x))) {
      # sample interaction
      sample.value <- sample(c(0, round(runif(
        interaction - 1,
        min(weights),
        max(weights)
      ), 2)),
      replace = FALSE,
      size = interaction - 1)
      
      # sample position
      sample.pos <- sample(seq_len(NCOL(x))[-COL],
                           replace = FALSE,
                           size = interaction - 1)
      # overwrite interaction matrix
      x[sample.pos, COL] <- sample.value
    }
    return(x)
  }
  
  # setup the correct name
  set.var.name <- function(i, x) {
    if (x[i] == 0) return(NULL)
    return(paste0(names(x)[i], "_", seq_len(x[i])))
  }
  
  # issue warnings ----
  # n
  if(!is.numeric(n)) {
    stop(paste0(sQuote("n"), " has to be a numeric value."))
  }
  
  # xvars character
  if(!is.numeric(xvars)) {
    stop(paste0(sQuote("xvars"), " has to be a numeric."))
  }
  
  # insufficient length
  if(length(xvars) != 2) {
    if (length(xvars) > 2) {
    xvars <- xvars[1:2]
    } else {
    xvars <- c(xvars, 0)
    }
    warning(paste0(sQuote("xvars"), " has to be of length two. Following settings ",
                  "are used: Linear (", xvars[1], ") and nonlinear (", xvars[2], ")"))
  }
  
  # noisevars
  if(!is.numeric(noisevars)) {
    stop(paste0(sQuote("noisevars"), " has to be a numeric value."))
  }
  
  # interaction
  if(!is.numeric(interactions)) {
    stop(paste0(sQuote("interactions"), " has to be a numeric vector"))
  }
  
  # signal to noise
  if(!is.numeric(stn)) {
    stop(paste0(sQuote("sig.e"), " has to be a numeric value."))
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
  
  # plot
  if(!is.logical(plot)) {
    stop(paste0(sQuote("plot"), " has to be a boolean."))
  }
  
  # noise.coll
  if(!is.logical(noise.coll)) {
    stop(paste0(sQuote("noise.coll"), " has to be a boolean."))
  }
  
  # preliminaries ----

  # features ----
  # handle noise collinearity
  if (noise.coll) {
    # dictionary
    mapping <- c("NLIN" = xvars[2], 
                 "LIN" = xvars[1],
                 "NOISE" = noisevars)
    # handle wrong dimensionality due to noise variables
    n.coll <- noisevars
  } else {
    # dictionary
    mapping <- c("NLIN" = xvars[2], "LIN" = xvars[1])
    # handle wrong dimensionality due to noise variables
    n.coll <- 0
  }
 
  # total number of variables
  vars <-  sum(mapping)
 
  # issue warning cov.mat ----
  if(!is.null(cov.mat)) {
    
    cov.mat <- tryCatch({as.matrix(cov.mat)},
                        error = function(e) return(NA))
    
    if(is.na(cov.mat)) {
      stop(paste0("Tried to coerce", sQuote("cov.mat"),
                  " to a matrix, but could not succeed."))
    }
    
    if(NCOL(cov.mat) != vars) {
      stop(paste0("The user-specified covariance matrix",
                  " has insufficient columns: ",
                  NCOL(cov.mat), " for ",
                  vars, " variables. Reconsider ", 
                  sQuote("cov.mat"), "."))
    }
  }
  
  # handle covariance matrix
  if (is.null(cov.mat)) {
    # covariance
    cov.mat <- matrix(runif(vars^2, min(cor), max(cor)),
                      nrow = vars,
                      ncol = vars)
    # variance
    diag(cov.mat) <- runif(NCOL(cov.mat),
                           min(sig),
                           max(sig))
  }
  
  # sample features
  FEATURES <- matrix(rnorm(n = n * vars, 
                            mean = 0,
                            sd = runif(1, min(sig), max(sig))),
                      ncol = vars, nrow = n) %*%  chol(cov.mat)
  
  # copy features
  VARS <- FEATURES[, 1:(vars-n.coll)]
  
  # set features as data.table
  FEATURES <- data.table(FEATURES)
  
  # transform nonlinear part
  if (xvars[2] > 0) {
    VARS[, 1:xvars[2]] <- apply(VARS[, 1:xvars[2]],
                                MARGIN = 2,
                                FUN = nlfun)
  }
  

  # set names
  names(FEATURES) <- unlist(sapply(seq_len(length(mapping)),
                                   set.var.name, x = mapping))
  
  # noise ----
  if (noisevars > 0 && !noise.coll) {
    S <- matrix(runif(noisevars ^ 2, min(cor), max(cor)),
                nrow = noisevars,
                ncol = noisevars)
    
    # fix diagonal
    diag(S) <- runif(NCOL(S), min(sig), max(sig))
    
    noise.mat <-  data.table(matrix(rnorm(n * noisevars),
                                    ncol = noisevars, nrow = n) %*%  chol(S))
    names(noise.mat) <- paste0("NOISE_", seq_len(noisevars))
    
    FEATURES <- cbind(FEATURES, noise.mat)
  }
  
  # manage interactions ----
  # interaction matrix raw (no interactions)
  int.mat <- diag(round(runif(vars-n.coll, min(weights), max(weights)), 2))
  
  # sample interactions
  if (interactions[1] > 1) {
    int.mat <- add.interactions(int.mat, weights, interactions[1])
  }
  
  # extract the target generating process
  int.raw <- sapply(seq_len(NCOL(int.mat)-n.coll),
                    FUN = ext.name,
                    x = int.mat,
                    var = names(FEATURES)[seq_len(NCOL(int.mat))])
  
  # fix negative terms
  int.raw[grep("-", int.raw)] <- gsub("(.*)", "\\(\\1\\)", int.raw[grep("-", int.raw)])
  
  # describe y
  dgp <- paste0("y = ", paste0(int.raw, collapse = " + "))
  
  # create target ----
  VARS <- apply(VARS, scale, center = TRUE, scale = TRUE, MARGIN = 2)

  target <- VARS %*%
              int.mat %*%
                  rep(1, NCOL(int.mat))
  
  # add to features
  FEATURES[, y := target]
  #EATURES[, names(FEATURES) := lapply(FEATURES, scale)]
  
  # fix - terms
  dgp <- gsub(" \\+ \\(-", " - ", dgp)
  
  # fix brackets
  dgp <- gsub("\\)|\\(", "", dgp)
  
  # add error
  dgp <- paste0(dgp, " + e ~ N(0,",round(stn, 2),")")
  
  # plot true effects
  if (plot) {
    
    plot.dat <- FEATURES[, -grep("NOISE", names(FEATURES)), with = FALSE]
    melted.dat <- melt(plot.dat, "y")
    melted.dat <- melted.dat[order(value),  .SD, by = variable]
    plot <- ggplot(melted.dat, aes(x = value, y = y)) + 
                geom_point(colour = "#13235B") + 
                facet_wrap( ~ variable, scale = "free") + 
                theme_minimal(base_size = 14) +
                xlab("") +
                ggtitle("True effects X vs y") +
                geom_smooth(method = "loess", colour = "#00C792")
  }
  
  # return ----
  return(list(data = na.omit(FEATURES), dgp = dgp, control = input,  plot = plot))
}
  