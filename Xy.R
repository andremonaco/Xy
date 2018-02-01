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
#' target. The influence on the target is highly customizable, i.e. interaction
#' depth can be specified. Furthermore, the nonlinearity can be specified via a
#' transformation function. Additionally coviarance structure of the features
#' can either be sampled by the function or specifically determined by the user.
#' 
#' @param n an integer specifying the number of observations
#' @param linvars an integer specifying the number of linear features
#' @param nlinvars an integer determining the number of nonlinear features
#' @param noisevars an integer determining the number of noise variables
#' @param nlfun a function transforming nonlinear variables
#' @param interaction an integer specifying the interaction depth
#' @param sig a vector c(min, max) indicating the scale parameter to sample from
#' @param cor a vector c(min, max) determining correlation to sample from
#' @param m.mag a vector c(min, max) specifying 
#'              the multiplication magnitud to sample from
#' @param cov.mat a covariance matrix for the linear and nonlinear simulation.
#'                Defaults to \code{NULL} which means the structure
#'                will be sampled from \code{cor}
#' @param sig.e  a numeric indicating the scale parameter of the target noise
#' @param plot a logical determining whether true effects should be plotted.
#'             This option is unavailable for
#'             \code{linvars} + \code{nlinvars} > 20
#'
#' @return a list with the following entries
#' \item \code{data} - the simulated data.table
#' \item \code{dgp} - the data generated process as a string
#' \item \code{plot} - a ggplot if aplicable
#' @export
#'
#' @examples
#' 
#' sim_data <- Xy()$data
#' 
Xy <-       function(n = 1000, 
                     linvars = 5,  
                     nlinvars = 10, 
                     noisevars = 5, 
                     nlfun = function(x) x^2,
                     interaction = 2,
                     sig = c(1,5), 
                     cor = c(0.1,0.3),
                     m.mag = c(-5,5),
                     cov.mat = NULL,
                     sig.e = 4,
                     plot = TRUE
                     ) {
  
  library(data.table)
  library(ggplot2)
  
  # funs -----
  # extracts the name out of 'int.mat'
  ext.name <- function(i, x, var) {
    
    OUT <- paste0(paste0(x[x[, i] != 0, i], var[x[, i] != 0]), collapse = " * ")
    
  }
  
  # adds interaction terms to the process
  add.interactions <- function(x, m.mag, interaction) {
    for (COL in seq_len(NCOL(x))) {
      # sample interaction
      sample.value <- sample(c(0, round(runif(
        interaction - 1,
        min(m.mag),
        max(m.mag)
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
  
  # features ----
  # dictionary
  mapping <- c("NLIN" = nlinvars, "LIN" = linvars)
  
  # total number of variables
  vars <-  linvars + nlinvars
  
  # invoke linear features
  FEATURES <-
    VARS <-
    matrix(rep(seq(-1, 1, length.out = n), vars),
           nrow = n,
           ncol = vars)
  
  # handle covariance matrix
  if (is.null(cov.mat)) {
    # covariance
    cov.mat <- matrix(runif(vars ^ 2, min(cor), max(cor)),
                      nrow = vars,
                      ncol = vars)
    # variance
    diag(cov.mat) <- runif(NCOL(cov.mat),
                           min(sig),
                           max(sig))
  }
  
  # sampled noise
  cor.noise <- matrix(rnorm(n * vars, min(sig), max(sig)),
                      ncol = vars, nrow = n) %*%  chol(cov.mat)
  
  # add noise
  FEATURES <- VARS <- FEATURES + cor.noise
  
  # set features as data.table
  FEATURES <- data.table(FEATURES)
  
  # transform nonlinear part
  if (nlinvars > 0) {
    VARS[, 1:nlinvars] <- sapply(VARS[, 1:nlinvars],
                                 FUN = nlfun)
  }
  
  # set names
  names(FEATURES) <- unlist(sapply(seq_len(length(mapping)),
                                   set.var.name, x = mapping))
  
  # noise ----
  if (noisevars > 0) {
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
  int.mat <- diag(round(runif(vars, min(m.mag), max(m.mag)), 2))
  
  # sample interactions
  if (interaction > 1) {
    int.mat <- add.interactions(int.mat, m.mag, interaction)
  }
  
  # extract the target generating process
  int.raw <- sapply(seq_len(NCOL(int.mat)),
                    FUN = ext.name,
                    x = int.mat,
                    var = names(FEATURES)[seq_len(NCOL(int.mat))])
  
  # fix negative terms
  int.raw[grep("-", int.raw)] <- gsub("(.*)", "\\(\\1\\)", int.raw[grep("-", int.raw)])
  
  # describe y
  dgp <- paste0("y = ", paste0(int.raw, collapse = " + "))
  
  # fix - terms
  dgp <- gsub(" \\+ \\(-", " - ", dgp)
  
  # fix brackets
  dgp <- gsub("\\)|\\(", "", dgp)
  
  # add error
  dgp <- paste0(dgp, " + e ~ N(0,",stn,")")
  
  # create target ----
  VARS <- apply(VARS, scale, center = TRUE, scale = TRUE, MARGIN = 2)
  target <-     VARS %*%
                  int.mat %*%
                    rep(1, NCOL(int.mat)) +
                        rnorm(n, 0, stn)
  
  # add to features
  FEATURES[, y := target]
  FEATURES[, names(FEATURES) := lapply(FEATURES, 
                                       scale,
                                       center = TRUE, 
                                       scale = TRUE)]
  
  # plot true effects
  if (plot && vars < 20) {
    
    plot.dat <- FEATURES[, c(1:vars, NCOL(FEATURES)), with = FALSE]
    names(plot.dat) <- c(names(FEATURES)[seq_len(NCOL(FEATURES)-noisevars-1)], "y")
    melted.dat <- melt(plot.dat, "y")
    melted.dat <- melted.dat[order(value),  .SD, by = variable]
    p <- ggplot(melted.dat, aes(x = value, y = y)) + 
                geom_point(colour = "#13235B") + 
                facet_wrap( ~ variable) + 
                theme_minimal(base_size = 14) +
                xlab("") +
                ggtitle("True effects X vs y") +
                geom_smooth(method = "loess", colour = "#00C792")
      
  # handle too many variable + no plot wanted  
  } else {
    p <- NULL
  }
  
  # return ----
  return(list(data = FEATURES, dgp = dgp, plot = p))
}
  