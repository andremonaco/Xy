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
#' @param task a Xy task object as created with \code{\link{Xy_task}}
#' @param sig a vector c(min, max) indicating the scale parameter to sample from
#' @param cor a vector c(min, max) determining correlation to sample from.
#' @param weights a vector c(min, max) specifying 
#'              the multiplication magnitude to sample from
#' @param cormat a correlation matrix for the linear and nonlinear simulation.
#'                Defaults to \code{NULL} which means the structure
#'                will be sampled from \code{cor}
#' @param stn an integer value determining the signal to noise ratio.
#'            Higher values lead to more signal and less noise.
#' @param noise_coll a boolean determining noise collinearity with X
#' @param intercept a boolean indicating whether an intercept should enter the model
#' 
#' @import data.table ggplot2 Matrix
#' @importFrom stats model.matrix na.omit quantile rnorm 
#'                   runif sd formula var median mad reorder
#' @importFrom Matrix .bdiag
#' @importFrom mvtnorm rmvnorm
#' 
#' @exportClass Xy_sim
#' 
#' @author Andre Bleier (\email{andre.bleier@@statworx.com})
#' 
#' @return a list with the following entries
#' \itemize{
#' \item \code{data} - the simulated data.table
#' \item \code{tgp} - the target generating process as a string
#' \item \code{eq} - a formula object with all variables
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
                     task = Xy_task(),
                     nlfun = function(x) x^2,
                     interactions = 1,
                     sig = c(1,1), 
                     cor = c(-.5, .5),
                     weights = c(5,10),
                     cormat = NULL,
                     stn = 4,
                     noise_coll = FALSE,
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
        min(weights)*0.1,
        max(weights)*0.1
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
  if (length(catvars)==2 &
      catvars[1] == 0) {
    catvars <- c(0,0)
  }
  
  if(length(catvars) != 2 | 
     !is.numeric(catvars)) {
    if (is.numeric(catvars) && catvars == 0) {
      catvars <- c(0,0)
    } else {
      stop(paste0(sQuote("catvars"), " has to be a vector of length two which specifies",
                  " first the number of categorical features and second their", 
                  " respective number of classes."))
    }
  }
  
  if (catvars[2]==1) {
    stop(paste0(sQuote("catvars"), " has to be a vector of length two which specifies",
                " first the number of categorical features (one or more) and second",
                " the number of classes (more than one)."))
  }
  
  # signal to noise
  if(!is.numeric(stn) | 
     length(stn) != 1 ||
     stn <= 0) {
    stop(paste0(sQuote("stn"), "has to be a positive numeric value"))
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
     any(cor < -1)) {
    stop(paste0(sQuote("cor"), "has to be a vector specifying a numeric range",
                " (in [-1,1]) or a single numeric."))
  }
  
  # interactions
  if (interactions >= sum(numvars)) {
    interactions <- sum(numvars)
    warning(paste0("Reduced the interaciton depth to ", interactions))
  }
  
  # noise_coll
  if(!is.logical(noise_coll)) {
    stop(paste0(sQuote("noise_coll"), " has to be a boolean."))
  }
  
  # preliminaries ----
  
  # X_TRANS ----
  # dictionary
  contvars <- list("NLIN" = numvars[2], 
                  "LIN" = numvars[1],
                  "NOISE" = noisevars)
  
  # function to create continuous variables
  cont_sim <- function(contvars, noise_coll, cor, sig, cormat) {
    
    # early exit: no continuous variables to simulate
    if (Reduce("+", contvars) == 0) 
      return(NULL) 
    
    noisevars <- contvars[[3]]
    
    # handle noise collinearity
    if (noise_coll) {
      sub_noise <- 0
      # handle wrong dimensionality due to noise variables
    } else {
      sub_noise <- noisevars
    }
    
    # total number of variables
    vars <-  Reduce("+", contvars)
    
    # handle correlation matrix ----
    if(!is.null(cormat)) {
      
      CORR <- tryCatch({as.matrix(cormat)},
                       error = function(e) return(NA))
      
      if(is.na(CORR)) {
        stop(paste0("Tried to coerce", sQuote("cormat"),
                    " to a matrix, but could not succeed."))
      }
      
      if(NCOL(CORR) != vars) {
        stop(paste0("The user-specified correlation matrix",
                    " has insufficient columns: ",
                    NCOL(cormat), " for ",
                    vars, " variables. Reconsider ", 
                    sQuote("cormat"), "."))
      }
      # handle correlation matrix
    } else {
      
      CORR <- matrix(runif((vars-sub_noise)^2, min(cor), max(cor)),
                     nrow = vars-sub_noise,
                     ncol = vars-sub_noise)
    }
    
    # force correlation matrix to be symmetric
    CORR <- Matrix::forceSymmetric(CORR, uplo = "L")
    
    # handle no noise collinearity
    if (!noise_coll) {
      CORRE <- matrix(runif(noisevars^2, min(cor), max(cor)),
                      nrow = noisevars,
                      ncol = noisevars)
      CORRE <- Matrix::forceSymmetric(CORRE, uplo = "L")
      CORR <- bdiag(CORR,CORRE)
    }
    
    # variance
    diag(CORR) <- 1
    
    # sample standard deviations
    sds <- runif(ncol(CORR), min(sig), max(sig))
    
    # create covariance matrix
    SIGMA <- diag(sds) %*% CORR %*% diag(sds)
    
    # sample X
    MAT <- mvtnorm::rmvnorm(n = n, mean = rep(0, ncol(SIGMA)), 
                            sigma = as.matrix(SIGMA),
                            method = "svd")
    
    numvars_ind <- seq_len(contvars[[1]]+contvars[[2]])
    
    # output container
    out <- list()
    if (contvars[[3]]>0) {
      out$X <- MAT[, numvars_ind]
      e_ind <- suppressWarnings(max(max(numvars_ind),0)+1)
      out$E <- MAT[, e_ind:ncol(MAT)]
    } else {
      out$X <- MAT
      out$E <- matrix(nrow=nrow(MAT))[,0]
    } 
    
    return(out)
  }
  
  xe <- cont_sim(contvars=contvars,
                 noise_coll=noise_coll,
                 cor=cor,
                 sig=sig,
                 cormat=cormat)
  
  # set X_TRANS as data.table
  X <- xe$X
  E <- xe$E
  X_TRANS <- copy(X)
  
  # set names
  colnames(X_TRANS) <- colnames(X) <- set_var_name(contvars[1:2], contvars)
  colnames(E) <- enames <- set_var_name(contvars[3], contvars)
  
  # transform nonlinear part
  if (numvars[2] > 0) {
    nlins <- grepl("NLIN", colnames(X_TRANS))
    X_TRANS[, nlins] <- sapply(X_TRANS[, nlins], nlfun)
  }
  
  # create dummmy X_DUM
  discr_sim <- function(n, catvars, contvars, noisevars) {
    # return empty matrix if no catvars are desired
    if (catvars[1]==0) 
      return(matrix(nrow=n)[,0])
    
    X_DUM_RAW <- do.call("data.frame", lapply(rep(list(seq_len(catvars[2])), 
                                                  catvars[1]), 
                                              FUN = sample, 
                                              replace = TRUE,
                                              size = n, 
                                              prob = runif(catvars[2], 0, 1)))
    
    # save names
    colnames(X_DUM_RAW) <- paste0("DUMMY_", 1:ncol(X_DUM_RAW))
    
    # factorize
    X_DUM <- data.frame(sapply(X_DUM_RAW, factor))
    colnames(X_DUM) <- paste0("DUMMY_", formatC(seq_len(catvars[1]),
                                                max(nchar(c(do.call("c", contvars), 
                                                            catvars[1])))-1,
                                                flag = "0"))
    
    # bind model matrix
    X_DUM <- do.call("data.frame", lapply(seq_along(X_DUM), FUN = function(i,x) {
      the_name <- colnames(x)[i]
      OUT <- model.matrix(~ . -1, data = data.frame(x[, i]))
      colnames(OUT) <- paste0(the_name, "__", 1:ncol(OUT))
      return(OUT)
    }, x = X_DUM))
    
    discr_names <- colnames(X_DUM)
    ref_class <- !grepl("*__1", colnames(X_DUM))
    
    X_DUM <- as.matrix(X_DUM[, ref_class])
    colnames(X_DUM) <- discr_names[ref_class]
    return(X_DUM)
  }

  xe$X_DUM <- discr_sim(n = n,
                       catvars = catvars,
                       contvars = contvars,
                       noisevars = noisevars)
  
  discr_names <- colnames(xe$X_DUM)
  cont_names <- colnames(X_TRANS)
  
  # complete feature matrix
  X_TRANS <- cbind(X_TRANS, xe$X_DUM)
  X <- cbind(X, xe$X_DUM)

  colnames(X_TRANS) <- colnames(X) <- c(cont_names, discr_names)
    
  # manage interactions ----
  # interaction matrix raw (no interactions)
  int_list <- as.list(round(runif(ncol(X_TRANS), min(weights), max(weights)), 2))
  if(length(int_list)==0) {
    INT <- Matrix(0, 0, 0)
  } else {
    INT <- .bdiag(int_list)  
  }
  
  # sample interactions
  if (interactions > 1) {
    INT <- add_interactions(INT, weights, interactions)
  }
  
  # extract the target generating process
  int_raw <- sapply(seq_len(NCOL(INT)),
                    FUN = ext_name,
                    x = INT,
                    var = colnames(X_TRANS))
  
  # fix negative terms
  int_raw[grep("-", int_raw)] <- gsub("(.*)", "\\(\\1\\)", int_raw[grep("-", int_raw)])
  
  # create target ----
  target <- as.numeric(
            X_TRANS[, which(!grepl("NOISE", colnames(X_TRANS)))] %*%
            INT %*%
            rep(1, NCOL(INT))
            )
  
  # add intercept
  if (intercept) {
    i_cept <-  diff(abs(range(target)))*0.3
    i_cept_paste <- paste0("y = ", round(i_cept, 3), " + ")
    target <- target + i_cept
    I <- matrix(rep(1, n), nrow=n)
    colnames(I) <- "(Intercept)"
  } else {
    i_cept <- 0
    i_cept_paste <- "y = "
    I <- matrix(nrow=n)[,0]
  }
  
  # add noise
  noise_n <- rnorm(n)
  noise <- noise_n * as.vector(sqrt(var(target)/(stn*var(noise_n))))
  target <- target + noise
  
  # build data
  data <- data.table::data.table(cbind(I, as.matrix(X), as.matrix(E)))
  colnames(data) <- c(colnames(I), colnames(X_TRANS), enames)
  data[, c("y") := target]
  
  # transform target according to task
  tryCatch({data[, c("y") := task$link(get("y"))]}, error = function(e) stop("Could not apply link function."))
  tryCatch({data[, c("y") := task$cutoff(get("y"))]}, error = function(e) stop("Could not apply cutoff function."))
  
  # describe y
  tgp <- paste0(i_cept_paste, paste0(int_raw, collapse = " + "))
  
  # fix - terms
  tgp <- gsub(" \\+ \\(-", " - ", tgp)
  
  # fix brackets
  tgp <- gsub("\\)|\\(", "", tgp)
  
  # add error
  tgp <- paste0(tgp, " + e ~ N(0,", round(sd(noise), 2),")")
  
  # create the transformation matrix
  psi <- list()
  # add intercept
  psi[[1]] <- i_cept
  # X
  psi[[2]] <- INT
  # noise variables
  psi[[3]] <- diag(1, noisevars)
  # target
  psi[[4]] <- diag(1, 1)
  
  # include intercept
  if (intercept) {
    X <- data.table(cbind("(Intercept)" = matrix(1, nrow = n, ncol = 1), X))
  }
  
  # create the block diagonal transformation matrix
  psi <- Matrix::.bdiag(psi)
  
  # setting names
  psi@Dimnames[[2]] <- colnames(data)
  
  # create a formula object
  features <- colnames(psi)
  features <- features[-which(features == "y")]
  features <- gsub("\\(Intercept\\)", 1, features)
  eq <- formula(paste0("y ~ ", paste0(features, collapse = " + ")))
  
  # add class
  OUT <- list(data = as.data.table(data), 
              psi = psi,
              eq = eq,
              task = task,
              tgp = tgp,
              control = input)
  
  class(OUT) <- "Xy_sim"
  
  # return ----
  return(OUT)
}

