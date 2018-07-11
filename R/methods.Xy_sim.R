# PRINT -------------------------------------------------------------------

#' @param x an object of class \code{Xy_sim}.
#' @param object an object of class \code{Xy_sim}.
#' @param _data an object of class \code{Xy_sim}.
#' @param ... arguments to be passed to method
#' @rdname Xy
#' @method print Xy_sim
#' @export
#'
#' @examples
#' # Get a summary of your simulation
#' print(my_simulation)
#' 
print.Xy_sim <- function(x, ...) {
  if(!inherits(x, "Xy_sim")) 
    stop(paste0("x must be of class ", sQuote('Xy_sim')))
  # summary ----
  cat(paste0("Xy Simulation \n"))
  cat(paste0(" \t | \n"))
  cat(paste0(" \t | + task ", x$task$name, "\n"))
  cat(paste0(" \t | + observations ", x$control$n, "\n"))
  cat(paste0(" \t | + interactions ", x$control$interactions, "D", "\n"))
  cat(paste0(" \t | + signal to noise ratio ", x$control$stn, "\n"))
  
  # effects -----
  cat(paste0(" \t | + effects \n"))
  cat(paste0(" \t   | - linear ", x$control$numvars[1], "\n"))
  cat(paste0(" \t   | - nonlinear ", x$control$numvars[2], "\n"))
  cat(paste0(" \t   | - categorical ", x$control$catvars[1], "\n"))
  cat(paste0(" \t   | - noise ", x$control$noisevars, "\n"))
  
  # intervalls ----
  cat(paste0(" \t | + intervals \n"))
  # print correlation
  if (length(x$control$cor)>1) {
    cat(paste0(" \t   | - correlation ", "[", x$control$cor[1], ",", x$control$cor[2], "]" , "\n"))  
  } else {
    cat(paste0(" \t   | - correlation ", x$control$cor[1], "\n"))
  }
  # print weights
  if (length(x$control$weights)>1) {
    cat(paste0(" \t   | - weights ", "[", x$control$weights[1], ",", x$control$weights[2], "]" , "\n"))  
  } else {
    cat(paste0(" \t   | - weights ", x$control$weights[1], "\n"))
  }
  # print signal
  if (length(x$control$sig)>1) {
    cat(paste0(" \t   | - sd ", "[", x$control$sig[1], ",", x$control$sig[2], "]" , "\n"))  
  } else {
    cat(paste0(" \t   | - sd ", x$control$sig[1], "\n"))
  }
  
  cat(paste0("\n"))
  cat(paste0("\n"))
  cat(paste0("Target generating process: \n"))
  # data generating process ----
  cat(paste0(x$tgp))
  
  return(invisible())
}

# COEF --------------------------------------------------------------------

#' @rdname Xy
#' @method coef Xy_sim
#' @export
#' @examples
#' # Extracting the weights
#' coef(my_simulation)
coef.Xy_sim <- function(object, ...) {
  coefs <- diag(as.matrix(object$psi))
  coefs <- coefs[!grepl("y|NOISE", colnames(object$psi))]
  names(coefs) <- colnames(object$psi)[!grepl("y|NOISE", colnames(object$psi))]
  return(coefs)
}


# PLOT --------------------------------------------------------------------

#' @rdname Xy
#' @method plot Xy_sim
#' @export
#' @examples
#' # Plotting the true underlying effects
#' plot(my_simulation)
plot.Xy_sim <- function(x, ...) {
  # get data
  X <- x$data

  plt_df <- melt(X[, .SD, .SDcols = grep("y|NLIN|LIN", names(X), value = TRUE)], "y")
  plt_df <- plt_df[order(value),  .SD, by = "variable"]
  effects_plt <- ggplot(plt_df, aes_string(x = 'value', y = 'y')) + 
    geom_point(colour = "#13235B") + 
    facet_wrap(formula(paste0("~ variable")), scales = "free") + 
    theme_minimal(base_size = 14) +
    xlab("") +
    ggtitle("True effects X vs y") +
    geom_smooth(method = "loess", colour = "#00C792")
  
  print(effects_plt)
  return(effects_plt)
}


# TRANSFORM ---------------------------------------------------------------

#' @rdname Xy
#' @method transform Xy_sim
#' @export
#' @examples
#' # Getting the true underlying data of the process
#' transform(my_simulation)
transform.Xy_sim <- function(`_data`, ...) {
  # get data
  X <- copy(`_data`$data)
  nlins <- grep("NLIN", names(X), value = TRUE)
  
  # transform nonlinear columns
  if (length(nlins) > 1) {
  X[, c(nlins) := lapply(.SD, FUN = `_data`$control$nlfun), .SDcols = nlins]
  }
  
  # add weights
  X <- data.table(as.matrix(as.matrix(X) %*% `_data`$psi))
  return(X)
}


# FEATURE IMPORTANCE ------------------------------------------------------

#' Variable Importance
#' @param object an object of class \code{Xy_sim}
#' @param use.noise a boolean indicating whether the noise of the process should
#'                  be added to the variable importance
#' @param plot a boolean specifying whether to print the variable importance
#' @export
#' @examples
#' # Visualize Feature Importance of a Simulation
#' my_simulation <- Xy()
#' varimp(my_simulation)
varimp <- function(object, use.noise = FALSE, plot = TRUE) {
  # transform the data
  trans <- transform(object)
  # exclude
  pattern <- "[^(NOISE_[:digit:])|y]"
  vars <- grep(pattern, names(trans), value = TRUE)
  # calculate e
  trans[, noise := y-rowSums(.SD), .SDcols = vars]
  
  # should the noise be added to the variable importance
  if (use.noise) {
    vars <- grep(pattern, names(trans), value = TRUE)
  }
  
  # calculate importance contribution
  trans[, c(vars) := abs(.SD) / rowSums(abs(.SD)), .SDcols = vars]
  
  # subset dummies
  if (any(colSums(trans)==0)) {
    vars <- setdiff(vars, names(which(colSums(trans)==0)))
  }
  
  # feature importance (table)
  imp_raw <- copy(trans[, .SD, .SDcols = vars])
  imp <- imp_raw[, list(IMPORTANCE_MEAN = as.numeric(lapply(.SD, mean)),
                        IMPORTANCE_MEDIAN = as.numeric(lapply(.SD, median)),
                        IMPORTANCE_SD = as.numeric(lapply(.SD, sd)),
                        IMPORTANCE_MAD = as.numeric(lapply(.SD, mad))), .SDcols = names(imp_raw)]
  imp <- cbind(FEATURES = names(imp_raw), imp)
  imp <- imp[order(imp$IMPORTANCE_MEAN, decreasing = TRUE) ,]
  
  # create plot
  imp_df <- suppressWarnings(melt(imp_raw))
  p <- ggplot(imp_df, aes(y = value, x = reorder(variable, value, median))) +
    stat_boxplot(geom='errorbar', linetype="longdash", width= 0.3) +
    geom_boxplot(outlier.color = "#F25D57", 
                 colour = "#13235B",
                 outlier.shape = 15,
                 outlier.alpha = 0.3) + 
    coord_flip() + 
    theme_minimal(base_size = 14) +
    ylab("Importance") + xlab("")
  if (plot) {
  print(p)
  }
  
  return(imp)
}
