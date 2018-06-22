# PRINT -------------------------------------------------------------------

#' @param x an object of class \code{Xy_sim}.
#' @param object an object of class \code{Xy_sim}.
#' @param _data an object of class \code{Xy_sim}.
#' @param ... arguments to be passed to methods
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
  cat(paste0(" \t | + family ", x$family$name, "\n"))
  cat(paste0(" \t | + observations ", x$control$n, "\n"))
  cat(paste0(" \t | + interactions ", x$control$interactions, "D", "\n"))
  cat(paste0(" \t | + signal ", (x$control$stn)*100, " %", " -- noise ", (1-x$control$stn)*100, " %",  "\n"))
  
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
  cat(paste0("Data generating process: \n"))
  # data generating process ----
  cat(paste0(x$dgp))
}

# COEF --------------------------------------------------------------------

#' @rdname Xy
#' @method coef Xy_sim
#' @export
#' @examples
#' # Extracting the weights
#' coef(my_simulation)
coef.Xy_sim <- function(object, ...) {
  if(!inherits(object, "Xy_sim")) 
    stop(paste0("object must be of class ", sQuote('Xy_sim')))
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
  if(!inherits(x, "Xy_sim")) 
    stop(paste0("x must be of class ", sQuote('Xy_sim')))
  
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
}


# TRANSFORM ---------------------------------------------------------------

#' @rdname Xy
#' @method transform Xy_sim
#' @export
#' @examples
#' # Getting the true underlying data of the process
#' transform(my_simulation)
transform.Xy_sim <- function(`_data`, ...) {
  if(!inherits(`_data`, "Xy_sim")) 
    stop(paste0("`_data` must be of class ", sQuote('Xy_sim')))
  
  # get data
  X <- `_data`$data
  nlins <- grep("NLIN", names(X), value = TRUE)
  
  # transform nonlinear columns
  if (length(nlins) > 1) {
  X[, c(nlins) := lapply(.SD, FUN = `_data`$control$nlfun), .SDcols = nlins]
  X[, c(nlins) := lapply(.SD, FUN = scale, center = TRUE, scale = TRUE), .SDcols = nlins]
  }
  
  # add weights
  X <- data.table(as.matrix(as.matrix(X) %*% `_data`$psi))
  return(X)
}
