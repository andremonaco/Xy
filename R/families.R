#' Family classes for a Xy simulation recipe
#'
#' These gathered list of family objects let you alter the distributional family
#' of a simulated variable. They are invoked within the effect generating functions [`add_effects`].
#' The families are using the stats functions corresponding to the distributional
#' family, i.e. for the normal distribution [`pnorm`].
#' @rdname families
#' @param mean a single numeric location parameter (see [`pnorm`]) 
#' @param meanlog a single numeric location parameter (see [`plnorm`]) 
#' @param sd a single numeric scale parameter (see [`pnorm`]) 
#' @param sdlog a single numeric scale parameter (see [`plnorm`]) 
#' @param size a single number of trials (see [`pbinom`])
#' @param prob a single probability (see [`pbinom`], [`pgeom`])
#' @param location a single location parameter (see [`pcauchy`], [`plogis`])
#' @param scale  a single scale paraneter (see [`pcauchy`], [`plogistic`], [`pweibull`], [`plogis`])
#' @param lambda a single numeric value of means (see [`ppois`])
#' @param shape a single shape parameter (see [`pgamma`], [`pweibull`])
#' @param shape1 a single non-negative parameter (see [`pbeta`])
#' @param shape2 a single non-negative parameter (see [`pbeta`])
#' @param rate a single non-negative rate parameter (see [`pexp`], [`pgamma`])
#' @param scale a single scale parameter (see [`pgamma`])
#' @param df a single integer specifying the degrees of freedom (see [`pchisq`], [`pt`])
#' @param df1 a single integer specifying the degrees of freedom (see [`pf`])
#' @param df2 a single integer specifying the degrees of freedom (see [`pf`])
#' @param m the number of observations in the first sample (see [`phyper`], [`pwilcox`])
#' @param n the number of observations in the second sample (see [`phyper`], [`pwilcox`], [`psignrank`])
#' @param k the number of observations drawn from the sample (see [`phyper`])
#' @param min a single numeric value specifying the lower bound (see [`punif`])
#' @param max a single numeric value specifying the upper bound (see [`punif`])
#' @param ncp a non-centrality parameter (e.g. see [`pbinom`])
#' 
#' @importFrom magrittr "%>%"
#' @importFrom tibble tibble
#' @importFrom rlang abort
#' 
#' @return a tibble with information on the distributional properties. This information
#'         is only used internally forwarded to the copula simulation.
#' @examples
#' # build a simulation recipe with linear features from the cauchy distribution
#' xy_recipe <- Xy(task = "regression") %>%
#' # add linear features with the desired distribution
#' add_linear(p = 5, family = xy_cauchy(location = 0, scale = 3))

#' # build a simulation recipe with squared nonlinear features from the normal distribution
#' xy_recipe <- Xy(task = "regression") %>%
#' # add nonlinear features with the desired distribution
#' add_nonlinear(p = 5, nlfun = function(x) x^2, 
#'               family = xy_normal(mean = 0, sd = 3))
#' 
#' @name families
NULL

#' @name families
#' @export
xy_beta <- function(shape1 = 3, shape2 = 1, ncp = 0) {
  if (ncp < 0) {
    rlang::abort("ncp must be a non-negative numeric value. `help(pbeta)`")
  }
  tibble(params = list(list(shape1 = shape1, shape2 = shape2, ncp = ncp)), 
         family = "beta")
}

#' @name families
#' @export
xy_binom <- function(size = 2, prob = 0.1) {
  tibble(params = list(list(size = size, prob = prob)), 
         family = "binom")
}

#' @name families
#' @export
xy_cauchy <- function(location = 0, scale = 1) {
  tibble(params = list(list(location = location, scale = scale)), 
         family = "cauchy")
}

#' @name families
#' @export
xy_chisq <- function(df = 3, ncp = 0) {
  # check input
  if (df < 0) {
    rlang::abort("df must be a non-negative numeric value. `help(pchisq)`")
  }
  
  if (ncp < 0) {
    rlang::abort("ncp must be a non-negative numeric value. `help(pchisq)`")
  }
  
  tibble(params = list(list(df = df, ncp = ncp)), 
         family = "chisq")
}

#' @name families
#' @export
xy_exp <- function(rate = 1) {
  if (rate <= 0) {
    rlang::abort("ncp must be a numeric value greater than zero. `help(pexp)`")
  }
  
  tibble(params = list(list(rate = rate)), 
         family = "exp")
}

#' @name families
#' @export
xy_f <- function(df1 = 1, df2 = 2, ncp = 0) {
  
  # check input
  if (df1 < 0) {
    rlang::abort("df1 must be a non-negative numeric value. `help(pf)`")
  }
  
  if (df2 < 0) {
    rlang::abort("df2 must be a non-negative numeric value. `help(pf)`")
  }
  
  if (ncp < 0) {
    rlang::abort("ncp must be a non-negative numeric value. `help(pf)`")
  }
  
  tibble(params = list(list(df1 = df1, df2 = df2, ncp = ncp)), 
         family = "f")
  
}

#' @name families
#' @export
xy_gamma <- function(shape = 1, rate = 2, scale = 1/rate) {
  # check input
  if (shape < 0) {
    rlang::abort("shape must be a non-negative numeric value. `help(pgamma)`")
  }
  
  if (rate <= 0) {
    rlang::abort("rate must be a value greater than zero. `help(pgamma)`")
  }
  
  if (scale <= 0) {
    rlang::abort("scale must be a value greater than zero. `help(pgamma)`")
  }
  
  tibble(params = list(list(shape = shape, rate = rate, scale = scale)), 
         family = "gamma")
}

#' @name families
#' @export
xy_geometric <- function(prob = 0.5) {
  # check input
  if (!dplyr::between(prob, 0, 1)) {
    rlang::abort("prob must be a probability between zero and one. `help(pgeom)`")
  }
  tibble(params = list(list(prob = prob)), 
         family = "geom")
}

#' @name families
#' @export
xy_hypergeometric <- function(m = 10, n = 7, k = 8) {
  
  # check input
  if (m < 0) {
    rlang::abort("m must be a non-negative numeric value. `help(phyper)`")
  }
  
  # check input
  if (n < 0) {
    rlang::abort("n must be a non-negative numeric value. `help(phyper)`")
  }
  
  # check input
  if (k < 0) {
    rlang::abort("k must be a non-negative numeric value. `help(phyper)`")
  }
  
  tibble(params = list(list(m = m, n = n, k = k)), 
         family = "hyper")
}

#' @name families
#' @export
xy_logistic <- function(location = 0, scale = 1) {
  tibble(params = list(list(location = location, scale = scale)), 
         family = "logis")
}

#' @name families
#' @export
xy_lognormal <- function(meanlog = 0, sdlog = 1) {
  tibble(params = list(list(meanlog = meanlog, sdlog = sdlog)), 
         family = "lnorm")
}

#' @name families
#' @export
xy_normal <- function(mean = 0, sd = 3) {
  tibble(params = list(list(mean = mean, sd = sd)), 
         family = "norm")
}

#' @name families
#' @export
xy_poisson <- function(lambda = 1) {
  tibble(params = list(list(lambda = lambda)), 
         family = "pois")
}

#' @name families
#' @export
xy_signrank <- function(n = 5) {
  # check input
  if (n < 0) {
    rlang::abort("n must be a non-negative numeric value. `help(psignrank)`")
  }
  tibble(params = list(list(n = n)), 
         family = "signrank")
}

#' @name families
#' @export
xy_t <- function(df = 1, ncp = 0) {
  
  # check input
  if (df < 0) {
    rlang::abort("df must be a non-negative numeric value. `help(pt)`")
  }
  
  tibble(params = list(list(df = df, ncp = ncp)), 
         family = "t")
}

#' @name families
#' @export
xy_uniform <- function(min = 0, max = 1) {
  tibble(params = list(list(min = min, max = max)), 
         family = "unif")
}
  
#' @name families
#' @export
xy_weibull <- function(shape = 1, scale = 1) {
  tibble(params = list(list(shape = shape, scale = scale)), 
         family = "weibull")
}
  
#' @name families
#' @export
xy_wilcox <- function(m = 4, n = 6) {
  
  # check input
  if (m < 0) {
    rlang::abort("m must be a non-negative numeric value. `help(pwilcox)`")
  }
  
  if (n < 0) {
    rlang::abort("n must be a non-negative numeric value. `help(pwilcox)`")
  }
  
  tibble(params = list(list(m = m, n = n)), 
         family = "wilcox")
}