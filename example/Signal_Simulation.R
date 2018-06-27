###
## Pushing OLS to the limit
#


# PRELIMINARIES -----------------------------------------------------------

# load libraries
library(Xy)
library(data.table)
library(future.apply)
library(ggplot2)
library(scales)

# simulate data with interaction effects and polynomial degree of two
sig_sim = Xy(n = 1000,
             numvars = c(2,0), # 8 linear ; 0 nonlinear
             catvars = 0, # no dummies
             noisevars = 0, # 10 random variables
             task = Xy_task(), # 'regression' task
             nlfun = function(x) x^2, # polynomial degree of two
             interactions = 1, # interactions of up to degree two
             sig = c(1,30), # feature sd range
             cor = c(0), # feature collinearity range
             weights = c(-10,10), # feature weights range
             stn = 0.01)  # no collinearity between random variables and X

# extract the training data
training <- sig_sim$data
          
# underlying effects
coef(sig_sim)
coef(lm(y ~ -1 + ., data = training))


# SIMULATION --------------------------------------------------------------


# simulate a single signal to noise ratio estimation
simulate_single <- function(stn, runs = 100) {
  
  coef_diff <- list() 
  
  for (i in seq_len(runs)) {
  
    # simulate regression task with Xy
    my_simulation <- Xy(n = 1000,
                        numvars = c(10,0), # 10 linear ; 0 nonlinear
                        catvars = 0, # no dummies
                        noisevars = 10, # 10 random variables
                        task = Xy_task(), # 'regression' task
                        cor = c(0), # feature collinearity range
                        weights = c(-10,10), # feature weights range
                        stn = stn, # signal to noise ratio
                        intercept = TRUE,
                        noise.coll = FALSE)  # no collinearity between random variables and X
    
    
    # extract the training data
    training <- my_simulation$data
    
    # extract the features
    features <- grep("NLIN|LIN", names(training), value = TRUE)
    # create the formula
    lm_formula <- formula(paste0("y ~ ", paste0(features, collapse = " + ")))
    # estimate the model
    my_model <- lm(lm_formula, training)
    # calculate absolute deviation
    se <- (coef(my_simulation) - coef(my_model))^2
    # save results
    coef_diff[[i]] <- data.table(stn = stn, coef = names(se), se = se)
    
  }
   
  # bind results
  coef_diff <- do.call("rbind", coef_diff)
  
  # aggregate results
  OUTPUT <- coef_diff[, list(mse = mean(se, na.rm = TRUE)), by = c("stn", "coef")]

  # return results
  return(OUTPUT)
}

# setup parallel backend (CAUTION: uses every core)
plan(multiprocess)

# create a signal to noise ratio
ratios <- c(rev(1/seq(1, 5, 1)), seq(2, 5, 1))
# ratios <- seq(100, 1000, 100) # strong signals
  
# run simulation: this may take some time (~2 minutes @ 7 cores)
results <- future_lapply(ratios, FUN = simulate_single, runs = 1000)

# bind results
dt_results <- do.call("rbind", results)

# rescale the mse
dt_results[, mse := (mse - min(mse)) / (max(mse) - min(mse)), by = "coef"]

# plot results
ggplot(dt_results, aes(x = stn, y = mse, colour = coef)) + geom_line() +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Signal to noise ratio",
       y = "Rescaled MSE",
       title = "Mean Squared Error of OLS Coefficients vs. Simulated Coefficients") +
  scale_colour_discrete(name = "Coefficients")
  
