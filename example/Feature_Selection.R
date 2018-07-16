###
## Feature Importance
#

# clear all
rm(list = ls())


# PRELIMINARIES -----------------------------------------------------------

# load libraries
library(Xy)
library(data.table)
library(caret)
library(dplyr)
library(ggplot2)

# setting the seed
set.seed(123)

# how many simulations should be started
n <- 20

# number of models per algorithm
n_models <- 100

# draw weights
w <- runif(1, 0, 10)

# numvars
num_vars <- sample(0:100, size = 2)

# categorical vars
cat_vars <- sample(0:5, size = 1)

# noisevars
noise_vars <- sample(1:100, size = 1)

# degree
pol_deg <- sample(1:4, size = 1)

# signal to noise ratio
stn_ratio <- sample(1:20, size = 1)

# different feature selection algorithms
tr_methods <- c("xgbDART", "glmboost", "foba", "rpart")

# result container
results <- list()

# loop over simulations
for (i in seq_len(n)) {
  
  # number of observations to simulate
  obs <- sample(1000:5000, size = 1)
  
  # simulate data 
  fs_sim <- Xy(n = obs,
               numvars = num_vars, # 10 linear ; 10 nonlinear
               catvars = c(cat_vars, 2), # 3 dummies with 2 categories
               noisevars = noise_vars, # 50 random variables
               task = Xy_task(), # 'regression' task
               nlfun = function(x) x^pol_deg, # polynomial degree of two
               interactions = 1, # interactions of up to degree two
               sig = c(1,4), # feature sd range
               cor = c(0), # feature collinearity range
               weights = c(-w,w), # feature weights range
               intercept = TRUE,
               stn = stn_ratio)  # signal to noise
  
  # the underlying variable importance
  sim_varimp <- Xy::varimp(fs_sim, plot = TRUE)
  
  # for measure 1
  sim_varimp[, REAL_IMPORTANCE := 100*(IMPORTANCE_MEAN - min(IMPORTANCE_MEAN)) / (max(IMPORTANCE_MEAN)-min(IMPORTANCE_MEAN))]
  real_imp <- copy(sim_varimp[, .SD, .SDcols = c("REAL_IMPORTANCE", "FEATURES")])
  setkey(real_imp, "FEATURES")
  # for measure 2
  total_features <- nrow(sim_varimp)
  
  
  
  
  # MODEL TRAINING ----------------------------------------------------------
  
  # split into training and test to tune algorithms
  split_ind <- sample(seq_len(obs), size = floor(obs*0.3), replace = FALSE)
  training <- fs_sim$data[-split_ind,] 
  testing <- fs_sim$data[split_ind,]
    
  # get the formula
  eq <- fs_sim$eq
  
  # get model.matrix + label
  x_train <- as.matrix(training[, .SD, .SDcols = all.vars(eq)[-1]])
  y_train <- training[, get(all.vars(eq)[1])]
  
  # try caret
  train_control <- trainControl(method = "cv",
                                number = 5,
                                search = "random",
                                allowParallel = TRUE)
  
    # train every method
    model_objects <- measures <- list()
    for(tr in seq_along(tr_methods)) {
    model_objects[[tr]] <- train(x = x_train,
                                 y = y_train,
                                 method = tr_methods[tr],
                                 trControl = train_control,
                                 tuneLength = n_models
                                 )
    # safe the importance
    tmp <- varImp(model_objects[[tr]])$importance
    
    # create output data
    est_imp <- data.table(FEATURES = rownames(tmp),
                          IMPORTANCE = unlist(tmp),
                          TYPE = ifelse(grepl("NOISE_.*", rownames(tmp)),
                                        "NOISE",
                                        "REAL"))
    # prepare measure 1
    setkey(est_imp, "FEATURES")
    diff_imp <- real_imp[est_imp]
    # calculate measure 1
    tmp_m1 <- diff_imp[TYPE == 'REAL', mean(abs(IMPORTANCE - REAL_IMPORTANCE))]
    
    # prepare measure 2 and 3
    est_features <- est_imp[TYPE == "REAL", .N]
    est_noise <-  est_imp[TYPE == "NOISE", .N]
    w_features <- est_imp[TYPE == "REAL", sum(IMPORTANCE)]
    w_noise <- est_imp[TYPE == "NOISE", sum(IMPORTANCE)]
    # calculate measure 2
    tmp_m2 <- round(est_features / total_features * 100, 2)
    # calculate measure 3
    tmp_m3 <- est_features * w_features / (est_features * w_features + est_noise * w_noise)
    
    measures[[tr]] <- data.table(SIM = i, 
                                 ALGORITHM = tr_methods[tr], 
                                 M1 = tmp_m1, 
                                 M2 = tmp_m2,
                                 M3 = tmp_m3 * 100)
    }
    
    results[[i]] <- data.table(do.call("rbind", measures))
    
    # check dir
    if (!dir.exists("~/Desktop/FS"))
      dir.create("~/Desktop/FS")
    
    # save results to HDD (better save than sorry)
    saveRDS(reslults[[i]], file = paste0("~/Desktop/FS/result=", i, ".RDS"))
    
} 

fs_simulation <- do.call("rbind", results)

#save(fs_simulation, file = "~/Desktop/fs_simulation.RData")

# OUTCOME EVALUATION ------------------------------------------------------

# boxplot
rslt_plt <- fs_simulation %>% 
                dplyr::select(., -SIM) %>%
                rename(., 
                       "ALGORITHM" = ALGORITHM, 
                       "Mean Absolute Error (M1)" = M1, 
                       "% Features Selected (M2)" = M2 , 
                       "Weighted Ratio (M3)" = M3) %>%
                reshape2::melt("ALGORITHM") %>%
              ggplot(., aes(y = value, colour = ALGORITHM)) + 
                geom_boxplot() +
                coord_flip() + 
                facet_wrap(~variable, ncol = 1) +
                theme_minimal(base_size = 14) +
                theme(legend.position = "bottom",
                      axis.text.y = element_blank()) +
                scale_colour_manual(name = "", values = 
                                      c("#D6263B", "#162552", 
                                        "#00B383", "#0085AF")) +
                xlab("") +
                ylab("")

# mean scores
fs_simulation %>%
  select(-SIM) %>% 
  group_by(ALGORITHM) %>%
  summarize_all(.funs = mean, na.rm = TRUE)