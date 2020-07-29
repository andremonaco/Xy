# FEATURE IMPORTANCE ------------------------------------------------------
#' @title Get the feature importance of an [Xy()] simulation
#' @details The importance is calculated as the relative local contribution of the effect,
#'          for instance the absolute share of a linear simulated effect (x_1) to
#'          the overall magnitude of y. The relative shares are then aggregated
#'          over the whole simulated data set
#' @param object an object of class `xy_sim`
#' @param plot a boolean signaling whether to print an importance plot
#' @import dplyr tibble ggplot2
#' @importFrom rlang abort
#' @export
#' @return Returns the effect importance of a cooked simulation recipe. The `plot`
#'         argument can be used to create a feature importance plot.
#' @examples 
#' # create a simulation
#' linear_sim <- Xy() %>%
#' add_linear(p = 5) %>%
#' Xy::simulate(n=100)
#' # fetch the variable importance of the simualtion
#' linear_sim %>% importance()
importance <- function(object, plot = TRUE) {
  
  # check input
  if (!inherits(object, "xy_sim")) {
    rlang::abort("object must be of class xy_sim")
  }
  
  # transform the data
  trans <- transform(object)
  
  rel_imp <- function(x, y) abs(x) / abs(y)
  imp_raw <- trans %>%
    as_tibble() %>%
    select(-y) %>%
    mutate(sum = rowSums(abs(.))) %>%
    mutate_all(., .funs = ~rel_imp(x = .x, y = sum)) %>%
    select(-sum) %>%
    select_if(function(x) var(x) != 0)
  
  # feature importance (table)
  imp <- imp_raw %>%
    tidyr::gather() %>%
    group_by(key) %>%
    summarize(mean = mean(value),
              median = median(value),
              sd = sd(value),
              mad = mad(value), .groups = 'keep') %>%
    rename("effect" = "key") %>%
    arrange(desc(median)) %>%
    ungroup()
  
  if (plot) {
    # create plot
    imp_df <- imp_raw %>%
      tidyr::gather()
    
    p <- ggplot(imp_df, aes(y = value, x = reorder(key, value, median))) +
      stat_boxplot(geom='errorbar', linetype="longdash", width= 0.3) +
      geom_boxplot(outlier.color = "#F25D57", 
                   colour = "#13235B",
                   outlier.shape = 15,
                   outlier.alpha = 0.3) + 
      coord_flip() + 
      theme_minimal(base_size = 14) +
      ylab("Importance") + xlab("")
    
    print(p)
  }
  return(imp)
}