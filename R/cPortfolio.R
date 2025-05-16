
#' cPortfolio: Class for Portfolio Management
#'
#' @field par data.frame.
#' @importFrom methods setRefClass new
#' @return Tables, plots
#' @export
cPortfolio <- setRefClass("cPortfolio",
                          #contains = "cTimeSeries",
                          fields = list(
                          parL = "list",
                          pardf = "data.frame",
                          configs = "list",
                          par = "data.frame")
                        )
# ---------------------------------------------------------------------------- #
# methods
# ---------------------------------------------------------------------------- #
cPortfolio$methods(
  initialize = function(...){
    # library(tidyverse)
    # library(tidyquant)
    # library(fPortfolio)
    .self$example_datasets()
    library(fPortfolio)
    library(fMultivar)
    library(fAssets)
    library(cluster)
    library(PerformanceAnalytics)

  },
  # .......................................................................... #
  show = function(.self){
    cat("Class Name: ", class(.self), " initiated. \n", sep="")
    cat("par: ", class(.self$par), ", dim: (",
        paste0(dim(.self$par), collapse = ","),
        ") \n", sep="")
  },
  # .......................................................................... #
  # load default data
  example_datasets = function(.self){

    },
  # .......................................................................... #
  calc_returns = function(prices){

  },
  # .......................................................................... #
  portfolio_weights = function(ts){

  },
  # .......................................................................... #
  portfolio_weighted_return_plot = function(returns, weights, rebalance_on = "months"){

    },
  # .......................................................................... #
  portfolio_weighted_return_summaryplot = function(obj, rebalance_on = "weeks"){

  },
  # .......................................................................... #
  portfolio_fspecs = function(ts, specstype = "Default",
                              params = list()){

  },
  # .......................................................................... #

  # .......................................................................... #
  portfolio_fdata = function(ts, fspec){

  },
  # .......................................................................... #
  portfolio_frontier = function(ts, specstype = "Default",
                                params = list(),
                                cnsttype = "LongOnly",
                                cnstL = list(),
                                type = "All"){

  },
  # .......................................................................... #
  portfolio_2assetlines = function(obj){

  },
  # .......................................................................... #
  portfolio_frontier_simple = function(ts){

  },
  # .......................................................................... #
  portfolio_weights_plots = function(ts, specstype = "Default",
                               params = list(),
                               cnsttype = "LongOnly",
                               cnstL = list()){

  },

  # .......................................................................... #
  portfolio_constraints = function(ts, type = "LongOnly",
                                   cnstL = list()){


  },
  # .......................................................................... #
  portfolio_obj = function(fdata, fspec, constraints = "LongOnly",
                           objtype = NULL){

  },
  # .......................................................................... #
  portfolio_weights_bar = function(obj){

  },
  # .......................................................................... #
  portfolio_backtest = function(ts){

  },
  # .......................................................................... #
  portfolio_backtest_plot = function(ts){

  },
  # .......................................................................... #
  portfolio_backtest_risk_plot = function(Smooth){

  },
  # .......................................................................... #
  weightedReturns = function(obj, weights ){

  },
  # .......................................................................... #
  equalweight_riskmeasures = function(ts, alpha = 0.05){

  },
  # .......................................................................... #
  portfolio_lineplot = function(ts, type = "Price"){

  },
  # .......................................................................... #
  portfolio_returnplot = function(ts){

  },
  # .......................................................................... #
  portfolio_drawdownplot = function(ts, symb = "SBI"){

  },
  # .......................................................................... #
  portfolio_boxplot = function(ts){

  },
  # .......................................................................... #
  portfolio_densityplot = function(ts, symb = "SBI"){

  },
  # .......................................................................... #
  portfolio_qqnormplot = function(ts, symb = "SBI"){

  },
  # .......................................................................... #
  clustering_hclust = function(ts, method = "hclust"){

  },
  # .......................................................................... #
  portfolio_factors = function(ts){

  },
  # .......................................................................... #
  clustering_kmeans = function(ts){

  },
  # .......................................................................... #
  clustering_eigenvalue = function(ts){

  },
  # .......................................................................... #
  clustering_clusplot = function(ts, title = ""){

  },
  # .......................................................................... #
  all_clustring = function(ts, type = "", title = ""){

  },
  # .......................................................................... #
  starplot_basicStats = function(ts){

  },
  # .......................................................................... #
  corgram = function(ts){

  },
  # .......................................................................... #
  corrplot = function(ts, method = "pearson"){

  },
  # .......................................................................... #
  return_risk_bubbleplot = function(ts, returnfactor = "mean",
                                    riskfactor = "sd",
                                    sizefactor = 1){


  }

) # cPortfolio$methods


# ............................................................................ #
# My_tailoredFrontierPlot
# ............................................................................ #
My_tailoredFrontierPlot <- function (object,
                                  return = c("mean", "mu"),
                                  risk = c("Cov", "Sigma", "CVaR", "VaR"),
                                  mText = NULL, col = NULL, xlim = NULL, ylim = NULL,
                                  twoAssets = FALSE, sharpeRatio = TRUE, title = TRUE, ...){

} # My_tailoredFrontierPlot
