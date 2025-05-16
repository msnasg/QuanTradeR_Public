
#' cTimeSeries: Class for TimeSeries Analysis
#'
#' @field par data.frame.
#' @importFrom methods setRefClass new
#' @return Tables, plots
#' @export
cTimeSeries <- setRefClass("cTimeSeries",
                          fields = list(
                            parL = "list",
                            pardf = "data.frame",
                            configs = "list",
                            par = "data.frame")
)

# ---------------------------------------------------------------------------- #
# methods
# ---------------------------------------------------------------------------- #
cTimeSeries$methods(
  initialize = function(...){
    .self$example_datasets()
  },
  # .......................................................................... #
  show = function(.self){
    cat("Class Name: ", class(.self), " initiated. \n", sep="")
    cat("par: ", class(.self$par), ", dim: (",
        paste0(dim(.self$par), collapse = ","),
        ") \n", sep="")
  },
  # .......................................................................... #
  example_datasets = function(.self){

  },
  # .......................................................................... #
  price_ts = function(symbol, col = "close"){

  },
  # .......................................................................... #
  convertprice_ts = function(df){

  },
  # .......................................................................... #
  returns_ts = function(ts){

  },
  # .......................................................................... #
  cumulatedreturns_ts = function(ts){

  },
  # .......................................................................... #
  drawdownsreturns_ts = function(ts){

  },
  # .......................................................................... #
  basicStats_ts = function(ts){

  },
  # .......................................................................... #
  drawdownsStats_ts = function(ts, i = 1){

  },
  # .......................................................................... #
  cov_ts = function(ts, digits = 4){

  },
  # .......................................................................... #
  test_normality = function(ts, method = "shapiro"){

  },
  # .......................................................................... #
  test_assetsfit = function(ts, method = "st"){

  },
  # .......................................................................... #
  capm_ts = function(){

  },
  # .......................................................................... #
  performancesummary_plt = function(ts, symb){

  },
  # .......................................................................... #
  calc_beta = function(df,
                       Rf = 0,
                       symbol = "TSLA",
                       benchmark_symbol = "SPY"){

  },
  # .......................................................................... #
  rolling_capm = function(df,
                          Rf = 0,
                          symbol = "TSLA",
                          benchmark_symbol = "SPY"){


    },
  # .......................................................................... #
  all_ratios = function(df, Rf = 0,
                        benchmark_symbol = "SPY"){

  },
  # .......................................................................... #
  ratio_tables = function(df,
                          Rf = 0,
                          symbol = "TSLA",
                          benchmark_symbol = "SPY",
                          whichtable = "AnnualizedReturns"){

  },

  # .......................................................................... #
  chart_CalendarReturns = function(df,
                                   symbol = "TSLA"){

  },
  # .......................................................................... #
  chart_CaptureRatios = function(df,
                              symbol = "TSLA",
                              benchmark_symbol = "SPY"){
  },
  # .......................................................................... #
  chart_Regression = function(df,
                              Rf = 0,
                              symbol = "TSLA",
                              benchmark_symbol = "SPY",
                              fit = "conditional"){

  },
  # .......................................................................... #
  chart_RelativePerformance = function(df,
                                      symbol = "TSLA",
                                      benchmark_symbol = "SPY"){

  },

  # .......................................................................... #
  chart_RollingCorrelation = function(df,
                                      symbol = "TSLA",
                                      benchmark_symbol = "SPY"){

  },
  # .......................................................................... #
  chart_VaRSensitivity = function(df, symbol = "TSLA"){

  }

) # cTimeSeries$methods
