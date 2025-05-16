
#' cETL: Class for DataSet ETL
#'
#' @field par data.frame
#' @importFrom methods setRefClass new
#' @return Tables, list, DB
#' @export
cETL <- setRefClass("cETL",
                    fields = list(
                      APIKEY = "list",
                      parL = "list",
                      pardf = "data.frame",
                      configs = "list",
                      par = "data.frame")
) # setRefClass

# Class methods
cETL$methods(
  initialize = function(...){
    .self$APIKEY = .e$APIKEY
  },
  # .......................................................................... #
  show = function(.self){
    cat("Class Name: ", class(.self), " initiated. \n", sep="")
    cat("par: ", class(.self$par), ", dim: (",
        paste0(dim(.self$par), collapse = ","),
        ") \n", sep = "")
  },
  # .......................................................................... #
  symbolsearch_alphavantage = function(keywords){

  },
  # .......................................................................... #
  read_alphavantage_options = function(symbol = "IBM"){

  }, 
  # .......................................................................... #
  save_datafolder = function(){

  },
  # .......................................................................... #
  make_portfolio = function(Symbols = c(), portfolio_name = "portfo_1"){

  },
  # .......................................................................... #
  read_localdata = function(symbol = "TSLA"){

  },
  # .......................................................................... #
  read_localdata_colnames = function(symbol = "TSLA"){

  },
  # .......................................................................... #
  read_alphavantage_dailyprice = function(symbol = "TSLA"){

  },
  # .......................................................................... #
  read_alphavantage_index = function(symbol = "SPY"){

  },
  # .......................................................................... #
  read_alphavantage_fx_daily = function(symbol = "EURUSD", window = "compact"){

  }

)
