
#' @title All necessary Settings to create and maintain Shiny App
#' @description
#' It is not possible to run this function.
#' @export

ShinyAppSetUp <- function() {
  # ........................................................................ #
  # code complexity: number of files in the project
  # ........................................................................ #

  # Run Local
  pkgload::load_all()
  Run_ShinyApp(app = "QuanTradeR", host = "127.0.0.1", launch.browser = NULL,  local = T)

  # Run on browser
  Run_ShinyApp(app = "QuanTradeR", host = "127.0.0.1", launch.browser = T,  local = T)
}
