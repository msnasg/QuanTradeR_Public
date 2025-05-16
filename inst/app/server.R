

#' Title Shiny App Server
#'
#' @param input
#' @param output
#' @param session
#' @return
#' @export
#' @import shiny
#' @examples
server <- function(input, output, session) {


  result_auth <<- shinymanager::secure_server(check_credentials =
                                                shinymanager::check_credentials(Authorization_Table))

  # .......................................................................... #
  # Data btn ui & server
  # .......................................................................... #
  observeEvent(input$navbartab_data_btn,{
    mod_DataSet_server(id = "navbartab_data_btn_id", up_input = input, .e = .e)

  })

  # .......................................................................... #
  # mainsidebar_id_marketscanner_mainbox
  # .......................................................................... #
  mod_MainBox_server("mainsidebar_id_marketscanner", up_input = input, configs)
  mod_MainBox_server("mainsidebar_id_marketiming_mainbox", up_input = input, configs)
  mod_MainBox_server("mainsidebar_id_financialmetrics_mainbox", up_input = input, configs)
  mod_MainBox_server("mainsidebar_id_portfolio", up_input = input, configs)
  mod_MainBox_server("mainsidebar_id_riskmanagement_mainbox", up_input = input, configs)
  mod_MainBox_server("mainsidebar_id_tradingstrategy_mainbox", up_input = input, configs)
  mod_MainBox_server("mainsidebar_id_aidrivenanalysis_mainbox", up_input = input, configs)
  mod_MainBox_server("mainsidebar_id_derivatives", up_input = input, configs)
  mod_MainBox_server("mainsidebar_id_alternativeinvestment_mainbox", up_input = input, configs)


  # .......................................................................... #
  # Market Screener
  # .......................................................................... #
    mod_MarketScanner_server("mainsidebar_id_marketscanner", up_input = input, .e = .e)

  # .......................................................................... #
  # Derivatives
  # .......................................................................... #
    mod_Derivatives_server("mainsidebar_id_derivatives", up_input = input, .e = .e)

  # .......................................................................... #
  # Portfolio Management
  # .......................................................................... #
    mod_Portfolio_server("mainsidebar_id_portfolio", up_input = input, .e = .e)

  # .......................................................................... #
  # sidebar main title
  # .......................................................................... #
  observeEvent(input$id_sidebar,{
    font = ifelse(input$id_sidebar, "font-size:14px;", "font-size:10px;")
    mod_dashnavbartitle_server("dashnavbartitle_id", font = font)
  })
}
