#' Title All ui and server of Market Scanner tab
#'
#' @param id NS(id)
#' @param .e .e env
#' @return Market info
#' @export
mod_MarketScanner_ui <- function(id, .e = .e) {
  ns <- NS(id)

tagList(
    conditionalPanel(
      condition = "input.PageViewMenu == 'Forex'", ns = ns,
      fluidRow(
        column(
          7,
          column(
            12,
            htmlTemplate(filename =
                           ifelse(.e$Deployflag, paste0("./HTML/TView_ForexCrossRatesWidget.Rhtml"),
                                  system.file("app/HTML/", "TView_ForexCrossRatesWidget.Rhtml",
                                              package = pkgload::pkg_name()  ))
                         )
          ),
          column(12,
            style = "margin-top:-20px;",
            htmlTemplate(filename =
                           ifelse(.e$Deployflag, paste0("./HTML/TView_ForexHeatMapWidget.Rhtml"),
                                  system.file("app/HTML/", "TView_ForexHeatMapWidget.Rhtml",
                                              package = pkgload::pkg_name() ))
                         )
          ),
          column(12,
            style = "margin-top:-20px;",
            htmlTemplate(filename =
                           ifelse(.e$Deployflag, paste0("./HTML/TView_ForexWidget.Rhtml"),
                                  system.file("app/HTML/", "TView_ForexWidget.Rhtml",
                                              package = pkgload::pkg_name() ))
                         )
          )
        ), # 6
        column(
          5,
          htmlTemplate(filename =
                         ifelse(.e$Deployflag, paste0("./HTML/TView_TechnicalAnalysis_Widget_Forex.Rhtml"),
                                system.file("app/HTML/", "TView_TechnicalAnalysis_Widget_Forex.Rhtml",
                                            package = pkgload::pkg_name() ) )
                       )
        )
      ) # fluidRow
    ),

    conditionalPanel(
      condition = "input.PageViewMenu == 'Stocks'", ns = ns,
      fluidRow(
        htmlTemplate(filename =
                       ifelse(.e$Deployflag, paste0("./HTML/TView_ForexCrossRatesWidget.Rhtml"),
                              system.file("app/HTML/", "TView_ForexCrossRatesWidget.Rhtml",
                                          package = pkgload::pkg_name() ))
          )
      )
    ),
    # EconomicCalendar
    conditionalPanel(
      condition = "input.PageViewMenu == 'EconomicCalendar'", ns = ns,
      fluidRow(
        htmlTemplate(filename =
                       ifelse(.e$Deployflag, paste0("./HTML/TView_EconomicCalendarWidget.Rhtml"),
                              system.file("app/HTML/", "TView_EconomicCalendarWidget.Rhtml",
                                            package = pkgload::pkg_name() ))
                     )
      ) # fluidRow
    ), # conditionalPanel

    # Indices
    conditionalPanel(
      condition = "input.PageViewMenu == 'Indices'", ns = ns,
      fluidRow(
        htmlTemplate(filename =
                       ifelse(.e$Deployflag, paste0("./HTML/TView_TickerWidget.Rhtml"),
                              system.file("app/HTML/", "TView_TickerWidget.Rhtml",
                                          package = pkgload::pkg_name() ))
                     ),
        htmlTemplate(filename =
                       ifelse(.e$Deployflag, paste0("./HTML/TView_IndicesWidget.Rhtml"),
                              system.file("app/HTML/", "TView_IndicesWidget.Rhtml",
                                          package = pkgload::pkg_name() ))
                     )
      ) # fluidRow
    ), # conditionalPanel

    # crypto
    conditionalPanel(
      condition = "input.PageViewMenu == 'Crypto'", ns = ns,
      fluidRow(
        column(12,
               htmlTemplate(filename =
                              ifelse(.e$Deployflag, paste0("./HTML/TView_Cryptos_OverView_Widget.Rhtml"),
                                     system.file("app/HTML/", "TView_Cryptos_OverView_Widget.Rhtml",
                                                 package = pkgload::pkg_name() ))
                            )
               ),
        column(12,
               htmlTemplate(filename =
                              ifelse(.e$Deployflag, paste0("./HTML/TView_Cryptos_Performance_Widget.Rhtml"),
                                     system.file("app/HTML/", "TView_Cryptos_Performance_Widget.Rhtml",
                                                   package = pkgload::pkg_name() ))
                            )
               )
        ) # fluidRow
    ) # conditionalPanel

) # tagList

} # mod_MarketScanner

mod_MarketScanner_server <- function(id, up_input, .e = .e) {
  moduleServer(
    # Setting the id
    id,
    # Defining the module core mechanism
    function(input, output, session) {

    } # function
  ) # moduleServer
} # mod_MarketScanner_server
