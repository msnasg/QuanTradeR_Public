
# ---------------------------------------------------------------------------- #
#' All ui functions of DataSet Button
#'
#' @param id NS(id)
#' @param configs configs
#' @param .e .e env
#' @return DataSet ui
#' @export
mod_DataSet_ui <- function(id, configs, .e = .e){

  ns <- NS(id)
  par_slct_witdth = "25%"

  fluidRow(style = "margin-left:10px;font-size:10px;", # height:400px;
           column(12,
                  div(class = "row",
                      style = "display:flex;flex-wrap:wrap;justify-content:space-between;width:100%;",
                      # Select Market to Filter Symbols
                      column(2,
                             shinyWidgets::pickerInput(inputId = ns("dataset_source_id"),
                                                       label = "Source",
                                                       choices = c("Alpha Vantage API", "DataBase", "Excel"),
                                                       selected = "Alpha Vantage API")
                      ),
                      # Select Market to Filter Symbols
                      column(2,
                             shinyWidgets::pickerInput(inputId = ns("dataset_market_id"),
                                                       label = "Market",
                                                       choices = c("Portfolio","Equity", "Forex", "Crypto",
                                                                   "Commodities", "Indices",
                                                                   "Options", "Futures", "ETFs",
                                                                   "All"),
                                                       selected = "Portfolio")
                      ),
                      # ---
                      column(2,
                             shinyWidgets::pickerInput(inputId = ns("dataset_regions_id"),
                                                       label = "Regions",
                                                       choices = c("North America","Latin America",
                                                                   "Asia/Pacific", "Europe",
                                                                   "Middle East", "Africa", "All"),
                                                       selected = "All")
                      ),
                      column(2,
                             shinyWidgets::pickerInput(inputId = ns("dataset_country_id"),
                                                       label = "Country",
                                                       choices = "All",
                                                       selected = "All")
                      ),
                      column(2,
                             shinyWidgets::pickerInput(inputId = ns("dataset_sector_id"),
                                                       label = "Sector",
                                                       choices = "All",
                                                       selected = "All")
                      ),
                    # Time Frame
                    column(3,
                      dateRangeInput(
                        inputId = ns("dataset_timerange_id"),
                        label = "Time Range",
                        start = Sys.Date() - 31, end = Sys.Date(),
                        min = NULL, max = NULL,
                        format = "yyyy-mm-dd",
                        startview = "month",
                        weekstart = 0, language = "en", separator = " to ",
                        width = NULL,
                        autoclose = TRUE
                      )
                    ),
                    column(6,
                      shinyWidgets::radioGroupButtons(
                        inputId = ns("dataset_timeframe_id"),
                        label = "Time Frame",
                        choices = c("30m","1h","4h","D","W","M","6M", "Y", "2Y", "5Y"),
                        selected = "D",
                        checkIcon = list(
                          yes = tags$i(class = "fa fa-check-square",
                                       style = "color: steelblue"),
                          no = tags$i(class = "fa fa-square-o",
                                      style = "color: steelblue"))
                      )
                    ),
                    column(2,
                           numericInput("dataset_window_id", "Window" ,
                                 value = 200, min = 1, max = NA, step = 1)
                           )
                  ) # div row
           ),
           # Select Symbols
           column(12,
                  div(class = "row",
                      style = "display:flex;flex-wrap:wrap;justify-content:space-between;width:100%;",
                      column(4,
                             shinyWidgets::pickerInput(
                               inputId = ns("dataset_symbols_id"),
                               label = "Ticker / Symbols",
                               choices = NULL, # .e$Symbols$ticker,
                               selected = NULL,
                               multiple = TRUE,
                               options = list('live-search'=TRUE, 'actions-box' = TRUE)
                               )
                      ),
                      column(4,
                             shinyWidgets::pickerInput(
                               inputId = ns("dataset_indices_id"),
                               label = "Benchmark Index",
                               choices = NULL,
                               selected = NULL,
                               multiple = TRUE,
                               options = list('live-search'=TRUE, 'actions-box' = TRUE)
                             )
                      ),
                      column(1,
                      ),
                      column(1, style = "margin-top:20px;",
                             actionButton(inputId = ns("dataset_xlsxtemplate_btn"),
                                          "Load (xlsx)")
                      ),
                      column(2, style = "margin-top:20px;",
                             actionButton(inputId = ns("dataset_createportfolio_btn"),
                                          "Create Portfolio")
                      )
                  ) # div row

           ),
           # Other Variables
           column(12,
                  div(class = "row",
                      style = "display:flex;flex-wrap:wrap;justify-content:space-between;width:100%;",
                      # ---
                      numericInput("dataset_initialcapital_id", "Initial Capital" ,
                          value = 1000, min = 1, max = NA, step = 1),
                      # ---
                      numericInput("dataset_maxrisk_percapital_id", "Max Risk % of Capital" ,
                                     value = 2.5, min = 0, max = 100, step = 0.01),
                      # ---
                      numericInput("dataset_maxpositions_id", "Max Positions" ,
                                   value = 100, min = 1, max = NA, step = 1)

                  ) # div row
           )

  ) # main fluidRow

} # mod_DataSet_ui

# ---------------------------------------------------------------------------- #
#' All server side functions of DataSet Button
#'
#' @param id NS(id)
#' @param up_input server side inputs
#' @param .e .e env
#' @return DataSet server
#' @export
mod_DataSet_server <- function(id, up_input, .e = .e){

  moduleServer(id,
    # Defining the module core mechanism
    function(input, output, session) {

      ns <- NS(id)

      showModal(modalDialog(
        size = "xl", easyClose = T, fade = TRUE,
        title = "Data Set: Select an Load Data for all tabs",
        mod_DataSet_ui(id = "navbartab_data_btn_id", configs, .e = .e) ,
        footer = tagList(
          actionButton(ns("load_dataset_btn"), "Load Data"),
          modalButton('Close')
        )
      ))

      observe({

        shiny::req(input$dataset_market_id)

        tickers = NULL
          if(input$dataset_market_id == "All"){
            tickers <- unname(.e$Symbols$ticker)
          }else{
            tickers <- unname(.e$Symbols[which(.e$Symbols$Market == input$dataset_market_id), "ticker"])
            indices <- unname(unlist(.e$Symbols[which(.e$Symbols$Market == "Index"), "ticker"]))
          }

        if(!is.null(tickers)){
          tickers <- unname(unlist(tickers))
          shinyWidgets::updatePickerInput(session = session,
                                          inputId = "dataset_symbols_id",
                                          choices = tickers,
                                          selected = tickers[1]  )

          shinyWidgets::updatePickerInput(session = session,
                                          inputId = "dataset_indices_id",
                                          choices = indices,
                                          selected = indices[1]  )
        }
      })

      # ---------------------------------------------------------------------- #
      # data Set btn to load dataset (Local / API) to global variable
      observeEvent(input$load_dataset_btn, ignoreInit = T, {

        shiny::req(input$dataset_symbols_id)
        # ------ #
        if(length(input$dataset_symbols_id) == 1){
          
          .e$data <<- .e$cETL_obj$read_localdata(symbol = input$dataset_symbols_id)

          showModal(modalDialog(
            size = "l", easyClose = T, fade = TRUE,
            title = "Data is loaded. !!",

            tagList(
              tags$ul(
                tags$li(paste("Symbol:", .e$data$symbol)),
                tags$li(paste("nrow:", nrow(.e$data$pr))),
                tags$li(paste("ncol:", ncol(.e$data$pr)))
              ),
              fluidRow(align = "center",
                       br(),
                       column(12, align = "center",
                              tableHTML::tableHTML(round(utils::head(.e$data$pr)[1:4,],2),
                                                   rownames = T,
                                                   collapse = "collapse",
                                                   footer = 'head'
                              ) %>%
                                tableHTML::add_css_thead(css = list('background-color', '#fafafa')) %>%
                                tableHTML::add_css_row(css = list('background-color', '#fff'),
                                            rows = tableHTML::even(2:4)) %>%
                                tableHTML::add_css_row(css = list('background-color', '#fafafa'),
                                            rows = tableHTML::odd(2:5))
                       ),
                       br(),
                       column(12, align = "center",
                              tableHTML::tableHTML(round(utils::tail(.e$data$pr),2),
                                                   rownames = T,
                                                   collapse = "collapse",
                                                   footer = 'tail'
                              ) %>%
                                tableHTML::add_css_thead(css = list('background-color', '#fafafa')) %>%
                                tableHTML::add_css_row(css = list('background-color', '#fff'),
                                            rows = tableHTML::even(2:5)) %>%
                                tableHTML::add_css_row(css = list('background-color', '#fafafa'),
                                            rows = tableHTML::odd(2:6))
                       )
                       )
            ),
            footer = NULL
          ))
        } else{
          # print(input$dataset_symbols_id)
          if(input$dataset_market_id == "Portfolio" && length(input$dataset_symbols_id) > 1){
            showModal(modalDialog(
              size = "s", easyClose = T, fade = TRUE,
              title = "Warning !!",
              "Please select just one Portfolio.",
              footer = NULL
            ))
          }
        }
      })

    } )
} # mod_DataSet_ui
