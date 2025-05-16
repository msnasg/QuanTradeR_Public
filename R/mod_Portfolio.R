
# ---------------------------------------------------------------------------- #
# All Portfolio UI
# ---------------------------------------------------------------------------- #
#' All ui and server of Portfolio tab
#'
#' @param id NS(id)
#' @param configs configs
#' @param .e .e env
#' @return Portfolio Analysis
#' @export
mod_Portfolio_ui <- function(id, configs, .e = .e){
  ns <- NS(id)

  tagList(

    conditionalPanel("input.PageViewMenu == null", ns = ns,
                        p("Here: Portfolio Dashboard or Help")
     ),# conditionalPanel
    # -------------------- #
    conditionalPanel(
      condition = "input.PageViewMenu == 'RiskReturn'", ns = ns,
      mod_Portfolio_RiskReturn_ui(id, configs, .e = .e)
    ), # conditionalPanel
    # -------------------- #
    conditionalPanel(
      condition = "input.PageViewMenu == 'Optimization'", ns = ns,
      mod_Portfolio_Optimization_ui(id, configs, .e = .e)
    ) # conditionalPanel
    # -------------------- #
  ) # tagList
} # mod_Portfolio_ui


# ---------------------------------------------------------------------------- #
# All Portfolio Server
# ---------------------------------------------------------------------------- #
#' All server side functions of Portfolio Tab
#'
#' @param id NS(id)
#' @param up_input server side inputs
#' @param .e .e env
#' @return Portfolio server
#' @export
mod_Portfolio_server <- function(id, up_input, .e = .e) {
  moduleServer(
    # Setting the id
    id,
    # Defining the module core mechanism
    function(input, output, session) {

      # ...................................................................... #
      mod_Portfolio_RiskReturn_server(input, output, session, id, up_input, .e = .e)

      # ...................................................................... #
      mod_Portfolio_Optimization_server(input, output, session, id, up_input, .e = .e)

      # .......................................................................... #
      # observe({ print(input$PageViewMenu )  })
      # ...................................................................... #
      output$dropdown_btn <- renderUI({

        if(!is.null(input$PageViewMenu)){
          if(input$PageViewMenu == 'RiskReturn'){

            Portfolio_RiskReturn_dropdownButton_func(id, .e =.e)

          } else if (input$PageViewMenu == 'Optimization'){

            Portfolio_Optimization_dropdownButton_func(id, .e =.e)
          } # if input$PageViewMenu
        } # not null
      })

    } # function
  ) # moduleServer
} # mod_Portfolio_server
