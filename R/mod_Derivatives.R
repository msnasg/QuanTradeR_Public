# ---------------------------------------------------------------------------- #
# All Derivatives UI
# ---------------------------------------------------------------------------- #
#' All ui and server of Derivatives tab
#'
#' @param id NS(id)
#' @param configs configs
#' @return Derivatives Analysis
#' @export
mod_Derivatives_ui <- function(id, configs){
  ns <- NS(id)
  # par_slct_witdth = "25%"

  tagList(

    conditionalPanel(
      condition = "input.PageViewMenu == 'Options'", ns = ns,
      mod_Derivatives_Options_ui(id, configs)
    ), # conditionalPanel

    conditionalPanel(
      condition = "input.PageViewMenu == 'Screener'", ns = ns,
      fluidRow(
          style = "font-size:80%;",
          column(12,
                 mod_Derivatives_Screener_ui(id, configs)
                 )
      )
    ) # conditionalPanel
  ) # tagList
} # mod_Derivatives_ui


# ---------------------------------------------------------------------------- #
# All Derivatives Server
# ---------------------------------------------------------------------------- #
#' All server side functions of Derivatives Tab
#'
#' @param id NS(id)
#' @param up_input server side inputs
#' @param .e .e env
#' @return Derivatives server
#' @export
mod_Derivatives_server <- function(id, up_input, .e = .e) {
  moduleServer(
    # Setting the id
    id,
    # Defining the module core mechanism
    function(input, output, session) {

      # ...................................................................... #
      mod_Derivatives_Options_server(input, output, session, id, up_input,.e = .e)

      # ...................................................................... #

      mod_Derivatives_Screener_server(input, output, session, id, up_input, .e = .e)
      # ...................................................................... #

      output$dropdown_btn <- renderUI({

        if(!is.null(input$PageViewMenu)){
          if(input$PageViewMenu == 'Options'){
            Options_dropdownButton_func(id, .e$cOptions_obj$configs, .e =.e)

          } else if (input$PageViewMenu == 'Screener'){

            options_screener_dropdownButton_ui_func(id, configs, .e=.e)

          } # if input$PageViewMenu
        } # not null
      })

      # .......................................................................... #

    } # function
  ) # moduleServer
} # mod_Derivatives_server
