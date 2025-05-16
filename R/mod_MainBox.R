
#' Title mod_MainBox ui & server
#' @description
#' mod_MainBox_ui(id, ...)
#' mod_MainBox_server(id, ...)
#' @param id NS(id)
#' @param params Parameters
#' @param mainui contents of Box / bs4Card
#' @return bs4dash Box
#' @export
mod_MainBox_ui <- function(id, params = NULL, mainui = NULL) {

  ns <- NS(id)
div(style = "display:flex;width:103%;margin-left:-20px;margin-top:-10px;",
  bs4Dash::bs4Card(
    id = ns("card_id"),
    title =
      div(class = "row",
          div(style = "border-color:#eee;border-style:solid;border-width:1px;border-radius:5px;",
            NestedMenu::NestedMenuOutput(ns("PageViewMenu"), height = "auto")
          ),

          div(style = "margin-left:5px;",
            shinyWidgets::dropdown(
              circle = T,
              label = "Setup",
              status = "myClass", # "info",
              margin = "10px",
              size = "md", # "sm",
              icon = icon(configs$icons$sliders, verify_fa = FALSE),
              width = "800px",
              tooltip = FALSE,
                  uiOutput(ns("dropdown_btn"))
            )
          ),

          div(style = "margin-left:5px;",
              actionButton(inputId = ns("refresh_btn"),
                           label = NULL,
                           icon = shiny::icon(configs$icons$refresh, verify_fa = FALSE,
                                              style = "font-size:90%;color:#000000;"),
                           style = "padding:3px;background-color:#ffffff;width:40px;height:30px;
                           border-color:#eee;border-style:solid;
                           border-width:1px;border-radius:5px;") # #fafafa
              ),

          div(style = "margin-left:20px;margin-top:5px;font-size:80%;",
              htmlOutput(ns("card_title_text"))
              )
                ), # div title row
    # ...
    footer = NULL,
    status = "gray-dark",
    solidHeader = FALSE,
    headerBorder = TRUE,
    collapsible = TRUE,
    collapsed = FALSE,
    closable = FALSE,
    maximizable = TRUE,
    icon = NULL,
    gradient = FALSE,
    boxToolSize = "sm",
    elevation = NULL,
    label = NULL,
    width = 12,
    height = "880px",
    ## ... Box dropdownMenu
    dropdownMenu = bs4Dash::boxDropdown(
       icon = shiny::icon("wrench"),
       bs4Dash::boxDropdownItem(p("ssss"), id = "NULL", href = NULL, icon = NULL)
       ),

    # ... Box sidebar
    sidebar = bs4Dash::boxSidebar(
      id = ns("boxSidebar_id"),
      width = 25,
      background = "#f9f9fa",
      startOpen = FALSE,
      icon = shiny::icon("gears", verify_fa = FALSE),
      easyClose = TRUE,
      tagList(
        uiOutput(ns("main_rightsidebar_params"))
        ),
    ),
    # ... Mian contents
    div(style = "margin-top:-5px;",
        mainui
    )
  ) # bs4Card
) # div
} # mod_MainBox_ui

# ---------------------------------------------------------------------------- #
# mod_MainBox_server
# ---------------------------------------------------------------------------- #
mod_MainBox_server <- function(id, up_input, configs) {
  # Calling the module Server function

  moduleServer(
    # Setting the id
    id,
    # Defining the module core mechanism
    function(input, output, session) {

      # ...................................................................... #
      output[["PageViewMenu"]] <- NestedMenu::renderNestedMenu({
        # print(configs$Pages[[up_input$mainsidebar_id]])
        NestedMenu::NestedMenu(
            label = "Views" ,
            trigger = "left", # "hover",
            size = NULL, # "sm", # NULL (normal)
            items = configs$Pages[[up_input$mainsidebar_id]],
            style = "info"
          )
      })

      # ...................................................................... #
      observeEvent(up_input$mainsidebar_id,{

        output$card_title_text <- renderText({

          if(up_input$mainsidebar_id == "Market_Scanner"){

            HTML(paste("<b>",
            input$PageViewMenu, "</b>" ))
          } else {
            HTML(paste("<b>", ifelse(is.null(input$PageViewMenu),"-",
              configs$Pages[[up_input$mainsidebar_id]][[input$PageViewMenu]][["pathname"]]), "</b>" ))
            }
          })


      # ...................................................................... #

      }) # observeEvent(up_input$mainsidebar_id
      # ...................................................................... #

    }
  ) # moduleServer

} # mod_MainBox_server
