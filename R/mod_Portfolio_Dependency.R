
# ---------------------------------------------------------------------------- #
#' All ui and server of Portfolio / Dependency Structure Tab
#'
#' @param id NS(id)
#' @param configs configs
#' @return Portfolio / Dependency Structure ui
#' @export
mod_Portfolio_Dependency_ui <- function(id, configs){
  ns <- NS(id)
  par_slct_witdth = "25%"

  div(
    style = "height:800px;margin-left:10px;",

    # ...................................................................... #
    # Main Contents
    # ...................................................................... #
    div(style = "display:flex;flex-wrap:wrap;justify-content:space-between;width:102%;
            margin-left:-20px;",

    ) # div 2
  ) # div 1
} # mod_Portfolio_Dependency_ui

# ---------------------------------------------------------------------------- #
# Server
# ---------------------------------------------------------------------------- #
#' All server side functions of Portfolio / Dependency Structure  Tab
#'
#' @param input input
#' @param output output
#' @param session session
#' @param .e .e env
#' @param id NS(id)
#' @param up_input server side inputs
#' @return Portfolio / Dependency Structure  Analysis
#' @export
mod_Portfolio_Dependency_server <- function(input, output, session,
                                            id, up_input, .e = .e){
} # mod_Portfolio_Dependency_server
