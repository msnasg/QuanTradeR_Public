# ui_utils
# ---------------------------------------------------------------------------- #
# Get image path from www for Shiny App
# ---------------------------------------------------------------------------- #
get_img_path_func <- function(name, .e = .e){

  if(.e$Deployflag){
    files <- base::normalizePath(list.files(paste0("./www/"),
                                            full.names = TRUE))
  }else{
    files <- base::normalizePath(list.files(system.file(package = pkgload::pkg_name(), "app", "www"),
                                            full.names = TRUE))
  }

  path = files[base::basename(files) %in% c(name)]
  gsub("\\\\","/",path)
}


# ---------------------------------------------------------------------------- #
# Module dash navbar title
# ---------------------------------------------------------------------------- #
mod_dashnavbartitle_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("uiout"))
}

mod_dashnavbartitle_server <- function(id, font){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output[["uiout"]] <- renderUI({

      div( class = "text-center", # tagList(

         h6("QuanTradeR", class = "text-center",
            style = paste0(font, "font-weight:normal; margin-top:20px; margin-bottom:20px;")),
      hr()
      ) # div
    })
  })
} # mod_dashnavbartitle_server

# ---------------------------------------------------------------------------- #
# MyWellPanel_ui
# ---------------------------------------------------------------------------- #
MyWellPanel_ui <- function(id, title = "", width = 12, height = NULL,
                           mainui = NULL) {
  ns <- NS(id)
  column(width = width, style = paste0("height:",height,";"),
         wellPanel(
           style ="border-style:solid; border-color:#fafafa;",
           fluidRow(
             column(12,
                    title
                    ),
             column(12,
              mainui
             )
           )
         )
  )

}
# ---------------------------------------------------------------------------- #
# MyBox_ui
# ---------------------------------------------------------------------------- #
MyBox_ui <- function(id, title = "", width = 12, height = NULL,
                     headerBorder = FALSE,
                     collapsible = FALSE,
                     drpdwnmenuui = NULL,
                     maximizable = FALSE,
                     sidebarui = NULL,
                     mainui = NULL) {
  ns <- NS(id)

  bs4Dash::box(
    title = div(class = "row",
               div(h6(title, style = "font-weight:bold;") )
           ),
    id = id,
    width = width,
    height = height,
    # background = "white",
    collapsible = collapsible,
    closable = F,
    solidHeader = F,
    headerBorder = headerBorder,
    maximizable = maximizable,
    # sidebar
    sidebar = sidebarui,
    # dropdownMenu
    dropdownMenu = drpdwnmenuui,
    # ... Mian contents
    mainui
  )
  
} # MyBox_ui

# ---------------------------------------------------------------------------- #
#
# ---------------------------------------------------------------------------- #
makeDT_Table <- function(df, type = 1, scrollY = "250px", pageLength = 15){

  if(!is.null(df) && nrow(df) >=1){
    if(type == 1){
      dt <- DT::datatable(df, class = "compact",
                          options = list(scrollY = scrollY,
                                         scrollX = TRUE,
                                         pageLength = pageLength,
                                         dom ="tp") ) %>%
        DT::formatStyle(c(0:ncol(df)), color="black", border = '1px solid #ddd')
    } # type 1
    return(dt)
  }
}

