
suppressWarnings({
  suppressMessages({
    library(shiny)
    library(shinymanager)
    library(shinyWidgets)
    library(fresh)
    library(bs4Dash)
    library(shinyjs)
    library(NestedMenu)
    library(derivmkts)
    library(ggplot2)
    library(htmltools)
    library(DT)
    library(base64enc)
    library(shinybusy)
    library(shinycssloaders)
    library(reactable)
    library(ggpubr)
    library(magrittr)
    library(methods)
    library(reshape2)
    library(stats)
    library(dplyr)
    library(rlang)
    library(alphavantager)
    library(fPortfolio)
    library(factoextra)
    library(Hmisc)
    library(fMultivar)
    library(timeSeries)
    library(stringr)
    require(PerformanceAnalytics)
    library(tableHTML)
    library(Hmisc)
}) })


.e <<- new.env()
.e$Deployflag <- FALSE
Deployflag <- FALSE


.e$var_OptionsPortfolio_df <- data.frame()


if(Deployflag){
  .e$DefaultOptions <- readxl::read_excel(path = paste0("./data/", "default_data.xlsx"),
                                          sheet = "Options")
  .e$Symbols <- readxl::read_excel(path = paste0("./data/", "default_data.xlsx"),
                                          sheet = "Symbols")
  .e$extdatafolder <- "./data/extdata/" # in cETL.R
}else{
  .e$DefaultOptions <- readxl::read_excel(path = system.file("app/data", "default_data.xlsx",
                                                             package = pkgload::pkg_name()),
                                          sheet = "Options")
  .e$Symbols <- readxl::read_excel(path = system.file("app/data", "default_data.xlsx",
                                                             package = pkgload::pkg_name()),
                                          sheet = "Symbols")
  .e$extdatafolder <- "./inst/app/data/extdata/" # in cETL.R
}
# New Upload local Data

.e$APIKEY <- list(AlphaVantage = "...", Quandal = "...")

.e$cOptions_obj <- cOptions(par = data.frame())
.e$cPortfo_obj <- cPortfolio(par = data.frame())
.e$cTS_obj <- cTimeSeries(par = data.frame())
.e$cETL_obj <- cETL()

.e$data$symbol <- NULL # fill in mod_DataSet.R

#----------------------------------------------------------------------------- #
#### configs
#----------------------------------------------------------------------------- #
{
  if(Deployflag){
    config_yml_path <- paste0("./www/yml/config.yml")
  }else{
    config_yml_path <- system.file("app/www/yml", "config.yml", package = pkgload::pkg_name())
  }

  if( !file.exists(config_yml_path) ){
    cat(
      crayon::red(cli::symbol$cross), "No", "config.yml", "in working directory\n"
    )
    return(NULL)
  }

  configs <<- yaml::read_yaml(config_yml_path)

}
.e$configs <- configs

# ---------------------------------------------------------------------------- #
# theme
# ---------------------------------------------------------------------------- #

mytheme <- fresh::create_theme(
  theme = "default",
  fresh::bs4dash_font(
    size_base = "0.7rem",
    weight_bold = 500 # ,
    # family_base = "Arial"
  ),
  fresh::bs4dash_vars(
    navbar_light_color = "#000000", # Three line left side menu high show
    navbar_light_hover_color = "#0000FF", # Three line left side menu high show hover
    navbar_light_active_color = "#000000", # color of text in navbarmenu after click
    navbar_dark_hover_color = "#0000FF",
    navbar_dark_active_color = "#FFFFFF"
  ),

  fresh::bs4dash_layout(
    main_bg = "#fafafa", # "#ffffff" ,
    sidebar_width = '200px'
  ),
  fresh::bs4dash_sidebar_light(
    bg = "#ffffff", #  "#272c30", "#c6c7c8"
    color = "#000000", # "#bec5cb",
    active_color = "#eeeeee" ,
    hover_color = "#000000",
    submenu_bg = "#ffffff",
    submenu_active_bg = NULL,
    hover_bg = "#eeeeee" 
  ),
  fresh::bs4dash_sidebar_dark(
   bg = "#000000",
   color = "#ffffff",
  hover_bg = "#000000",
  active_color = "#558817",
   submenu_bg = "#000000",
  submenu_hover_bg = "#000000",
  submenu_active_color = "#000000",
    submenu_active_bg = NULL #,
  #   header_color = NULL
  ),
  fresh::bs4dash_status(
     danger = "#fafafa",
     info = "#fafafa", # "#c1dba3"  # HDI light blue # e0edd1
     dark = "#000" # ,
     # light = "#fff"
  )
)

# necessary for portfolio_riskreturn_cluster_grouping_plt
.pamSelect <<- function(x, control = NULL, ...) {
  if (is.null(control))
    control = c(k = 2, metric = "euclidean")
  k <- as.integer(control[1])
  metric <- control[2]
  pam(x <- as.matrix(x), k = k, metric = metric, ...)
}
