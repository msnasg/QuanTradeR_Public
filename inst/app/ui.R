#' Title Shiny App ui
#'
#' @return ui
#' @export
#'
#' @examples
ui_main <<- function(){

  tagList(
    # Settings
    shinyjs::useShinyjs(),
    shinybusy::add_busy_spinner(spin = "fading-circle", position = "bottom-right"),
    tags$head(includeCSS(path =
                           ifelse(Deployflag, paste0("./www/css/styles.css"),
                                  system.file("app/www/css", "styles.css", package = pkgload::pkg_name())) )
    ),
    # Main Page
    bs4Dash::bs4DashPage(
      title = "QuanTradeR",
      # -------------------------------------------------------------------- #
      # navigation bar
      # -------------------------------------------------------------------- #
      header = bs4Dash::bs4DashNavbar(
        fixed = FALSE, # Whether to fix the navbar to the top. FALSE by default
        skin = "light",
        status = "white",
        sidebarIcon = shiny::icon(configs$icons$bars, verify_fa = FALSE),
        controlbarIcon = shiny::icon(configs$icons$gear, verify_fa = FALSE),
        compact = TRUE, # Whether items should be compacted. FALSE by default.
        # titleWidth = 200,
        title = mod_dashnavbartitle_ui("dashnavbartitle_id"),
        # .................................................................. #
        # Right UI
        rightUi = tagList(
          bs4Dash::dropdownMenu(
            type = c("messages", "notifications", "tasks")[1],
            badgeStatus = "secondary", icon = shiny::icon(configs$icons$refresh,
                                                          verify_fa = FALSE)#,
            #headerText = "DataSet"
          )
        ),
        # .................................................................. #
        # left UI
        leftUi = NULL,
        # .................................................................. #
        # header navbar Menu
        bs4Dash::navbarMenu(
          id = "navmenu",
             actionLink(inputId = "navbartab_data_btn",
                     icon = shiny::icon("database", verify_fa = FALSE),
                     label = "Data",
                     style = "margin-top:5px;"),

          bs4Dash::navbarTab(
            tabName = "navbartab_about",
            text = "About" 
          )
        ) # navbarMenu
      ), # bs4DashNavbar
      # -------------------------------------------------------------------- #
      #  controlbar
      # -------------------------------------------------------------------- #
      controlbar =  bs4Dash::dashboardControlbar(
        id = "id_controlbar",
        width = 250,
        collapsed = TRUE,
        overlay = TRUE,
        skin = "light",
        bs4Dash::controlbarMenu(
          id = "menu",
          bs4Dash::controlbarItem(
            icon = shiny::icon(configs$icons$gear, verify_fa = FALSE),
            paste0("Setting"),
            paste0("Welcome to Setting")
          ),
          bs4Dash::controlbarItem(
            icon = shiny::icon(configs$icons$circle_info, verify_fa = FALSE),
            paste0("Help"),
            paste0("Welcome to Help")
          )
        )
      ),
      # -------------------------------------------------------------------- #
      # left main sidebar
      # -------------------------------------------------------------------- #
      sidebar = bs4Dash::bs4DashSidebar(
        id = "id_sidebar",
        skin = "light",
        collapsed = TRUE,
        expandOnHover = TRUE,
        status = "white",
        minified = TRUE, # If FALSE, sidebar is hidden
        # .................................................................. #
        bs4Dash::bs4SidebarMenu(
          id = "mainsidebar_id",
          childIndent = TRUE,
          # ................................................................ #
          # Tab Home
          bs4Dash::bs4SidebarMenuItem(
            text = "Home",
            tabName = "mainsidebar_id_home",
            icon = shiny::icon(configs$icons$home, verify_fa = FALSE) ,
            selected = F
          ),
          # ................................................................ #
          # Tab Market Scanner
          bs4Dash::bs4SidebarMenuItem(
            text = "Market Scanner",
            selected = F,
            startExpanded = F,
            tabName = "Market_Scanner",
            icon = shiny::icon(configs$icons$list_check,
                               verify_fa = FALSE)
          ),
          # ................................................................ #
          # Tab Market Timing
          bs4Dash::bs4SidebarMenuItem(
            text = "Market Timing",
            tabName = "Market_Timing",
            icon = shiny::icon(configs$icons$clock, verify_fa = FALSE) ,
            selected = F
          ),
          # ................................................................ #
          # Tab AI-Driven Analysis
          bs4Dash::bs4SidebarMenuItem(
            text = "Financial Metrics",
            tabName = "Financial_Metrics",
            icon = shiny::icon(configs$icons$chart_line, verify_fa = FALSE) ,
            selected = F
          ),
          # ................................................................ #
          # Tab Portfolio Management
          bs4Dash::bs4SidebarMenuItem(
            text = "Portfolio Management",
            tabName = "Portfolio_Management",
            icon = shiny::icon(configs$icons$yin_yang, verify_fa = FALSE) ,
            selected = T
          ),
          # ................................................................ #
          # Tab Portfolio Management
          bs4Dash::bs4SidebarMenuItem(
            text = "Risk Management",
            tabName = "Risk_Management",
            icon = shiny::icon(configs$icons$check, verify_fa = FALSE) ,
            selected = F
          ),
          # ................................................................ #
          # Tab Trading Strategy
          bs4Dash::bs4SidebarMenuItem(
            text = "Trading Strategy",
            tabName = "Trading_Strategy",
            icon = shiny::icon(configs$icons$chess, verify_fa = FALSE) ,
            selected = F
          ),
          # ................................................................ #
          # Tab AI-Driven Analysis
          bs4Dash::bs4SidebarMenuItem(
            text = "AI-Driven Analysis",
            tabName = "AI-Driven_Analysis",
            icon = shiny::icon(configs$icons$robot, verify_fa = FALSE) ,
            selected = F
          ),
          # ................................................................ #
          # Tab Derivatives
          bs4Dash::bs4SidebarMenuItem(
            text = "Derivatives",
            tabName = "Derivatives",
            icon = shiny::icon(configs$icons$money_bill_trend_up, verify_fa = FALSE) ,
            selected = F
          ),
          # ................................................................ #
          # Tab Alternative Investment
          bs4Dash::bs4SidebarMenuItem(
            text = "Alternative Investment",
            tabName = "Alternative_Investment",
            icon = shiny::icon(configs$icons$puzzle_piece, verify_fa = FALSE) ,
            selected = F
          )
        ) # bs4SidebarMenu
      ),
      # -------------------------------------------------------------------- #
      # main body
      # -------------------------------------------------------------------- #
      body = bs4Dash::bs4DashBody(
        fresh::use_theme(mytheme),
        shinyjs::useShinyjs(),
        bs4Dash::bs4TabItems(
          # ................................................................ #
          # Header Tabs
          # ................................................................ #
          bs4Dash::bs4TabItem(
            tabName = "navbartab_about",
            p("navbartab_about")
          ),
          bs4Dash::bs4TabItem(
            tabName = "navbartab_data_save",
            p("navbartab_data_save")
          ),
          bs4Dash::bs4TabItem(
            tabName = "navbartab_data",
            p("navbartab_data")
          ),
          #
          # ................................................................ #
          # Home Main Sidebar Tab
          # ................................................................ #
          bs4Dash::bs4TabItem(
            tabName = "mainsidebar_id_home",
            # mod_welcome_ui("welcome_ui_1")
            p("Home")
          ),
          # ................................................................ #
          # Market_Scanner Main Sidebar Tab
          # ................................................................ #
          bs4Dash::bs4TabItem(
            tabName = "Market_Scanner",
            mod_MainBox_ui(id = "mainsidebar_id_marketscanner",
                           params = list(title = "Market Scanner"),
                           mainui = mod_MarketScanner_ui(id = "mainsidebar_id_marketscanner", .e=.e)
            )

          ),
          # ................................................................ #
          # Market_Timing Main Sidebar Tab
          # ................................................................ #
          bs4Dash::bs4TabItem(
            tabName = "Market_Timing",
            mod_MainBox_ui(id = "mainsidebar_id_marketiming_mainbox",
                           params = list(title = "Market Timing"),
                           mainui = fluidRow(p("Market_Timing"))
            )
          ),
          # ................................................................ #
          # Financial_Metrics Main Sidebar Tab
          # ................................................................ #
          bs4Dash::bs4TabItem(
            tabName = "Financial_Metrics",
            mod_MainBox_ui(id = "mainsidebar_id_financialmetrics_mainbox",
                           params = list(title = "Financial Metrics"),
                           mainui = fluidRow(p("Financial_Metrics"))
            )
          ),
          # ................................................................ #
          # Portfolio_Management Main Sidebar Tab
          # ................................................................ #
          bs4Dash::bs4TabItem(
            tabName = "Portfolio_Management",
            mod_MainBox_ui(id = "mainsidebar_id_portfolio",
                           params = list(title = "Portfolio Management"),
                           mainui = mod_Portfolio_ui(
                             id = "mainsidebar_id_portfolio",
                             configs,
                             .e = .e)
            )
          ),
          # ................................................................ #
          # Risk_Management Main Sidebar Tab
          # ................................................................ #
          bs4Dash::bs4TabItem(
            tabName = "Risk_Management",
            mod_MainBox_ui(id = "mainsidebar_id_riskmanagement_mainbox",
                           params = list(title = "Risk Management"),
                           mainui = fluidRow(p("Risk_Management"))
            )
          ),
          # ................................................................ #
          # Trading_Strategy Main Sidebar Tab
          # ................................................................ #
          bs4Dash::bs4TabItem(
            tabName = "Trading_Strategy",
            mod_MainBox_ui(id = "mainsidebar_id_tradingstrategy_mainbox",
                           params = list(title = "PTrading Strategy"),
                           mainui = fluidRow(p("Trading_Strategy"))
            )
          ),
          # ................................................................ #
          # AI-Driven_Analysis Main Sidebar Tab
          # ................................................................ #
          bs4Dash::bs4TabItem(
            tabName = "AI-Driven_Analysis",
            mod_MainBox_ui(id = "mainsidebar_id_aidrivenanalysis_mainbox",
                           params = list(title = "AI-Driven Analysis"),
                           mainui = fluidRow(p("AI-Driven_Analysis"))
            )
          ),
          # ................................................................ #
          # Derivatives Main Sidebar Tab
          # ................................................................ #
          bs4Dash::bs4TabItem(
            tabName = "Derivatives",
            mod_MainBox_ui(id = "mainsidebar_id_derivatives",
                           params = list(title = "Derivatives"),
                           mainui = mod_Derivatives_ui(id = "mainsidebar_id_derivatives",
                                                       configs)
            )
          ),
          # ................................................................ #
          # AAlternative_Investment Main Sidebar Tab
          # ................................................................ #
          bs4Dash::bs4TabItem(
            tabName = "Alternative_Investment",
            mod_MainBox_ui(id = "mainsidebar_id_alternativeinvestment_mainbox",
                           params = list(title = "Alternative Investment"),
                           mainui = fluidRow(p("Alternative_Investment"))
            )
          )
        ) # bs4TabItems
      ), # bs4DashBody
      # -------------------------------------------------------------------- #
      # footer
      # -------------------------------------------------------------------- #
      footer = tags$footer(
        class = "main-footer",
        fluidRow(
          column(4,
                 style = "margin-left:5px;",
                 actionLink("btn_indicator_updates",
                            label = "QuanTradeR App | Developed by: Mohsen Asgari | Version: 0.1",
                            style = "color: #fff;"
                 )
          ),
          column(
            3,
            actionLink("ContactUs",
                       label = "Contact Us!",
                       icon = icon("at", verify_fa = FALSE), style = "color: #fff;"
            )
          ),
          column(
            2,
            tags$a(
              href = "https://www.google.com", "Privacy & cookies",
              class = "externallink", style = "color: #fff; text-decoration: none"
            )
          ),
          column(
            1,
            HTML(paste0(format(Sys.time(), "%d - %b - %Y"))) # (%a)
          )
        ),
        style = "position:fixed;
          /*text-align:center; */
          left: 0;
          bottom: 0;
          width:100%;
          z-index:1000;
          height:30px; /*  Height of the footer */
          color: white ;
          padding: 5px;
          font-size: 10px;
          /*  font-weight: bold;  */
          background-color: #808080; "
      ) # footer
      # -------------------------------------------------------------------- #
    ) # bs4DashPage
) # main ui
}

# ---------------------------------------------------------------------------- #
#' extra UI
#'
#' @param .e .e env
#' @return ui
#' @export
ui_Func <- function(.e = .e){
  # https://datastorm-open.github.io/shinymanager/reference/secure-app.html
  shinymanager::secure_app(
    ui = ui_main(),
    language = "en", # "de"
    # choose_language = FALSE,
    fab_position = "bottom-left", # "bottom-right", "none"
    status = "primary",
    # https://developer.mozilla.org/fr/docs/Web/CSS/background
    # pic from https://www.freepik.com/
    background = paste0(
      "no-repeat center url('",
      base64enc::dataURI(
        file = ifelse(.e$Deployflag, paste0("./www/Main_Background.jpg"),
                      system.file("app/www", "Main_Background.jpg", package = pkgload::pkg_name()) ),
        mime = "image/png"
      ), "');  background-size: cover; "
    ),


    # ------------------------------------------------------------------------ #
    # Main Ui
    # ------------------------------------------------------------------------ #

    # ------------------------------------------------------------------------ #
    # Authentication Page title an footer
    # ------------------------------------------------------------------------ #
    tags_top = tags$div(
      tags$head(tags$style(HTML(".btn-primary {
              color: #ffffff;
              background-color: #Odc5c1;
              border-color: #Odc5c1; }

              .panel-primary {
              border-color: #Odc5c1; }"))),
      tags$img(
        # src = "https://www.r-project.org/logo/Rlogo.png",
        src = base64enc::dataURI(
          file = ifelse(.e$Deployflag, paste0("./www/hex-QuanTradeR.png"),
                        system.file("app/www", "hex-QuanTradeR.png", package = pkgload::pkg_name())
          ),
          mime = "image/png"
        ),
        width = 150
      )
    ), # tags_top

    tags_bottom = tags$div(
      tags$p(
        "Developed by: ",
        tags$a(href = "https://www.linkedin.com/in/mohsen-asgari/", target = "_top", "Mohsen Asgari")
      )
    ) # tags_bottom
    # ------------------------------------------------------------------------ #
  ) # shinymanager::secure_app
} # ui_Func


# ---------------------------------------------------------------------------- #
# to use for Deploy App or run shiny app from Run App btn
# ---------------------------------------------------------------------------- #
# Any Change here: update also ui_Func in Run_ShinyApp.R
suppressWarnings({
ui <- shinymanager::secure_app(
    ui = ui_main(),
    language = "en", # "de"
    # choose_language = FALSE,
    fab_position = "bottom-left", # "bottom-right", "none"
    status = "primary",
    # https://developer.mozilla.org/fr/docs/Web/CSS/background
    # pic from https://www.freepik.com/
    background = paste0(
      "no-repeat center url('",
      base64enc::dataURI(
        file = ifelse(.e$Deployflag, paste0("./www/Main_Background.jpg"),
                      system.file("app/www", "Main_Background.jpg", package = pkgload::pkg_name()) ),
        mime = "image/png"
      ), "');  background-size: cover; "
    ),


    # ------------------------------------------------------------------------ #
    # Authentication Page title an footer
    # ------------------------------------------------------------------------ #
    tags_top = tags$div(
      tags$head(tags$style(HTML(".btn-primary {
              color: #ffffff;
              background-color: #Odc5c1;
              border-color: #Odc5c1; }

              .panel-primary {
              border-color: #Odc5c1; }"))),
      # tags$h3("QuanTradeR", style = "align:center; color:#006729;"),
      tags$img(
        src = base64enc::dataURI(
          file = ifelse(.e$Deployflag, paste0("./www/hex-QuanTradeR.png"),
                        system.file("app/www", "hex-QuanTradeR.png", package = pkgload::pkg_name())
          ),
          mime = "image/png"
        ),
        width = 150
      )
    ), # tags_top

    tags_bottom = tags$div(
      tags$p(
        "Developed by: ",
        tags$a(href = "https://www.linkedin.com/in/mohsen-asgari/", target = "_top", "Mohsen Asgari")
      )
    ) # tags_bottom
    # ------------------------------------------------------------------------ #
  ) # shinymanager::secure_app
})
