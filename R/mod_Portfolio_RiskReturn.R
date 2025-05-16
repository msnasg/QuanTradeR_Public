
#
# ---------------------------------------------------------------------------- #
#' All ui and server of Portfolio / RiskReturn Tab
#'
#' @param id NS(id)
#' @param configs configs
#' @param .e .e env
#' @return Portfolio / RiskReturn ui
#' @export
mod_Portfolio_RiskReturn_ui <- function(id, configs, .e = .e){

  ns <- NS(id)

  par_slct_witdth = "25%"

  div(
    style = "height:800px;margin-left:10px;",

    tags$head(
      tags$script(
        paste0("$(function() {
              $('[data-card-widget=\"maximize\"]').on('click', function() {
                setTimeout(function() {
                  var isMaximized = $('html').hasClass('maximized-card');
                  if (isMaximized) {

                    $('#", NS(id, "portfolio_riskreturn_pricesmry_dt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_price_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_return_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_stars_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_cumulatedreturn_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_drawdown_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_box_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_density_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_qq_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_frontier_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_frontierVar_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_corgram_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_corrplot_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_clustring_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_profile_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_performancesmry_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_relativeperformance_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_regress_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_beta_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_rollingcorrelation_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_weightedreturn_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_captureratios_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_calendarreturns_plt"),"').css('height', '850px');
                    $('#", NS(id, "portfolio_riskreturn_VaRsensitivity_plt"),"').css('height', '850px');

                  } else {
                    $('#", NS(id, "portfolio_riskreturn_price_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_return_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_stars_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_cumulatedreturn_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_drawdown_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_box_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_density_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_qq_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_frontier_plt"),"').css('height', '280px');
                    $('#", NS(id, "portfolio_riskreturn_frontierVar_plt"),"').css('height', '335px');
                    $('#", NS(id, "portfolio_riskreturn_corgram_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_corrplot_plt"),"').css('height', '280px');
                    $('#", NS(id, "portfolio_riskreturn_clustring_plt"),"').css('height', '280px');
                    $('#", NS(id, "portfolio_riskreturn_profile_plt"),"').css('height', '335px');
                    $('#", NS(id, "portfolio_riskreturn_performancesmry_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_relativeperformance_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_regress_plt"),"').css('height', '290px');
                    $('#", NS(id, "portfolio_riskreturn_beta_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_rollingcorrelation_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_weightedreturn_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_captureratios_plt"),"').css('height', '335px');
                    $('#", NS(id, "portfolio_riskreturn_calendarreturns_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_VaRsensitivity_plt"),"').css('height', '325px');
                    $('#", NS(id, "portfolio_riskreturn_pricesmry_dt"),"').css('height', '325px');


                  }
                }, 300);
                $('#", NS(id, "portfolio_riskreturn_price_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_return_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_stars_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_cumulatedreturn_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_drawdown_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_box_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_density_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_qq_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_frontier_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_frontierVar_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_corgram_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_corrplot_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_clustring_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_profile_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_performancesmry_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_relativeperformance_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_regress_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_beta_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_rollingcorrelation_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_weightedreturn_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_captureratios_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_calendarreturns_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_VaRsensitivity_plt"),"').trigger('resize');
                $('#", NS(id, "portfolio_riskreturn_pricesmry_dt"),"').trigger('resize');

              });
            });
            "
      ) )
    ),

    # ...................................................................... #
    # Main Contents
    # ...................................................................... #
    div(style = "display:flex;flex-wrap:wrap;justify-content:space-between;width:102%;
            margin-left:-20px;",
        # ----------------------------- #
        MyBox_ui(id = ns("portfolio_riskreturn_box_id_1"),
                 title = "Assets Summary",
                 collapsible = FALSE,
                 maximizable = TRUE,
                 width = 6,
                 mainui = div(
                   style = "margin-left:10px;margin-right:10px;font-size:90%;",
                   bs4Dash::tabsetPanel(
                     id = "portfolio_riskreturn_table_tabsetPanel",
                     type = "tabs", # "pills",
                     vertical = F,
                     side = "left",
                     selected	= "Covariance",
                     tabPanel("Price",
                              br(),
                              DT::DTOutput(outputId = ns("portfolio_riskreturn_pricesmry_dt"), height = "310px")
                     ),
                     tabPanel("Return",
                              br(),
                              DT::DTOutput(outputId = ns("portfolio_riskreturn_returnsmry_dt"), height = "310px")
                     ),
                     # CalendarReturns
                     tabPanel("CalendarReturns",
                              plotOutput(outputId = ns("portfolio_riskreturn_calendarreturns_plt"),height = "325px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("DrawDowns",
                              br(),
                              DT::DTOutput(outputId = ns("portfolio_riskreturn_drawdownsstats_dt"), height = "310px")
                     ),
                     tabPanel("Covariance",
                              br(),
                              DT::DTOutput(outputId = ns("portfolio_riskreturn_covariance_dt"), height = "310px")
                     ),
                     tabPanel("Portfolio Risk",
                              br(),
                              p("Equal-Weighted Portfolio: ", style = "font-weight:bold;"),
                              DT::DTOutput(outputId = ns("portfolio_riskreturn_portfoliorisks_dt"), height = "280px")
                     ),
                     tabPanel("Fitting",
                              br(),
                              div(style = "height:310px; overflow-y: auto;",
                              verbatimTextOutput(outputId = ns("portfolio_riskreturn_fitting_txt") ) %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                              )
                     )
                   ), # tabsetPanel
                   br()
                 ) # div
        ),
        # ----------------------------- #
        MyBox_ui(id = ns("portfolio_riskreturn_box_id_2"),
                 title = "Price / Return",
                 width = 6,
                 collapsible = FALSE,
                 maximizable = TRUE,
                 mainui = div(
                   style = "margin-left:10px;margin-right:10px;font-size:90%;",
                   bs4Dash::tabsetPanel(
                     id = "portfolio_riskreturn_plot_tabsetPanel",
                     type = "tabs", # "pills",
                     vertical = F,
                     side = "left",
                     selected	= "VaR",
                     # PerformanceSummary
                     tabPanel("Performance",
                              plotOutput(outputId = ns("portfolio_riskreturn_performancesmry_plt"),height = "325px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("Price",
                              plotOutput(outputId = ns("portfolio_riskreturn_price_plt"),height = "325px") %>%
                                shinycssloaders::withSpinner(color="#fafafa") #
                     ),
                     tabPanel("Return",
                              plotOutput(outputId = ns("portfolio_riskreturn_return_plt"),height = "325px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("Moments",
                              plotOutput(outputId = ns("portfolio_riskreturn_stars_plt"),height = "325px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("Cumulated",
                              plotOutput(outputId = ns("portfolio_riskreturn_cumulatedreturn_plt"),height = "325px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("DrawDowns",
                              plotOutput(outputId = ns("portfolio_riskreturn_drawdown_plt"),height = "325px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("Box",
                              plotOutput(outputId = ns("portfolio_riskreturn_box_plt"),height = "325px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("Density",
                              plotOutput(outputId = ns("portfolio_riskreturn_density_plt"),height = "325px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("qq",
                              plotOutput(outputId = ns("portfolio_riskreturn_qq_plt"),height = "325px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("VaR",
                              plotOutput(outputId = ns("portfolio_riskreturn_VaRsensitivity_plt"),height = "325px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     )

                   ), # tabsetPanel
                   br()
                 )
        ),
        # ----------------------------- #
        MyBox_ui(id = ns("portfolio_riskreturn_box_id_3"),
                 title = "Dependency Structure",
                 width = 6,
                 collapsible = FALSE,
                 maximizable = TRUE,
                 mainui = div(
                   style = "margin-left:10px;margin-right:10px;font-size:90%;",
                   bs4Dash::tabsetPanel(
                     id = "portfolio_riskreturn_plot_tabsetPanel",
                     type = "tabs", # "pills",
                     vertical = F,
                     side = "left",
                     selected	= "Clustring",
                     tabPanel("Relative",
                              plotOutput(outputId = ns("portfolio_riskreturn_relativeperformance_plt"),
                                         height = "325px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("Regress",
                              br(),
                            radioButtons(inputId = ns("portfolio_riskreturn_regresfit_rdiobtn"),
                                           label = NULL,
                                           choices = c("loess", "linear", "conditional"),
                                           selected = "conditional",
                                           inline = T, width = NULL),
                                plotOutput(outputId = ns("portfolio_riskreturn_regress_plt"), height = "280px") %>%
                                  shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("CAPM Beta",
                              div(style = "margin-top:5px;",
                                plotOutput(outputId = ns("portfolio_riskreturn_beta_plt"), height = "325px") %>%
                                 shinycssloaders::withSpinner(color="#fafafa")
                              )
                     ),
                     tabPanel("Correlogram",
                              plotOutput(outputId = ns("portfolio_riskreturn_corgram_plt"), height = "325px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("Roll.Corr",
                              plotOutput(outputId = ns("portfolio_riskreturn_rollingcorrelation_plt"),
                                         height = "325px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("Corr.Plot",
                              br(),
                              radioButtons(inputId = ns("portfolio_riskreturn_corrplot_rdiobtn"),
                                           label = NULL, choices = c("pearson","kendall", "spearman",
                                            "circle", "square", "ellipse", "number", "shade", "color", "pie"),
                                           selected = "pearson", inline = T, width = NULL),
                              plotOutput(outputId = ns("portfolio_riskreturn_corrplot_plt"), height = "280px") %>%
                                shinycssloaders::withSpinner(color="#fafafa") # ,height = "325px"
                     ),
                     tabPanel("Clustring",
                              br(),
                              radioButtons(inputId = ns("portfolio_riskreturn_clustring_rdiobtn"),
                                           label = NULL, choices = c("Hierarchical","Kmeans", "EigenValue", "Grouping"),
                                           selected = "Hierarchical", inline = T, width = NULL),

                              plotOutput(outputId = ns("portfolio_riskreturn_clustring_plt"), height = "280px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                    
                              )
                     #
                   ) # tabsetPanel
                 ) # div
        ),
        # ----------------------------- #
        MyBox_ui(id = ns("portfolio_riskreturn_box_id_4"),
                 title = "Portfolio Composition",
                 width = 6,
                 collapsible = FALSE,
                 maximizable = TRUE,
                 mainui = div(
                   style = "margin-left:10px;margin-right:10px;font-size:90%;",
                   bs4Dash::tabsetPanel(
                     id = "portfolio_riskreturn_portfolio_tabsetPanel",
                     type = "tabs", # "pills",
                     vertical = F,
                     side = "left",
                     selected	= "Risk/Return",
                     tabPanel("Ratios",
                              br(),
                              radioButtons(inputId = ns("portfolio_riskreturn_ratios_rdiobtn"),
                                           label = NULL,
                                           choices = c("All", "Factors", "Specific", "Returns","Capture",
                                                       "Downside", "Drawdowns","Info",
                                                       "Arbitrary", "Autocorr", "Corr", "Dist",
                                                       "Moments",  "Variability"), # "Stats",
                                           selected = "Info", inline = T, width = NULL),
                              DT::DTOutput(outputId = ns("portfolio_riskreturn_ratios_dt"), height = "250px")
                     ),
                     tabPanel("Risk/Return",
                              plotOutput(outputId = ns("portfolio_riskreturn_profile_plt"),height = "335px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("Return",
                              plotOutput(outputId = ns("portfolio_riskreturn_weightedreturn_plt"),height = "330px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),

                     tabPanel("Capture",
                              plotOutput(outputId = ns("portfolio_riskreturn_captureratios_plt"),height = "335px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("Frontier",
                              br(),
                              radioButtons(inputId = ns("portfolio_riskreturn_frontierrisktype_rdiobtn"),
                                           label = NULL,
                                           choices = c("All", "Sigma", "Cov", "VaR","CVaR"),
                                           selected = "Sigma", inline = T, width = NULL),
                              plotOutput(outputId = ns("portfolio_riskreturn_frontier_plt"), height = "280px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("FrontierVaR",
                              plotOutput(outputId = ns("portfolio_riskreturn_frontierVar_plt"),height = "335px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     )

                   ) # tabsetPanel
                 ) # div
        ) # MyBox_ui 4

    ) # div 2
  ) # div 1
} # mod_Portfolio_RiskReturn_ui

# ---------------------------------------------------------------------------- #
# Server
# ---------------------------------------------------------------------------- #
#' All server side functions of Portfolio / RiskReturn Tab
#'
#' @param input input
#' @param output output
#' @param session session
#' @param id NS(id)
#' @param up_input server side inputs
#' @param .e .e env
#' @return Portfolio / RiskReturn Analysis
#' @export
mod_Portfolio_RiskReturn_server <- function(input, output, session,
                                           id, up_input, .e = .e){


    Port_TS <- reactive({
      # create price and return series of selected portfolio or custom
      shiny::req(input$portfolio_riskreturn_dataset_pkr)
      Pr = .e$data$pr
      Ret = .e$data$ret
      return(list(Pr = Pr, Ret = Ret))
    })

    # ...................................................................... #

    output$portfolio_riskreturn_weights_ui <- renderUI({
      portfolio_weights_ui (id, .e = .e, items_name = tmp )
    })


  # ...................................................................... #
  # Refresh btn refresh_btn
  # ...................................................................... #
  observeEvent(input$refresh_btn, {

    if(!is.null(input$PageViewMenu)){
      if(input$PageViewMenu == 'RiskReturn'){

        if(is.null(input$portfolio_riskreturn_dataset_pkr)){
          showModal(modalDialog(
            size = "s", easyClose = T, fade = TRUE,
            title = "Warning !!!",
            "First set the portfolio specification",
            footer = NULL
          ))
        }else{

          # ---------------- #
          output$portfolio_riskreturn_performancesmry_plt <- renderCachedPlot(
              res = 70,
              sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
              cache = "session",
              cacheKeyExpr = { list(input$portfolio_riskreturn_assets_pkr) },{
               ts = Port_TS()$Ret
               .e$cTS_obj$performancesummary_plt(ts,
                            symb = input$portfolio_riskreturn_assets_pkr )
          })
          # ---------------- #
          output$portfolio_riskreturn_pricesmry_dt <- DT::renderDT({
          tmp = .e$cTS_obj$basicStats_ts(Port_TS()$Pr)
          if(input[["portfolio_riskreturn_box_id_1"]]$maximized){
            makeDT_Table(tmp, type = 1, scrollY = "700px", pageLength = 50)
          }else{
            makeDT_Table(tmp, type = 1, scrollY = "250px", pageLength = 15)
          }
        }) # portfolio_riskreturn_pricesmry_dt


        # ---------------- # portfolio_riskreturn_calendarreturns_plt
          output$portfolio_riskreturn_calendarreturns_plt <- renderCachedPlot(
            res = 70,
            sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
            cache = "session",
            cacheKeyExpr = { list(input$portfolio_riskreturn_assets_pkr) },{
              .e$cTS_obj$chart_CalendarReturns(df = Port_TS()$Ret,
                                               symbol = input$portfolio_riskreturn_assets_pkr)
            })
        # ---------------- #
        output$portfolio_riskreturn_returnsmry_dt <- DT::renderDT({
          tmp = .e$cTS_obj$basicStats_ts(Port_TS()$Ret)
          if(input[["portfolio_riskreturn_box_id_1"]]$maximized){
            makeDT_Table(tmp, type = 1, scrollY = "700px", pageLength = 50)
          }else{
            makeDT_Table(tmp, type = 1, scrollY = "250px", pageLength = 15)
          }
        }) # portfolio_riskreturn_returnsmry_dt
        # ---------------- #
        output$portfolio_riskreturn_drawdownsstats_dt <- DT::renderDT({
          tmp <- .e$cTS_obj$drawdownsStats_ts(Port_TS()$Ret, i = 1)
          if(input[["portfolio_riskreturn_box_id_1"]]$maximized){
            makeDT_Table(tmp, type = 1, scrollY = "700px", pageLength = 50)
          }else{
            makeDT_Table(tmp, type = 1, scrollY = "250px", pageLength = 15)
          }
        }) # portfolio_riskreturn_drawdownsstats_dt
        # ---------------- #
        output$portfolio_riskreturn_covariance_dt <- DT::renderDT({
          tmp <- .e$cTS_obj$cov_ts(Port_TS()$Ret)
          if(input[["portfolio_riskreturn_box_id_1"]]$maximized){
            makeDT_Table(tmp, type = 1, scrollY = "700px", pageLength = 50)
          }else{
            makeDT_Table(tmp, type = 1, scrollY = "250px", pageLength = 15)
          }
        })
        # ---------------- #
        output$portfolio_riskreturn_portfoliorisks_dt <- DT::renderDT({
          tmp <- .e$cPortfo_obj$equalweight_riskmeasures(Port_TS()$Ret, alpha = 0.05)
          if(input[["portfolio_riskreturn_box_id_1"]]$maximized){
            makeDT_Table(tmp, type = 1, scrollY = "700px", pageLength = 50)
          }else{
            makeDT_Table(tmp, type = 1, scrollY = "200px", pageLength = 15)
          }
        })
        # ---------------- #
        output$portfolio_riskreturn_fitting_txt <- renderPrint({
          fit <- .e$cTS_obj$test_assetsfit(Port_TS()$Ret, method = "st")
          c('model' = fit@model, fit@fit$estimated)
        })
        # ---------------- #
        output$portfolio_riskreturn_price_plt <- renderCachedPlot(
          res = 70,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_dataset_pkr) }, # must be changed ****
          {
              .e$cPortfo_obj$portfolio_lineplot(Port_TS()$Pr, type = "Price")
          })
        # ---------------- #
        output$portfolio_riskreturn_return_plt <- renderCachedPlot(
          res = 70,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_dataset_pkr) },{
            .e$cPortfo_obj$portfolio_returnplot(Port_TS()$Ret)
          })
        # ---------------- #
        output$portfolio_riskreturn_cumulatedreturn_plt <- renderCachedPlot(
          res = 70,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_dataset_pkr) },{
            .e$cPortfo_obj$portfolio_lineplot(Port_TS()$Ret, type = "Cumulated Return")
          })
        # ---------------- #
        output$portfolio_riskreturn_drawdown_plt <- renderCachedPlot(
          res = 70,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_dataset_pkr) },  {
            .e$cPortfo_obj$portfolio_drawdownplot(Port_TS()$Ret)
          })
        # ---------------- #
        output$portfolio_riskreturn_box_plt <- renderCachedPlot(
          res = 70, # height = 600, width = 500,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_dataset_pkr) },  {
            .e$cPortfo_obj$portfolio_boxplot(Port_TS()$Ret)
          })
        # ---------------- #
        output$portfolio_riskreturn_density_plt <- renderCachedPlot(
          res = 70, # height = 600, width = 500,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_dataset_pkr,
                                input$portfolio_riskreturn_assets_pkr) },{
            ts = Port_TS()$Ret
            .e$cPortfo_obj$portfolio_densityplot(ts,
                                                 symb = input$portfolio_riskreturn_assets_pkr )
          })
        # ---------------- #
        output$portfolio_riskreturn_qq_plt <- renderCachedPlot(
          res = 70, # height = 600, width = 500,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_dataset_pkr,
                                input$portfolio_riskreturn_assets_pkr) },{
            ts = Port_TS()$Ret
            .e$cPortfo_obj$portfolio_qqnormplot(ts, symb = input$portfolio_riskreturn_assets_pkr)
          })
        # ---------------- #
        output$portfolio_riskreturn_VaRsensitivity_plt <- renderCachedPlot(
          res = 70,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_dataset_pkr,
                                input$portfolio_riskreturn_assets_pkr) },{
              ts = Port_TS()$Ret
              .e$cTS_obj$chart_VaRSensitivity(ts, symbol = input$portfolio_riskreturn_assets_pkr)
                                })


        # -------------------------------------------------------------------- #
  
        # ---------------- #
        output$portfolio_riskreturn_clustring_plt <- renderCachedPlot(
          res = 70, # height = 600, width = 500,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_clustring_rdiobtn,
                                input$portfolio_riskreturn_dataset_pkr) },  {
            .e$cPortfo_obj$all_clustring(ts = Port_TS()$Ret,
                                         type = input$portfolio_riskreturn_clustring_rdiobtn,
                                         title = input$portfolio_riskreturn_dataset_pkr)
          })

        # ---------------- #
        output$portfolio_riskreturn_stars_plt <- renderCachedPlot(
          res = 70, # height = 600, width = 500,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_dataset_pkr) },  {
            .e$cPortfo_obj$starplot_basicStats(Port_TS()$Ret)
          })
        # ---------------- #
        output$portfolio_riskreturn_relativeperformance_plt <- renderCachedPlot(
          res = 70,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_assets_pkr,
                                input$portfolio_riskreturn_benchmark_pkr) },  {
            .e$cTS_obj$chart_RelativePerformance(df = Port_TS()$Ret,
                                                 symbol = input$portfolio_riskreturn_assets_pkr,
                                                 benchmark_symbol = input$portfolio_riskreturn_benchmark_pkr)
          })
        # ---------------- #
        output$portfolio_riskreturn_regress_plt <- renderCachedPlot(
          res = 70,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_assets_pkr,
                                input$portfolio_riskreturn_benchmark_pkr,
                                input$portfolio_riskreturn_regresfit_rdiobtn) },  {
            .e$cTS_obj$chart_Regression(df = Port_TS()$Ret,
                                     Rf = 0,
                                     symbol = input$portfolio_riskreturn_assets_pkr,
                                     benchmark_symbol = input$portfolio_riskreturn_benchmark_pkr,
                                     fit = input$portfolio_riskreturn_regresfit_rdiobtn)


                                })
        # ---------------- #
        output$portfolio_riskreturn_beta_plt <- renderCachedPlot(
          res = 70,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_assets_pkr,
                                input$portfolio_riskreturn_benchmark_pkr) },  {
            .e$cTS_obj$calc_beta(df = Port_TS()$Ret,
                                 Rf = 0,
                                 symbol = input$portfolio_riskreturn_assets_pkr,
                                 benchmark_symbol = input$portfolio_riskreturn_benchmark_pkr)

          })

        # ---------------- #
        output$portfolio_riskreturn_corgram_plt <- renderCachedPlot(
          res = 70, # height = 600, width = 500,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_dataset_pkr) },  {
            .e$cPortfo_obj$corgram(Port_TS()$Ret)
          })


        # ---------------- #
        output$portfolio_riskreturn_rollingcorrelation_plt <- renderCachedPlot(
          res = 70,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_assets_pkr,
                                input$portfolio_riskreturn_benchmark_pkr) },  {
            .e$cTS_obj$chart_RollingCorrelation(df = Port_TS()$Ret,
                                                symbol = input$portfolio_riskreturn_assets_pkr,
                                                benchmark_symbol = input$portfolio_riskreturn_benchmark_pkr)
          })
        # ---------------- #
        output$portfolio_riskreturn_corrplot_plt <- renderCachedPlot(
          res = 70,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_corrplot_rdiobtn) },  {
            .e$cPortfo_obj$corrplot(Port_TS()$Ret,
                                    method = input$portfolio_riskreturn_corrplot_rdiobtn)
          })

        # ---------------- #
        output$portfolio_riskreturn_frontier_plt <- renderCachedPlot(
          res = 70, # height = 600, width = 500,
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_dataset_pkr,
                                input$portfolio_riskreturn_frontierrisktype_rdiobtn) },  {
            .e$cPortfo_obj$portfolio_frontier(ts = Port_TS()$Ret, specstype = "Default",
                                              type = input$portfolio_riskreturn_frontierrisktype_rdiobtn)
          })
        # ---------------- #

        # ---------------- #
        output$portfolio_riskreturn_frontierVar_plt <- renderCachedPlot(
          res = 70, # height = 600, width = 500,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_dataset_pkr) },  {
            .e$cPortfo_obj$portfolio_frontier_simple(ts = Port_TS()$Ret)
          })
        # ---------------- #
        output$portfolio_riskreturn_ratios_dt <- DT::renderDT({
          shiny::req(input$portfolio_riskreturn_ratios_rdiobtn)
          shiny::req(input$portfolio_riskreturn_assets_pkr)
          shiny::req(input$portfolio_riskreturn_ratios_rdiobtn)
          tmp = .e$cTS_obj$ratio_tables(df = Port_TS()$Ret,
                                        Rf = 0,
                                        symbol = input$portfolio_riskreturn_assets_pkr,
                                        benchmark_symbol = input$portfolio_riskreturn_benchmark_pkr,
                                        whichtable = input$portfolio_riskreturn_ratios_rdiobtn)

          if(input[["portfolio_riskreturn_box_id_4"]]$maximized){
            makeDT_Table(tmp, type = 1, scrollY = "700px", pageLength = 50)
          }else{
            makeDT_Table(tmp, type = 1, scrollY = "180px", pageLength = 10)
          }
        }) # portfolio_riskreturn_ratios_dt

        # ---------------- #
        output$portfolio_riskreturn_profile_plt <- renderCachedPlot(
          res = 70, # height = 600, width = 500,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_dataset_pkr) },  {
            .e$cPortfo_obj$return_risk_bubbleplot(ts = Port_TS()$Ret)
          })
        # ---------------- #
        output$portfolio_riskreturn_weightedreturn_plt <- renderCachedPlot(
          res = 70,
          sizePolicy = sizeGrowthRatio(width = 500, height = 600, growthRate = 1.2),
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_dataset_pkr,
                                input$portfolio_rebalanceon_rdiobtn) },  {
            weights = .e$cPortfo_obj$portfolio_weights(Port_TS()$Ret)
            .e$cPortfo_obj$portfolio_weighted_return_plot(returns = Port_TS()$Ret,
                                                          weights,
                                                          rebalance_on = input$portfolio_rebalanceon_rdiobtn
                                                          )
          })
        # ---------------- #
        output$portfolio_riskreturn_captureratios_plt <- renderCachedPlot(
          res = 70,
          cache = "session",
          cacheKeyExpr = { list(input$portfolio_riskreturn_assets_pkr,
                                input$portfolio_riskreturn_benchmark_pkr) },
          {
            .e$cTS_obj$chart_CaptureRatios(df = Port_TS()$Ret,
                                           symbol = input$portfolio_riskreturn_assets_pkr,
                                           benchmark_symbol = input$portfolio_riskreturn_benchmark_pkr)
            })
        # ---------------- #

        } # else Specfications
      } # if RiskReturn
    }
  }) # observeEvent(input$refresh_btn

} #


# ---------------------------------------------------------------------------- #
# Parameters of Portfolio_RiskReturn_dropdownButton_func Page
# ---------------------------------------------------------------------------- #
Portfolio_RiskReturn_dropdownButton_func <- function(id, configs = .e$configs,
                                                     .e=.e){

  ns <- NS(id)
  par_slct_witdth = NULL
  fluidRow(style = "font-size:10px;",
           column(12,
                  tags$h6("Portfolio RiskReturn Specification", style = "font-weight:bold;")
           ),
           column(4,
                  shinyWidgets::pickerInput(inputId = ns("portfolio_riskreturn_dataset_pkr"),
                                            label = "Select Portfolio",
                                            choices = .e$data$symbol,
                                            selected = .e$data$symbol[1])
           ),
           column(4,
                  shinyWidgets::pickerInput(inputId = ns("portfolio_riskreturn_assets_pkr"),
                                            label = "Securities",
                                            choices = colnames(.e$data$ret),
                                            selected = colnames(.e$data$ret)[1])
           ),
           column(4,
                  shinyWidgets::pickerInput(inputId = ns("portfolio_riskreturn_benchmark_pkr"),
                                            label = "Benchmark",
                                            choices = colnames(.e$data$ret),
                                            selected = colnames(.e$data$ret)[length(colnames(.e$data$ret))])
           ),
           column(12,
                  radioButtons(inputId = ns("portfolio_riskreturn_weightstype_rdiobtn"),
                               label = "Portfolio Weights type",
                               choices = c("Equal Weight", "Custom" ),
                               selected = "Equal Weight", inline = T, width = NULL)
           ),
           column(12,
                  conditionalPanel("input.portfolio_riskreturn_weightstype_rdiobtn == 'Custom'", ns = ns,
                                   div(
                                     uiOutput(ns("portfolio_riskreturn_weights_ui"))
                                     )
                                   )
                  ),
           column(6,
                  radioButtons(inputId = ns("portfolio_rebalanceon_rdiobtn"),
                               label = "Rebalance on", 
                               choices = c("years", "quarters", "months", "weeks", "days"),
                               selected = "weeks", inline = T, width = NULL)

           )

  )

} # Portfolio_RiskReturn_dropdownButton_func



# ---------------------------------------------------------------------------- #
portfolio_weights_ui <- function(id, .e = .e, items_name = c() ){

  # items_name = colnames(SWX)
  ns <- NS(id)
  nAssets = length(items_name)
  w = rep(1/nAssets, times = nAssets)

  inputs <- lapply(1:nAssets, function(i){
    numericInput(inputId = ns(paste0("weight_", items_name[i])),
                 label = paste(items_name[i]), value = w[i],
                 min = 0, max = 1)
  }) # lapply


  if (length(inputs) >= 1) {
    numRows <- ceiling(length(inputs) / 4)
    inputs_list <- split(inputs, rep_len(1:numRows, length(inputs)))
       rows <- lapply(inputs_list, function(row_inputs) {
      div(style = "display:flex;", row_inputs)
    })
    return(tagList(rows))
  } else {
    return(div(style = "display:flex;", inputs))
  }
} # portfolio_weights_ui
