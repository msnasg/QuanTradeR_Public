# ---------------------------------------------------------------------------- #
#' All ui and server of Portfolio / Optimization Tab
#'
#' @param id NS(id)
#' @param configs configs
#' @param .e .e env
#' @return Portfolio / Optimization ui
#' @export
mod_Portfolio_Optimization_ui <- function(id, configs, .e = .e){
  ns <- NS(id)
  par_slct_witdth = "25%"

  div(
    style = "height:800px;margin-left:10px;",

    # ...................................................................... #
    # Main Contents
    # ...................................................................... #
    div(style = "display:flex;flex-wrap:wrap;justify-content:space-between;width:102%;
            margin-left:-20px;",
        # ----------------------------- #
        MyBox_ui(id = ns("portfolio_optimization_box_id_1"),
                 title = "Portfolio Optimization",
                 collapsible = FALSE,
                 maximizable = TRUE,
                 width = 12,
                 mainui = div(
                   style = "margin-left:10px;margin-right:10px;font-size:80%;",
                   bs4Dash::tabsetPanel(
                     id = "portfolio_optimization_tabsetPanel",
                     type = "tabs",
                     vertical = F,
                     side = "left",
                     selected	= "Optimization",
                     # PerformanceSummary
                     tabPanel("Summary",
                              plotOutput(outputId = ns("portfolio_optimization_performancesmry_plt"),height = "750px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("Optimization",
                              fluidRow(style = "margin-left:5px;margin-top:5px;",
                                  column(12,align = "center",
                                  # radioButtons(inputId = ns("portfolio_objtype_rdiobtn"),
                                  #              label = "Portfolio Type",
                                  #              choices = c("Efficient", "Highest Return/Risk Ratio",
                                  #                          "Tangency", "Min Risk", "Min Variance",
                                  #                          "Max Return", "Frontier"),
                                  #              selected = "Efficient", inline = T, width = NULL)
                                  shinyWidgets::radioGroupButtons(
                                    inputId = ns("portfolio_objtype_rdiobtn"),
                                    label = NULL,
                                    choices = c("Efficient", "Highest Return/Risk Ratio",
                                                "Tangency", "Min Risk", "Min Variance",
                                                "Max Return"), # "Frontier"
                                    selected = "Efficient",
                                    checkIcon = list(
                                      yes = tags$i(class = "fa fa-check-square",
                                                   style = "color: steelblue"),
                                      no = tags$i(class = "fa fa-square-o",
                                                  style = "color: steelblue"))
                                  )
                                ),
                                column(6,
                                       div(style = "height:700px; overflow-y:auto; font-size:14px;",
                                             verbatimTextOutput(outputId = ns("portfolio_optimization_txt") ) %>%
                                               shinycssloaders::withSpinner(color="#fafafa")
                                           )
                                       ),
                                column(6,
                                       plotOutput(outputId = ns("portfolio_optimization_barplot"),
                                                  height = "700px") %>%
                                         shinycssloaders::withSpinner(color = "#fafafa")
                                )
                              )
                              ),
                     tabPanel("Frontier",
                              plotOutput(outputId = ns("portfolio_optimization_frontier_plt"),
                                         height = "750px") %>%
                                shinycssloaders::withSpinner(color = "#fafafa"),
                              br(),
                              plotOutput(outputId = ns("portfolio_optimization_frontier_riskmeasures_plt"),
                                         height = "400px") %>%
                                shinycssloaders::withSpinner(color = "#fafafa")
                     ),
                     tabPanel("Weights",
                              plotOutput(outputId = ns("portfolio_optimization_weights_plt"), height = "750px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("Backtest",
                              plotOutput(outputId = ns("portfolio_optimization_backtest_plt"), height = "750px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     ),
                     tabPanel("Risk",
                              plotOutput(outputId = ns("portfolio_optimization_backtestrisk_plt"), height = "750px") %>%
                                shinycssloaders::withSpinner(color="#fafafa")
                     )

                   )
                 ) # mainui div
        ) # MyBox_ui
    ) # div 2
  ) # div 1
} # mod_Portfolio_Optimization_ui

# ---------------------------------------------------------------------------- #
# Server
# ---------------------------------------------------------------------------- #
#' All server side functions of Portfolio / Optimization Tab
#'
#' @param input input
#' @param output output
#' @param session session
#' @param id NS(id)
#' @param .e .e env
#' @param up_input server side inputs
#' @return Portfolio / Optimization Analysis
#' @export
mod_Portfolio_Optimization_server <- function(input, output, session,
                                            id, up_input, .e = .e){
  # --------------------- #
  Port_TS <- reactive({
    # create price and return series of selected portfolio or custom
    shiny::req(input$portfolio_dataset_pkr)
    #Pr = .e$exampleDS[[input$portfolio_dataset_pkr]]
    #Ret = .e$exampleDS[[paste0(input$portfolio_dataset_pkr,".RET")]]
    # Local
    Pr = .e$data$pr
    Ret = .e$data$ret
    return(list(Pr = Pr, Ret = Ret))
  })
  # --------------------- #
  Port_Params <- reactive({
    # shiny::req(input$portfolio_dataset_pkr)
    params = list(type = input$portfolio_modeltype_rdiobtn,
                  optimize = input$portfolio_optimize_rdiobtn,
                  estimator = input$portfolio_estimator_pkr,
                  alpha = input$portfolio_alpha_numinput,
                  riskFreeRate = input$portfolio_riskFreeRate_numinput,
                  nFrontierPoints = input$portfolio_nFrontierPoints_numinput,
                  solver = input$portfolio_solver_pkr)
    return(params)
  })
  # --------------------- #
  Other_Constraints = reactive({
    l = list(
      input$portfolio_constraints_minW_numinput,
      input$portfolio_constraints_maxW_numinput,
      input$portfolio_constraints_minsumW_numinput,
      input$portfolio_constraints_maxsumW_numinput,
      input$portfolio_constraints_minB_numinput,
      input$portfolio_constraints_maxB_numinput
    )
    names(l) <- c('minW','maxW','minsumW','maxsumW','minB','maxB')
    return(l)
  })

  # --------------------- #
  Port_obj <- reactive({
    fspec <- .e$cPortfo_obj$portfolio_fspecs(ts = Port_TS()$Ret, specstype = "NotDefault",
                                             params = Port_Params())
    fdata <- .e$cPortfo_obj$portfolio_fdata(ts = Port_TS()$Ret, fspec)
    # constraints <- "LongOnly" # from function portfolio_constraints

    constraints <-  .e$cPortfo_obj$portfolio_constraints(ts = Port_TS()$Ret,
                                                         type = input$portfolio_constraints_pkr,
                                                         cnstL = Other_Constraints() )

    if(input$portfolio_optimize_rdiobtn == "maxReturn"){
      fPortfolio::setTargetRisk(fspec) <- input$portfolio_getTargetRisk_numinput
    }
    obj <- .e$cPortfo_obj$portfolio_obj(fdata, fspec, constraints,
                                 objtype = input$portfolio_objtype_rdiobtn)
    return(obj)
  })


  anyparamschange <- reactive({
    list(input$portfolio_dataset_pkr ,
         input$portfolio_modeltype_rdiobtn,
         input$portfolio_optimize_rdiobtn,
         input$portfolio_estimator_pkr,
         input$portfolio_alpha_numinput,
         input$portfolio_riskFreeRate_numinput,
         input$portfolio_nFrontierPoints_numinput,
         input$portfolio_solver_pkr,
         input$portfolio_constraints_pkr,
         input$portfolio_constraints_minW_numinput,
         input$portfolio_constraints_maxW_numinput,
         input$portfolio_constraints_minsumW_numinput,
         input$portfolio_constraints_maxsumW_numinput,
         input$portfolio_constraints_minB_numinput,
         input$portfolio_constraints_maxB_numinput,
         input$portfolio_objtype_rdiobtn,
         input$portfolio_rebalanceon_rdiobtn)
  })
  # ...................................................................... #
  # Refresh btn refresh_btn
  # ...................................................................... #
  observeEvent(input$refresh_btn, {

    if(!is.null(input$PageViewMenu)){
      if(input$PageViewMenu == 'Optimization'){

        if(is.null(input$portfolio_dataset_pkr)){
          showModal(modalDialog(
            size = "s", easyClose = T, fade = TRUE,
            title = "Warning !!!",
            "First set the portfolio optimization specification",
            footer = NULL
          ))
        }else{

          output$portfolio_optimization_performancesmry_plt <- renderCachedPlot(
            res = 90, cache = "session",
            cacheKeyExpr = { anyparamschange()  },  {
              # obj <<- Port_obj()
              .e$cPortfo_obj$portfolio_weighted_return_summaryplot(obj = Port_obj(),
                                                                   rebalance_on = input$portfolio_rebalanceon_rdiobtn)
            })

          # ---------------- #
          output$portfolio_optimization_frontier_plt <- renderCachedPlot(
            res = 90, cache = "session",
            cacheKeyExpr = { anyparamschange()  },  {
              print(as.data.frame(Port_Params()))
             .e$cPortfo_obj$portfolio_frontier(ts = Port_TS()$Ret,
                                                specstype = "Opt",
                                                params = Port_Params(),
                                               cnsttype = input$portfolio_constraints_pkr,
                                               cnstL = Other_Constraints() )
            })
          # ---------------- #
          output$portfolio_optimization_frontier_riskmeasures_plt <- renderPlot({
            .e$cPortfo_obj$portfolio_frontier_simple(ts = Port_TS()$Ret)
          })

          # ---------------- #
          output$portfolio_optimization_weights_plt <- renderCachedPlot(
            res = 90, cache = "session",
            cacheKeyExpr = {  anyparamschange()  },  {

              .e$cPortfo_obj$portfolio_weights_plots(ts = Port_TS()$Ret,
                                                specstype = "Opt",
                                                params = Port_Params(),
                                                cnsttype = input$portfolio_constraints_pkr,
                                                cnstL = Other_Constraints())
            })
          # ---------------- #
          output$portfolio_optimization_txt <- renderPrint({
            # obj <<- Port_obj()
            Port_obj()
          })
          # ---------------- #
          output$portfolio_optimization_barplot <- renderCachedPlot(
            res = 100, cache = "session",
            cacheKeyExpr = { anyparamschange() },  {
              .e$cPortfo_obj$portfolio_weights_bar( obj = Port_obj() )
              })
          # ---------------- #
          output$portfolio_optimization_backtest_plt <- renderCachedPlot(
            res = 100, cache = "session",
            cacheKeyExpr = { anyparamschange() },  {
              .e$cPortfo_obj$portfolio_backtest_plot(  )
            })
          # ---------------- #
          output$portfolio_optimization_backtestrisk_plt <- renderCachedPlot(
            res = 100, cache = "session",
            cacheKeyExpr = { anyparamschange() },  {
              .e$cPortfo_obj$portfolio_backtest_risk_plot( )
            })
          # ---------------- #
        } # else
      } # Optimization
    } # PageViewMenu
  }) # refresh_btn

} # mod_Portfolio_Optimization_server

# ---------------------------------------------------------------------------- #
# Parameters of Portfolio_Optimization_dropdownButton_func Page
# ---------------------------------------------------------------------------- #
Portfolio_Optimization_dropdownButton_func <- function(id, configs = .e$configs,
                                                     .e = .e){
  ns <- NS(id)
  par_slct_witdth = NULL
  fluidRow(style = "font-size:10px;",
           column(12,
                  tags$h6("Portfolio Optimization Specification / Parameters", style = "font-weight:bold;")
           ),
           column(12,
                  shinyWidgets::pickerInput(inputId = ns("portfolio_dataset_pkr"),
                                            label = "Select Portfolio",
                                            #p("Select Portfolio", style = "font-weight:bold;"),
                                            # choices = grep(".RET", names(.e$exampleDS),
                                            #                value = T, invert = T),
                                            # selected = "SWX"
                                            choices = .e$data$symbol,
                                            selected = .e$data$symbol[1])
           ),

           # slot model: type
           column(4,
                  radioButtons(inputId = ns("portfolio_modeltype_rdiobtn"),
                               label = p("Model type", style = "font-weight:bold;"),
                               # choices = c("MV", "CVaR", "QLPM", "SPS","MAD"),
                               choices = list("MV: mean-variance (Markowitz) portfolio" = "MV",
                                              "CVaR: mean-conditional Value at Risk portfolio" = "CVaR",
                                              "QLPM: mean quadratic lower partial moment portfolio" = "QLPM",
                                              "SPS: Stone, Pedersen and Satchell type portfolios" = "SPS",
                                              "MAD: mean-absolute-deviance Portfolio" = "MAD" ),
                               selected = "MV", inline = F, width = NULL)
                  ),

           # slot model: estimator
           # column(4,
           #        shinyWidgets::pickerInput(inputId = ns("portfolio_estimator_pkr"),
           #                                  label = "Model estimator",
           #                                  # choices = c("covEstimator", # Covariance sample estimator
           #                                  #             "kendallEstimator", # Kendall's rank estimator
           #                                  #             "spearmanEstimator", # Spearman's rank estimator
           #                                  #             "mcdEstimator", # Minimum covariance determinant estimator
           #                                  #             "mveEstimator", # Minimum volume ellipsoid estimator
           #                                  #             "covMcdEstimator", # Minimum covariance determinant estimator
           #                                  #             "covOGKEstimator", # Orthogonalized Gnanadesikan-Kettenring
           #                                  #             "shrinkEstimator", # Shrinkage estimator
           #                                  #             "baggedEstimator", # Bagged Estimator
           #                                  #             "nnveEstimator" #  Nearest neighbour variance estimator
           #                                  #             ),
           #                                  choices = list("Covariance sample estimator" = "covEstimator",
           #                                                 "Kendall's rank estimator" = "kendallEstimator",
           #                                                 "Spearman's rank estimator" = "spearmanEstimator",
           #                                                 "Minimum covariance determinant estimator" = "mcdEstimator",
           #                                                 "Minimum volume ellipsoid estimator" = "mveEstimator",
           #                                                 "Minimum covariance determinant estimator" = "covMcdEstimator",
           #                                                 "Orthogonalized Gnanadesikan-Kettenring" = "covOGKEstimator",
           #                                                 "Shrinkage estimator" = "shrinkEstimator",
           #                                                 "Bagged Estimator" = "baggedEstimator",
           #                                                 "Nearest neighbour variance estimator" = "nnveEstimator" ),
           #                                  selected = "covEstimator")
           #
           # ),
           column(4,
                  radioButtons(inputId = ns("portfolio_estimator_pkr"),
                               label = p("Model estimator", style = "font-weight:bold;"),
                               choices = list("Covariance sample estimator" = "covEstimator",
                                              # "Kendall's rank estimator" = "kendallEstimator",
                                              "Spearman's rank estimator" = "spearmanEstimator",
                                              "Minimum covariance determinant estimator" = "mcdEstimator",
                                              "Minimum volume ellipsoid estimator" = "mveEstimator",
                                              "Minimum covariance determinant estimator" = "covMcdEstimator",
                                              "Orthogonalized Gnanadesikan-Kettenring" = "covOGKEstimator",
                                              "Shrinkage estimator" = "shrinkEstimator" #,
                                              #"Bagged Estimator" = "baggedEstimator",
                                              #"Nearest neighbour variance estimator" = "nnveEstimator"
                                              ),
                               selected = "covEstimator", inline = F, width = NULL)
           ),
           column(4,
                  radioButtons(inputId = ns("portfolio_constraints_pkr"),
                               label = p("Constraints", style = "font-weight:bold;"),
                               choices = list("long-only constraints [0,1]" = "LongOnly",
                                              "unlimited short selling, [-Inf,Inf]" = "Short",
                                              "Others" = "Others"
                                              #"lower/upper box bounds [minW,maxw]" = "box",
                                              #"lower/upper group bounds [minsumW,maxsumW]" = "group",
                                              #"lower/upper covariance risk budget bounds [minB,maxB]" = "riskbudget"
                                              ),
                               selected = "LongOnly", inline = F, width = NULL)
                  ),
           # Others Constraints
           column(12,
           conditionalPanel(condition = "input.portfolio_constraints_pkr == 'Others'", ns = ns,
                            fluidRow(
                              column(2,
                                     numericInput(inputId = ns("portfolio_constraints_minW_numinput"),
                                                label = "minW: lower box",
                                                value = 0.01, min = 0, max = 1, step = 0.01 )
                                     ),
                              column(2,
                                   numericInput(inputId = ns("portfolio_constraints_maxW_numinput"),
                                                label = "maxw: upper box",
                                                value = 0.5, min = 0, max = 1, step = 0.01 )
                                   ),
                              column(2,
                                   numericInput(inputId = ns("portfolio_constraints_minsumW_numinput"),
                                                label = "minsumW: lower group",
                                                value = 0.01, min = 0, max = 1, step = 0.01 )
                                   ),
                              column(2,
                                   numericInput(inputId = ns("portfolio_constraints_maxsumW_numinput"),
                                                label = "maxsumW: upper group",
                                                value = 1, min = 0, max = 1, step = 0.01 )
                                   ),
                              column(2,
                                   numericInput(inputId = ns("portfolio_constraints_minB_numinput"),
                                                label = "minB: cov. risk budget",
                                                value = 0.01, min = 0, max = 1, step = 0.01 )
                                   ),
                              column(2,
                                   numericInput(inputId = ns("portfolio_constraints_maxB_numinput"),
                                                label = "maxB: cov.risk budget",
                                                value = 1, min = 0, max = 1, step = 0.01 )
                              )
                            ) # fluidRow
                  )
           ),
           # slot optim: solver
           column(4,
                  shinyWidgets::pickerInput(inputId = ns("portfolio_solver_pkr"),
                                            label = "Model Solver",
                                            choices = c("solveRquadprog", # Rmetrics default QP solver
                                                        "solveRglpk", # Rmetrics default LP solver
                                                        "solveRshortExact", # analytical short selling QP solver
                                                        "solveRipop", # alternative QP solver
                                                        "solveRlpSolveAPI", # alternative LP solver
                                                        "solveRsymphony", # alternative LP solver
                                                        "solveRsocp", # QP solver for quadratic constraints
                                                        "solveRdonlp2" # NL solver for non-linear constraints
                                                        # ... for additional solvers
                                            ),
                                            selected = "solveRquadprog")
           ),
           # slot model: optimize
           column(4,
                  radioButtons(inputId = ns("portfolio_optimize_rdiobtn"),
                               label = "Model objective to optimize",
                               choices = c("minRisk", "maxReturn"), # "objRisk" ?
                               selected = "minRisk", inline = T, width = NULL)

           ),
           column(4,
                  conditionalPanel("input.portfolio_optimize_rdiobtn == 'maxReturn'", ns = ns,
                                   numericInput(inputId = ns("portfolio_getTargetRisk_numinput"),
                                                label = "getTargetRisk", # significance level
                                                value = 0.25, min = 0, max = 1, step = 0.01 )
                                   )
                  ),
           # VaR alpha
           column(4,
                  numericInput(inputId = ns("portfolio_alpha_numinput"),
                               label = "VaR alpha", # significance level
                               value = 0.05, min = 0, max = 1, step = 0.01 )
           ),
           # riskFreeRate
           column(4,
                  numericInput(inputId = ns("portfolio_riskFreeRate_numinput"),
                               label = "riskFreeRate", # significance level
                               value = 0, min = 0, max = 100, step = 0.01 )
           ),
           # nFrontierPoints
           column(4,
                  numericInput(inputId = ns("portfolio_nFrontierPoints_numinput"),
                               label = "nFrontierPoints", # significance level
                               value = 50, min = 10, max = 1000, step = 1 )
           ),
           #
           column(6,
                  radioButtons(inputId = ns("portfolio_rebalanceon_rdiobtn"),
                               label = "Rebalance on", # p("Rebalance on", style = "font-weight:bold;"),
                               choices = c("years", "quarters", "months", "weeks", "days"),
                               selected = "weeks", inline = T, width = NULL)
                  # rebalance_on = c(NA, "years", "quarters", "months", "weeks", "days")
                  )



  )

} # Portfolio_Optimization_dropdownButton_func

