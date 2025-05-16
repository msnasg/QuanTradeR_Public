
# ---------------------------------------------------------------------------- #
#' All ui and server of Derivatives / Options Tab
#'
#' @param id NS(id)
#' @param configs configs
#' @return Derivatives / Options ui
#' @importFrom magrittr %>%
#' @export
mod_Derivatives_Options_ui <- function(id, configs){
  ns <- NS(id)
  par_slct_witdth = "25%"

  div(
    style = "height:800px;margin-left:10px;",

    # ...................................................................... #
    # Main Contents
    # ...................................................................... #
    div(style = "display:flex;flex-wrap:wrap;justify-content:space-between;width:102%;
            margin-left:-20px;",

        # MyBox_ui() # same
        MyWellPanel_ui(id = "driv_options_mian_payoffs",
                       title =
                         div(class = "row",
                             div(style = "margin-left:20px;margin-top:10px;",
                                 tags$b("Risk Profile")
                             ),
                             div(class = "pickerInput_options",
                                 style = "margin-left:10px;margin-top:5px;",
                                 shinyWidgets::pickerInput(
                                   inputId = ns("options_id"),
                                   label = NULL,
                                   choices = NULL,
                                   selected = NULL)
                             )
                         ),
                       width = 4,
                       #height = "230px",
                       mainui = div(
                         style = "margin-left:10px;font-size:80%;",
                         div(
                           plotOutput(outputId = ns("payoffs_ggplot"), height = "290px"
                           ) %>%
                             shinycssloaders::withSpinner(color="#fafafa")
                         )
                       )
        ),
        # ................................................................ #
        MyWellPanel_ui(
          id = "driv_options_box_greeks_dttbl",
          title = # "Black-Scholes Option Portfolio Greeks",
            div(class = "row",
                div(style = "margin-left:20px;margin-top:10px;",
                    tags$b("Black-Scholes Greeks")
                )
            ),
          width = 3,
          # height = "380px",
          mainui = div(
            style = "margin-left:5px;margin-right:5px;font-size:80%;height:310px;",
            # br(),
            DT::DTOutput(ns("options_bsgreeks_dttbl"))
          )
        ),
        # ................................................................ #
        MyWellPanel_ui(
          id = "driv_options_box_greeks_plot",
          title =
            div(class = "row",
                div(style = "margin-left:20px;margin-top:10px;",
                    tags$b("Greeks Plots")
                )
            ),
          width = 5,
          #height = "380px",
          mainui = div(
            plotOutput(outputId = ns("greeks_ggplot"), height = "310px"
            ) %>%
              shinycssloaders::withSpinner(color="#fafafa")
          )
        ),

        # ................................................................ #
        # Sensitivity plots
        MyWellPanel_ui(id = "driv_options_box_sensitivity",
                       title = div(class = "row",
                                   div(style = "margin-left:20px;margin-top:10px;",
                                       tags$b("Sensitivity Analysis")
                                   ),
                                   div(class = "pickerInput_options",
                                       style = "margin-left:10px;margin-top:5px;",
                                       shinyWidgets::pickerInput(
                                         inputId = ns("sensitivity_whichgreek_id"),
                                         label = NULL,
                                         choices = c("Delta", "Gamma"),
                                         selected = "Delta")
                                   ),
                                   div(class = "pickerInput_options",
                                       style = "margin-left:10px;margin-top:5px;",
                                       shinyWidgets::pickerInput(
                                         inputId = ns("sensitivity_whichvariable_id"),
                                         label = NULL,
                                         choices = c("k", "v", "r", "tt", "d"),
                                         selected = "k")
                                   )
                       ),
                       width = 6,
                       # height = "340px",
                       mainui = div(
                         style = "font-size:80%;",
                         plotOutput(outputId = ns("sensitivity_ggplt"),
                                    height = "300px") %>%
                           shinycssloaders::withSpinner(color="#fafafa")
                       )
        ),

        # .................................................................. #

        # ................................................................ #
        MyWellPanel_ui(
          id = "driv_options_box_binomial_plot",
          title = # "Binomial option pricing",
            div(class = "row",
                div(style = "margin-left:20px;margin-top:10px;",
                    tags$b("Binomial Option Pricing")
                )
            ),
          width = 6,
          # height = "380px",
          mainui = div(
            style = "font-size:80%;",
            plotOutput(outputId = ns("binomplot"), height = "320px") %>%
              shinycssloaders::withSpinner(color = "#fafafa") #,
          )
        ),

        # ................................................................ #
        # Portfolio Table
        MyWellPanel_ui(id = "driv_options_box_portfolio_dttbl",
                       title = div(class = "row",
                                   div(style = "margin-left:20px;margin-top:10px;",
                                       tags$b("Portfolio")
                                   )
                       ),
                       width = 12,
                       # height = "340px",
                       mainui = div(
                         style = "margin-left:5px;margin-right:5px;margin-top:5px;
                       font-size:80%;",
                         DT::DTOutput(ns("options_portfolio_dttbl"), height = "315px")
                       )
        ),
        # .................................................................. #
        # ------ # help
        MyWellPanel_ui(id = "driv_options_boxhelp",
                       title = div(class = "row",
                                   div(style = "margin-left:20px;margin-top:10px;margin-down:10px;",
                                       tags$b("Glossary / Help")
                                   )
                       ),
                       width = 12,
                       mainui =
                         div(style = "width:100%;margin-left:10px;",
                             br(),
                             HTML("<li><b> s / uap:</b> underlying asset Price </li>"),
                             HTML("<li><b> k:</b> Strike price </li>"),
                             HTML("<li><b> v:</b> Volatility of the underlygin asset price, defined as the annualized standard deviation of the continuously-compounded return </li>"),
                             HTML("<li><b> r:</b> Annual continuously-compounded risk-free interest rate </li>"),
                             HTML("<li><b> Implied volatility:</b> The volatility percentage that produces the best fit for all underlying option prices on that underlying stock. </li>"),
                             HTML("<li><b> Delta:</b>	A measure of the rate of change in an option's theoretical value for a one-unit change in the price of the underlying stock. </li>"),
                             HTML("<li><b> Gamma:</b> A measure of the rate of change in an option's Delta for a one-unit change in the price of the underlying stock. </li>"),
                             HTML("<li><b> Theta:</b> A measure of the rate of change in an option's theoretical value for a one-unit change in time to the option's expiration date. </li>"),
                             HTML("<li><b> Time decay:</b> A term used to describe how the theoretical value of an option erodes or reduces with the passage of time. Time decay is specifically quantified by Theta. </li>"),
                             HTML("<li><b> Vega:</b> A measure of the rate of change in an option's theoretical value for a one-unit change in the volatility assumption. </li>"),
                             HTML("<li><b> Vera:</b>	Vera is the rate of change of an option's Rho with respect to its Implied Volatility level. Vera is a second-order Greek. </li>"),
                             HTML("<li><b> Veta:</b> Veta, or Vega decay, is the rate of change of an option's Vega with respect to the passage of time. Veta is a second-order Greek. </li>"),
                             HTML("<li><b> Vonna:</b> Vomma, otherwise known as Vega convexity, is the rate of change of an option's Vega with respect to its Implied Volatility level. Vomma is a second-order Greek. </li>"),
                             HTML("<li><b> Charm:</b> range from -1 to +1, known as <b>Delta decay</b>, is the rate of change of an option's Delta with respect to the passage of time. Charm is a second-order Greek. </li>"),
                             HTML("<li><b> Speed:</b> measures the rate of change of gamma with respect to changes in the underlying asset's price. </li>"),
                             HTML("<li><b> Zomma:</b>	is an option's sensitivity of gamma to changes in implied volatility, where a higher zomma indicates small changes in IV translate into large changes in gamma.</li>"),
                             HTML("<li><b>Vonna / Vomma:</b> known as Vega convexity, is the rate of change of an option's Vega with respect to its Implied Volatility level. Vomma is a second-order Greek. </li>"),
                             HTML("<li><b> Rho:</b> A measure of the expected change in an option's theoretical value for a 1% change in interest rates.</li>"),
                             HTML("<li><b> </b> </li>"),

                             br()
                         )
        ), # MyBox_ui help
        # .................................................................. #
        # ------ # box for test MyBox_ui
        MyWellPanel_ui(id = "test_box",
                       title = div(class = "row",
                                   div(style = "margin-left:20px;margin-top:10px;margin-down:10px;",
                                       tags$b("Expanded Portfolio Table")
                                   )
                       ),
                       width = 12,
                       mainui = div(
                         style = "margin-left:10px;margin-right:10px;font-size:80%;",
                         DT::DTOutput(outputId = ns("test_all")) #,
                         # br()
                       )
        ),
        # .................................................................. #
        MyWellPanel_ui(id = "driv_options_box4",
                       title = div(class = "row",
                                   div(style = "margin-left:20px;margin-top:10px;margin-down:10px;",
                                       tags$b("Options Payoff")
                                   )
                       ),
                       width = 12,
                       mainui = div(
                         p("...")
                       )
        ),
        # ----------------------------- #
        MyBox_ui(id = "driv_options_mian_1",
                 title = "Portfolio Payoffs",
                 width = 6,
                 mainui = div(
                   style = "margin-left:10px;margin-right:10px;
                     font-size:80%;",

                 )
        ),

        MyBox_ui(id = "driv_options_mian_2",
                 title = div(class = "pickerInput_options_greekscharts_pikr",
                             shinyWidgets::pickerInput(
                               inputId = ns("options_greekscharts_pikr2"),
                               label = NULL,
                               choices = c("Implied Volatilities",
                                           "Delta", "Gamma", "Premium",
                                           "Rho", "Theta", "Vega", "Psi",
                                           "Elast"),
                               selected = "Delta",
                               choicesOpt = list(style = rep_len("font-size:90%;",10))
                             )
                 ),
                 width = 6,
                 mainui = div(
                   style = "margin-left:10px;margin-right:10px;
                     font-size:80%;",

                   div(
                     br()
                   )
                 )
        ),
        MyBox_ui(id = "driv_options_mian_3",
                 title = "Tables",
                 width = 6, mainui = div(
                   style = "margin-left:10px;margin-right:10px;
                     font-size:80%;",
                   br()
                 )
        ),

        MyBox_ui(id = "driv_options_mian_4",
                 title = "Binomials Tree",
                 width = 6, mainui = div(
                   style = "margin-left:10px;margin-right:10px;
                     font-size:80%;",
                   br()
                 )
        ),

        MyBox_ui(id = "driv_options_box5", title = "Options Portfolio Payoff Table", width = 12,
                 mainui = div(
                   p("fff")
                 )
        )
        # ---
    )
  ) # mian div box fluidRow

} # mod_Derivatives_Options_ui


# ---------------------------------------------------------------------------- #
# Server
# ---------------------------------------------------------------------------- #
#' All server side functions of Derivatives / Options Tab
#'
#' @param input input
#' @param output output
#' @param session session
#' @param id NS(id)
#' @param .e .e env
#' @param up_input server side inputs
#' @return Derivatives / Options Analysis
#' @importFrom magrittr %>%
#' @export
mod_Derivatives_Options_server <- function(input, output, session,
                                           id, up_input,
                                           .e = .e){

  # .......................................................................... #

  observeEvent(input$options_guide_btn,{
    showModal(modalDialog(
      size = "l", easyClose = T, fade = TRUE,
      title = "Options Startegy Guide",
      options_guide_btn_ui(id, .e = .e),
      footer = NULL
    ))
  })

  observeEvent(input$options_direction, {

    l = names(c(.e$cOptions_obj$configs$pars[-1],
                .e$cOptions_obj$read_strateges_picfolder(input$options_direction) ))
    shinyWidgets::updatePickerInput(session, inputId = "options_strategy",
                      choices = l, selected = l[1])
  })
  # ...................................................................... #
  # main_rightsidebar_params
  output$main_rightsidebar_params <- renderUI({
    if(!is.null(input$PageViewMenu)){
      if(input$PageViewMenu == 'Options'){
        p("Options", style = "color:#000;")

      } # if input$PageViewMenu == 'Options'
      else if(input$PageViewMenu == 'Futures'){
        p("Futures", style = "color:#000;")
      } # if input$PageViewMenu == 'Futures'
    } # not null
  }) # renderUI main_rightsidebar_params

  # ...................................................................... #
  output$Options_commonParameters <- renderUI({
    .e$cOptions_obj$auto_numericInput_func("common",id)
  })

  output$Options_varParameters <- renderUI({
    # print(input$options_strategy)
    .e$cOptions_obj$auto_numericInput_func(input$options_strategy, id)
  })

  showdf_func <- reactive({

    #cat("showdf_func \n")
    inputsL <- list()
    inputsL$Position_Strategy <- paste0(input$options_position, " ", input$options_strategy)

    inputsL$k <- NULL # bellow
    inputsL$pr <- NULL # bellow
    inputsL$tt <- input$options_tt
    inputsL$v <- input$options_v
    inputsL$r <- input$options_r
    inputsL$d <- input$options_d
    inputsL$s_low <- input$options_s_low
    inputsL$s_up <- input$options_s_up
    inputsL$Strategy <- input$options_strategy
    inputsL$Position <- input$options_position

    inputsL$Exercise <- input$options_exercise

    tmpkpr <- .e$cOptions_obj$configs$pars[[input$options_strategy]]
    inputsL$uap <- paste0("[", paste0(tmpkpr[grep("uap", names(tmpkpr))], collapse = ","), "]")

    inputsL$k <- paste0("[", paste0(tmpkpr[grep("k", names(tmpkpr))], collapse = ","), "]")

    inputsL$pr <- paste0("[", paste0(tmpkpr[grep("pr", names(tmpkpr))], collapse = ","), "]")
    inputsL$comment <- paste0("[", paste0(grep("k", names(tmpkpr), value = T),
                                          collapse = ","), "] , [",
                              paste0(grep("pr", names(tmpkpr), value = T), collapse = ","), "]")
    return(.e$cOptions_obj$portfolio_showdf(inputsL))
    # return(.e$DefaultOptions)
  })

  # .......................................................................... #
  # options_defaultportfolio_btn
  # .......................................................................... #
  observeEvent(input$options_defaultportfolio_btn,{
    .e$showdf <- .e$DefaultOptions
    if(!is.null(.e$showdf)){
      shinyWidgets::updatePickerInput(session,
                        inputId = "options_id",
                        choices = unname(unlist(c(.e$showdf[,"id"], "All" ))),  # , "Portfolio", "All"
                        selected = as.character(.e$showdf[1,"id"])
      )
    }
  })

  # .......................................................................... #
  # options_add_btn
  # .......................................................................... #
  observeEvent(input$options_add_btn,{
    .e$showdf <- showdf_func()

    if(!is.null(.e$showdf)){
      shinyWidgets::updatePickerInput(session,
                        inputId = "options_id",
                        choices = unname(unlist(c(.e$showdf[,"id"], "All" ))),  # , "Portfolio", "All"
                        selected = as.character(.e$showdf[1,"id"])
      )

      # confirm msg
      showModal(modalDialog(
        size = "s", easyClose = T, fade = TRUE,
        title = "Done.",
        fluidRow(
          p("Added to the portfolio Table. check the Table. ")
        ),
        footer = NULL
      ))
    }
    })

    # ------------------ # show df
    output$options_portfolio_dttbl <- DT::renderDT({

      req(any(isTruthy(input$options_add_btn), isTruthy(input$options_defaultportfolio_btn)))

      tmp <- .e$showdf
      rownames(tmp) <- NULL
      if(!is.null(tmp) && nrow(tmp) >=1){
        dt <- DT::datatable(tmp,
                            class = "compact",
                            extensions = 'Buttons', # dom with 'B' for btn
                            rownames = FALSE,
                            options = list(scrollY = "240px",
                                           scrollX = TRUE,
                                           columnDefs = list(
                                             list(targets = c(9:(ncol(tmp)-1)), visible = FALSE)
                                             # must be ncol -1 to rownames = FALSE
                                           ),
                                           buttons = list(list(extend = 'colvis',
                                                               text = 'Columns' )),
                                           dom ="Bt") )  %>%
          DT::formatStyle(c(0:ncol(tmp)), color="black", border = '1px solid #eee')
        dt
      }
    })

    # ------------------ # show df
    output$options_greeks_dttbl <- DT::renderDT({
      tmp = data.frame(matrix(0,4,4))
      dt <- DT::datatable(tmp,
                          class = "compact",
                          extensions = 'Buttons', # dom with 'B' for btn
                          rownames = FALSE,
                          options = list(scrollY = "240px",
                                         scrollX = TRUE,
                                         dom ="t") )  %>%
        DT::formatStyle(c(0:ncol(tmp)), color="black", border = '1px solid #eee')
      dt
    })

    # .... # Test Box
    output$test_all <- DT::renderDT({
      # shiny::req(input$options_add_btn)

      req(any(isTruthy(input$options_add_btn),
              isTruthy(input$options_defaultportfolio_btn)))

      if(!is.null(.e$showdf) && nrow(.e$showdf) >= 1){
        tmp <- .e$cOptions_obj$selectoption_df("All", .e$showdf) # input$options_id

        if(!is.null(tmp) && nrow(tmp) >=1){
          dt <- DT::datatable(tmp, class = "compact",
                              options = list(scrollY = "250px",
                                             scrollX = TRUE,
                                             dom ="t") )  %>%
            DT::formatStyle(c(0:ncol(tmp)), color="black", border = '1px solid #ddd')
          dt
        }
      }
    })
  # ...................................................................... #
  # Refresh btn refresh_btn
  # ...................................................................... #
  observeEvent(input$refresh_btn, {

    if(!is.null(input$PageViewMenu)){
      if(input$PageViewMenu == 'Options'){

        output$payoffs_ggplot <- renderCachedPlot(res=70,
                                                  cache = "session",
                                                  cacheKeyExpr = { list(input$options_id) },
                                                  { 
  
                                                    if(!is.null(.e$showdf) && !is.null(input$options_id)){
                                                      .e$cOptions_obj$payoffs_plots(input$options_id, .e$showdf)
                                                    }
                                                  })
        # ................................ #
        output$payoff_ggplot <- renderPlot(res=70,{

        }) # payoff_ggplot

        # ................................ #
        # output$greeks_ggplot <- renderPlot(#res=70,
        output$greeks_ggplot <- renderCachedPlot(
          cache = "session",
          cacheKeyExpr = { list(input$options_id) },
          {
            #cOptions_obj = cOptions(par = data.frame())
            if(!is.null(.e$showdf) && !is.null(input$options_id) && (input$options_id %in% .e$showdf$id)){
              .e$cOptions_obj$greeks_plots(input$options_id, .e$showdf)
            }
          })

        # ................................ #
        # output$sensitivity_ggplot <- renderPlot(res=60,{
        output$sensitivity_ggplt <- renderCachedPlot(
          res=60,
          cache = "session",
          cacheKeyExpr = {
            list(input$options_id, input$sensitivity_whichvariable_id,
                 input$sensitivity_whichgreek_id) },
          {
            #cOptions_obj = cOptions(par = data.frame())
            if(!is.null(.e$showdf) && !is.null(input$options_id) && (input$options_id %in% .e$showdf$id)){
              .e$cOptions_obj$sensitivity_plots(options_id = input$options_id,
                                             showdf = .e$showdf,
                                             var = input$sensitivity_whichvariable_id,
                                             whichgreek = input$sensitivity_whichgreek_id                                             )
            } #  if null
          })
        # ................................ #
        # BS greeks table
        output$options_bsgreeks_dttbl = DT::renderDT({

          #cOptions_obj = cOptions(par = data.frame())
          if(!is.null(.e$showdf) && !is.null(input$options_id) && (input$options_id %in% .e$showdf$id)){

            tmp <- .e$cOptions_obj$greeks_calc_id(options_id = input$options_id,
                                               .e$showdf)

            # print(tmp)
            if(is.null(tmp)){ tmp = data.frame(matrix(NA,5,3)) } # if

            dt <- DT::datatable(tmp, class="compact",
                                options = list(scrollY = "280px",
                                               scrollX = TRUE,
                                               pageLength = 100,
                                               dom ="t") )  %>%
              DT::formatStyle(c(0:ncol(tmp)), color="black", border = '1px solid #ddd')
            dt
          } # if !is.null

        }) # options_bsgreeks_dttbl
        # ................................ #
        # Binomial plot
        output$binomplot <- renderPlot(res=60,{
          # cOptions_obj = cOptions(par = data.frame())
          if(!is.null(.e$showdf) && !is.null(input$options_id) && (input$options_id %in% .e$showdf$id)){
            .e$cOptions_obj$binomplot(input$options_id, .e$showdf,
                                   nstep = input$options_nstep)
          }
        }) # renderPlot
      } # if(input$PageViewMenu == 'Options'){
    } # if(!is.null(input$PageViewMenu))

    # ..................................... #
  }) # observeEvent(input$refresh_btn
} # mod_Derivatives_Options_server

# ---------------------------------------------------------------------------- #
# Parameters of Options Strategy Page
# ---------------------------------------------------------------------------- #
Options_dropdownButton_func <- function(id, configs, .e = .e){
  # no need for configs here as arg
  ns <- NS(id)
  par_slct_witdth = NULL # "25%"

  fluidRow(style = "font-size:10px;",
           column(12,
                  tags$h6("Options Parameters", style = "font-weight:bold;")
           ),
           column(12,
                  div(class = "row",
                      style = "display:flex;flex-wrap:wrap;justify-content:space-between;width:100%;",
                      column(8,
                             shinyWidgets::radioGroupButtons(
                               inputId = ns("options_direction"),
                               label = "Direction",
                               choices = c("Bullish", "Bearish", "Neutral", "Others"),
                               checkIcon = list(
                                 yes = tags$i(class = "fa fa-check-square",
                                              style = "color: steelblue"),
                                 no = tags$i(class = "fa fa-square-o",
                                             style = "color: steelblue"))
                             )
                      ),
                      column(4, style = "margin-top:20px;", align = "right",
                             # code end of the file here
                             actionButton(inputId = ns("options_guide_btn"),
                                          label = "Visual Guide",
                                          icon = shiny::icon(.e$configs$icons$question,
                                                             verify_fa = FALSE) )
                      )
                  )
           ),
           column(12,
                  div(class = "row",
                      style = "display:flex;flex-wrap:wrap;justify-content:space-between;width:100%;",
                      column(4,
                             shinyWidgets::pickerInput(inputId = ns("options_position"),
                                                       label = NULL,
                                                       choices = c("Long", "Short", "Covered"),
                                                       selected = "Long")
                      ),
                      column(4,
                             shinyWidgets::pickerInput(inputId = ns("options_strategy"),
                                                       label = NULL,
                                                       choices = # NULL,
                                                         c(names(.e$cOptions_obj$configs$pars)[-1]),
                                                       selected = "call")
                      ),
                      column(4,
                             shinyWidgets::pickerInput(inputId = ns("options_exercise"),
                                                       label = NULL, # "Exercise",
                                                       choices = c("American", "European", "Asian"),
                                                       selected = "American")
                      )
                  )
           ),
           column(12,
                  tags$h6("Common Parameters:", style = "font-weight:bold;")
           ),
           column(12,
                  uiOutput(ns("Options_commonParameters"))
           ),
           column(12,
                  tags$h6("Strategy Specific Parameters:", style = "font-weight:bold;")
           ),
           column(12,
                  uiOutput(ns("Options_varParameters"))
           ),
           column(12,
                  tags$h6("Other Parameters:", style = "font-weight:bold;")
           ),
           column(12,
                  div(class = "row",
                      # # Number of prices in the average calculation
                      column(3,
                             numericInput(
                               inputId = ns("options_m"),
                               label = "m",
                               value = 3,
                               min = NA, max = NA, step = NA, width = NULL
                             )
                      ),
                      # Number of Monte Carlo iterations
                      column(3,
                             numericInput(
                               inputId = ns("options_numsim"),
                               label = "numsim",
                               value = 1000,
                               min = NA, max = NA, step = NA, width = NULL
                             )
                      ),
                      # nstep Number of binomial steps. Default is nstep = 10
                      column(3,
                             numericInput(
                               inputId = ns("options_nstep"),
                               label = "nstep",
                               value = 20,
                               min = NA, max = NA, step = NA, width = NULL
                             )
                      ),
                      # Barrier H
                      column(3,
                             numericInput(
                               inputId = ns("options_H"),
                               label = "Barrier",
                               value = 44,
                               min = NA, max = NA, step = NA, width = NULL
                             )
                      )
                  )
           ),
           br(),
           column(12,
                  actionButton(inputId = ns("options_add_btn"),
                               label = "Add to Portfolio",
                               icon = shiny::icon(.e$configs$icons$refresh, verify_fa = FALSE) ),
                  actionButton(inputId = ns("options_defaultportfolio_btn"),
                               label = "Default Portfolio",
                               icon = shiny::icon(.e$configs$icons$refresh, verify_fa = FALSE) )
           )
  ) # fluidRow
} # Options_dropdownButton_func

# ---------------------------------------------------------------------------- #
options_guide_btn_ui <- function(id, .e = .e){

  ns <- NS(id)

  if(.e$Deployflag){
    path <- paste0("./www/Options_Pics/")
  }else{
    path <- system.file("app/www/Options_Pics", package = pkgload::pkg_name())
  }

  files = list.files(path)

  inputs <- lapply(1:length(files), function(x){
    tags$img(
      src = base64enc::dataURI(
      file = paste0(path,"/", files[x]),
      mime = "image/png"
    ), height = 115, width = 115)
  }) # lapply

  if (length(inputs) >= 1) {
    inputs_list <- split(inputs, rep_len(1:4, length(inputs)))
    rows <- lapply(inputs_list, function(row_inputs) {
      div(style = "display:flex;", row_inputs)
    })
    return(tagList(rows))
  } else {
    return(div(style = "display:flex;", inputs))
  }

}
