
# ---------------------------------------------------------------------------- #
#' All ui and server of Derivatives / Screener Tab
#'
#' @param id NS(id)
#' @param configs configs
#' @return Derivatives / Screener ui
#' @export
mod_Derivatives_Screener_ui <- function(id, configs){
  ns <- NS(id)
  # https://www.quantower.com/blog/volatility-smile-smirk-skew-and-options-strategies

  div(
    style = "margin-left:10px;",

    conditionalPanel(
    condition = "input.derivatives_screener_view == 'Options'", ns = ns,
    bs4Dash::tabsetPanel(
      id = "derivatives_screener_options_tabsetPanel",
      type = "tabs", # "pills",
      vertical = F,
      side = "left",
      selected	= "Market Data",
      tabPanel("Market Data",
               br(),
               DT::DTOutput(ns("screener_options_historicaldata_dttbl"), height = "800px")
               ),
      tabPanel(title = "Visuals",
               fluidRow(
                 column(12, align = "center", style = "margin-top:5px;",
                        shinyWidgets::radioGroupButtons(
                          inputId = ns("screener_visuals_plotview"),
                          label = NULL,
                          choices = c("implied_volatility", "delta", "gamma",
                                      "theta","vega","rho"),
                          selected = "implied_volatility",
                          justified = F,
                          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                        )
                 ),
                 column(12, align = "center",
                   br(),
                   plotOutput(outputId = ns("options_screener_visuals_ggplt"), height = "650px") %>%
                     shinycssloaders::withSpinner(color = "#fafafa")
                 )
               )
               ),
      tabPanel("Risk Measures",
               br(),
               DT::DTOutput(ns("options_screener_risk_dttbl"))
               ),
      tabPanel("Open Interest",
               br(),
               plotOutput(outputId = ns("options_screener_openinterest_ggplt"), height = "650px") %>%
                 shinycssloaders::withSpinner(color = "#fafafa")
               ),
      tabPanel("Volume",
               br(),
               plotOutput(outputId = ns("options_screener_volume_ggplt"), height = "650px") %>%
                 shinycssloaders::withSpinner(color = "#fafafa")
               ),
      tabPanel("Spread",
               br(),
               plotOutput(outputId = ns("options_screener_spread_ggplt"), height = "650px") %>%
                 shinycssloaders::withSpinner(color = "#fafafa")
      ),
      tabPanel("IV",
               br(),
               plotOutput(outputId = ns("options_screener_iv_ggplt"), height = "650px") %>%
                 shinycssloaders::withSpinner(color = "#fafafa")
      )

      ) # tabsetPanel
    ),  # conditionalPanel Options
    # c("Options", "Future", "Swap", "Others")
    conditionalPanel(
      condition = "input.derivatives_screener_view == 'Future'", ns = ns,
      p("Future")
    )

    ) # mian div
} # mod_Derivatives_Screener_ui


# ---------------------------------------------------------------------------- #
# Server
# ---------------------------------------------------------------------------- #
#' All server side functions of Derivatives / Screener Tab
#'
#' @param input input
#' @param output output
#' @param session session
#' @param id NS(id)
#' @param .e .e env
#' @param up_input server side inputs
#' @return Derivatives / Screener
#' @export
mod_Derivatives_Screener_server <- function(input, output, session,
                                           id, up_input, .e = .e){



  # .......................................................................... #
  observeEvent(input$refresh_btn, {

    if(!is.null(input$PageViewMenu)){
      if(input$PageViewMenu == 'Screener'){

        if(existsymbol(.e$data$symbol, session) ){
          cat("Screener for symbol : ", .e$data$symbol, "\n")
          .e$alphavantage_options <- .e$cETL_obj$read_alphavantage_options(symbol =
                                                                             .e$data$symbol)$data
          if(length(.e$alphavantage_options) == 0){
            .e$alphavantage_options <- data.frame()
          }
        }else{
          .e$alphavantage_options <- data.frame()
        }

        # .................................................................... #
        # Market data table
        output$screener_options_historicaldata_dttbl <- DT::renderDT({
          if(nrow(.e$alphavantage_options) >= 1){
            tmp <- .e$alphavantage_options
            if(!is.null(tmp) && nrow(tmp) >=1){
              dt <- DT::datatable(tmp, class = 'cell-border stripe',
                                  filter = 'top',
                                  rownames = FALSE,
                                  options = list(
                                    pageLength = 20,
                                    scrollY = "600px",
                                    scrollX = F))
              dt
            }
          }
        })

        # .................................................................... #
        # # screener_volatilitysmile
        output$options_screener_visuals_ggplt <- renderCachedPlot(
          res = 70, cache = "session",
          cacheKeyExpr = {list(input$refresh_btn,
                               input$derivatives_screener_options_tabsetPanel,
                               input$screener_visuals_plotview
                               ) },
          {
            if(nrow(.e$alphavantage_options) >= 1){
              options_screener_create_visuals_ggplt(df = .e$alphavantage_options,
                                         spotprice = 250,
                                         plotview = input$screener_visuals_plotview)
            }
          }) # output$options_screener_visuals_ggplt

        # .................................................................... #
        output$options_screener_risk_dttbl <- DT::renderDT({

          if(nrow(.e$alphavantage_options) >= 1){
            tmp <- options_screener_RiskThresholds(df = .e$alphavantage_options)
          }else{tmp = NULL}

          if(!is.null(tmp) && nrow(tmp) >=1){
            dt <- DT::datatable(tmp, class = 'cell-border stripe',
                                rownames = FALSE,
                                filter = 'top',
                                options = list(
                                  pageLength = 20,
                                  scrollY = "600px",
                                  scrollX = F))
            dt
          }
        }) # output$options_screener_risk_dttbl
        # .................................................................... #
        # Open interest Box Plot
        output$options_screener_openinterest_ggplt <- renderCachedPlot(
          res = 70, cache = "session",
          cacheKeyExpr = {list(input$derivatives_screener_options_tabsetPanel) },
          {
            if(nrow(.e$alphavantage_options) >= 1){
              options_screener_create_openinterest_ggplt(df = .e$alphavantage_options,
                                                  spotprice = 250)
            }
          })

        # .................................................................... #
        # options_screener_volume_ggplt
        output$options_screener_volume_ggplt <- renderCachedPlot(
          res = 70, cache = "session",
          cacheKeyExpr = {list(input$derivatives_screener_options_tabsetPanel) },
          {
            if(nrow(.e$alphavantage_options) >= 1){
              options_screener_create_volume_ggplt(df = .e$alphavantage_options,
                                                       spotprice = 250)
            }
          })
        # .................................................................... #
        # options_screener_spread_ggplt
        output$options_screener_spread_ggplt <- renderCachedPlot(
          res = 70, cache = "session",
          cacheKeyExpr = {list(input$derivatives_screener_options_tabsetPanel) },
          {
            if(nrow(.e$alphavantage_options) >= 1){
              options_screener_create_spread_ggplt(df = .e$alphavantage_options,
                                                 spotprice = 250)
            }
          })
        # .................................................................... #
        # options_screener_iv_ggplt
        output$options_screener_iv_ggplt <- renderCachedPlot(
          res = 70, cache = "session",
          cacheKeyExpr = {list(input$derivatives_screener_options_tabsetPanel) },
          {
            if(nrow(.e$alphavantage_options) >= 1){
              options_screener_create_iv_ggplt(df = .e$alphavantage_options,
                                                 spotprice = 250)
            }
          })
        #

      } # if Screener
      } # if
  })

} # mod_Derivatives_Screener_server

# ---------------------------------------------------------------------------- #
# Suport Functions
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# create_volatilitysmile_plt
# ---------------------------------------------------------------------------- #
options_screener_create_visuals_ggplt <- function(df,
                                                  symbol = NULL,
                                                  spotprice = 250, # 221,10
                                                  plotview = "implied_volatility"){

  if(!is.null(df) && nrow(df) >= 1){
    df$dtm <-  as.numeric(difftime(df$expiration, df$date, units = "days"))
    df$tt = as.numeric(difftime(df$expiration, df$date, units = "days") / 365)
    # colnames(df)
    df$strike <- as.numeric(df$strike)
    df$dtm <- round(as.numeric(df$dtm),0)
    df$dtm_label <- paste0("T: ", df$dtm)
    df$implied_volatility <- as.numeric(df$implied_volatility)
    # Add median of each dtm or tt for IV
    df$delta <- as.numeric(df$delta)
    df$gamma <- as.numeric(df$gamma)
    df$theta <- as.numeric(df$theta)
    df$vega <- as.numeric(df$vega)
    df$rho <- as.numeric(df$rho)
    unqdtm <- unique(df$dtm)
    # df$IV_Median <- unlist(sapply(1:length(unqdtm), function(i){
    #   n = nrow(df[df$dtm == unqdtm[i],])
    #   rep(median(df[df$dtm == unqdtm[i], "implied_volatility"]),n)
    # }))
    df$IV_Median <- NA
    #clnms = c("type", "tt", "dtm","dtm_label","strike", "implied_volatility", "IV_Median")
    clnms <- c("type", "tt", "dtm","dtm_label","strike", "implied_volatility", "delta", "gamma",
              "theta","vega","rho" )

    df <- df[,clnms]
    # df1 <<- df
    colnames(df) <- gsub(plotview, "plotview",  colnames(df))
    # S = spotprice = 197.88
    df <- as.data.frame(df)
    # volatility smile
    gplt <- ggplot2::ggplot(df, ggplot2::aes(x = df$strike, y = df$plotview, group = df$type,
                                             colour = df$type)) +
      ggplot2::geom_line() + # facet_wrap(~ dtm_label, scales = 'free_y') +
      ggplot2::facet_wrap(~factor(dtm_label, unique(dtm_label)), scales = 'free_y') +
      ggplot2::geom_vline(xintercept = spotprice, color = "blue", linewidth=0.3) +
      #geom_text(aes(x=S, y = IV_Median, label=paste0("\n",S)), size=3, colour="blue", angle=90) +
      #geom_hline(aes(yintercept = IV_Median), color = "black", size=0.3) +
      #geom_text(aes(x=S, y = IV_Median, label=paste0("\n",IV_Median)), size=3, colour="black", angle=0) +
      ggplot2::ggtitle(paste0(symbol, " (S = ", spotprice, ")")) +
      ggplot2::xlab("Strike Price") + ggplot2::ylab(plotview) +
      ggplot2::scale_color_discrete(name = 'Options') +
      ggplot2::theme_bw() + ggplot2::theme(legend.position = "right") +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    }else{
      gplt <- ggplot2::ggplot(data.frame()) + ggplot2::geom_point() + ggplot2::xlim(0, 10) +
        ggplot2::ylim(-20, 20) + ggplot2::theme_bw()
      }
  return(gplt)
} # create_volatilitysmile_plt

# ---------------------------------------------------------------------------- #
# Filter Options (Greeks Strategy), Low, Medium; High Risk
# ---------------------------------------------------------------------------- #

options_screener_RiskThresholds <- function(df){

  # .......................................................................... #
  ## Delta:
  # Low Risk: Delta closer to 0.5 (for calls) or -0.5 (for puts) indicates a balanced response to
  # underlying asset price movement.
  # Medium Risk: Delta can range from 0.2 to 0.8 (calls) or -0.2 to -0.8 (puts),
  # suggesting a moderate price change with the underlying asset.
  # High Risk: Delta closer to 1 (calls) or -1 (puts) signifies a strong and potentially
  # rapid price change with the underlying asset.
  df$delta <- as.numeric(df$delta)
  df$DeltaRisk <- sapply(df$delta, function(x){
    if(abs(x) > 0.8){"High Risk"}
    else if(abs(x) > 0.55 && abs(x) <= 0.8){"Medium Risk"}
    else if(abs(x) > 0.2 && abs(x) <= 0.45){"Medium Risk"}
    else if(abs(x) > 0.45 && abs(x) <= 0.55){"Low Risk"}else{NA}
  })

  # .......................................................................... #
  ## Theta:
  # Low Risk: Slightly negative Theta indicates slower time decay, allowing more time
  # for the option to potentially become profitable.
  # Medium Risk: Moderately negative Theta suggests a balance between time decay and potential for price gains.
  # High Risk: Highly negative Theta signifies rapid time decay, making the option lose value faster,
  # especially closer to expiration.
  # Low Risk: Theta between 0 and -0.05 (slightly negative, slower decay)
  # Medium Risk: Theta between -0.05 and -0.15 (moderately negative, moderate decay)
  # High Risk: Theta less than -0.15 (highly negative, faster decay)
  df$theta <- as.numeric(df$theta)
  df$ThetaRisk <- sapply(df$theta, function(x){
    if(x > -0.05 && x <= 0){"Low Risk"}
    else if(x > -0.15 && x <= -0.05){"Medium Risk"}
    else if( x <= -0.15){"High Risk"}
  })

  # .......................................................................... #
  ## Vega:
  # Low Risk: Low Vega indicates the option price is less sensitive to changes in implied volatility.
  # Medium Risk: Moderate Vega suggests some sensitivity to IV changes, which can be both beneficial and risky.
  # High Risk: High Vega means the option price can fluctuate significantly with changes in IV,
  # potentially leading to amplified gains or losses.

  vega_range = range(as.numeric(df$vega))
  vega_quantiles <- stats::quantile(vega_range, probs = c(0.33, 0.66))
  # Assign classifications based on quantiles
  df$VegaRisk <- ifelse(vega_range <= vega_quantiles[1], "Low Risk",
                              ifelse(vega_range > vega_quantiles[2], "High Risk", "Medium Risk"))


  # .......................................................................... #
  # Gamma: (Consider in conjunction with Delta)
  # Low Risk: Low Gamma suggests a slower change in Delta, leading to more predictable but potentially smaller gains (or losses).
  # Medium Risk: Moderate Gamma indicates a balance between the rate of Delta change and potential for price movement.
  # High Risk: High Gamma signifies a rapid change in Delta, potentially leading to larger but also more unpredictable gains (or losses).
  gamma_range = range(as.numeric(df$gamma))
  gamma_quantiles <-  stats::quantile(gamma_range, probs = c(0.33, 0.66))
  # Assign classifications based on quantiles
  df$GammaRisk <- ifelse(gamma_range <= gamma_quantiles[1], "Low Risk",
                        ifelse(gamma_range > gamma_quantiles[2], "High Risk", "Medium Risk"))
  # .......................................................................... #
  # Add probaility of profit at expiration
  cOptions_obj <- cOptions(par = data.frame())

  df$probability = sapply(1:nrow(df), function(x){

    tt = as.numeric( difftime(df$expiration[x], df$date[x], units  = "days")) / 365
    k = as.numeric(df$strike[x])
    v = as.numeric(df$implied_volatility[x])
    price = as.numeric(df$last[x])
    if(price == 0){price = as.numeric(df$bid[x])}
    if(price == 0){price = as.numeric(df$ask[x])}
    type = df$type[x]
    r = 0.05
    if(price != 0){
      pr = cOptions_obj$calc_implied_price(s = 1,k = k,
                                           v = v, r = r, tt = tt , d = 0,
                                           price = price, type = type) # s is not matter

      round(cOptions_obj$calc_profitprobability(S = pr,K = k,v = v,r = r,tt = tt , d = 0),4)
    }else{NA}

  })

  df$spread = round(as.numeric(df$ask) - as.numeric(df$bid),2)

  # colnames(df)
  clnms = c("contractID", "type","strike","expiration","probability", "spread", "DeltaRisk","ThetaRisk","VegaRisk","GammaRisk" ,
            "implied_volatility", "delta", "gamma","theta","vega","rho" )
  df <- df[,clnms]
  return(df)

} # options_screener_RiskThresholds


# ---------------------------------------------------------------------------- #
# create_volatilitysmile_plt
# ---------------------------------------------------------------------------- #
options_screener_create_openinterest_ggplt <- function(df,
                                                  spotprice = 250 # , # 221,10
                                                  ){
  # df = .e$alphavantage_options
  if(!is.null(df) && nrow(df) >= 1){

    df <- df[df$open_interest > 0, ]
    df_call <- df[df$type == "call",]
    df_put <- df[df$type == "put",]

    # max(df_call$open_interest); max(df_put$open_interest)
    df_call <- df_call[df_call$strike <= spotprice,]
    df_put <- df_put[df_put$strike >= spotprice,]

    df1 = rbind(df_call, df_put)
    df1$ttm = round(difftime(df1$expiration, df1$date, units="days"),1)
    df1$ttm <- factor(as.character(df1$ttm), levels = unique(sort(df1$ttm)))

    df1$open_interest <- as.numeric(df1$open_interest)

    gplt <- ggplot2::ggplot(df1, ggplot2::aes(x = df1$ttm, y = df1$open_interest, fill = df1$type)) +
      ggplot2::geom_boxplot(outlier.alpha = 0.4, outlier.size = 1) +
      ggplot2::xlab("Days to Expiration Date") +
      ggplot2::theme(legend.position="right") +
      ggplot2::ggtitle("Open Interest") +
      ggplot2::theme_bw() # +
      # add label
      # ggplot2::stat_summary(ggplot2::aes(label = round(after_stat(y), 1)),
      #   geom = "text",
      #   size = 3,
      #   fun = function(y) { o <- grDevices::boxplot.stats(y)$out; if(length(o) == 0) NA else o },
      #   hjust = -0.2 )

  }else{
    gplt <- ggplot2::ggplot(data.frame()) + ggplot2::geom_point() + ggplot2::xlim(0, 10) +
      ggplot2::ylim(-20, 20) + ggplot2::theme_bw()
  }
  return(gplt)
}

# ---------------------------------------------------------------------------- #
# options_screener_create_volume_ggplt
# ---------------------------------------------------------------------------- #
options_screener_create_volume_ggplt <- function(df,
                                                 spotprice = 250 # , # 221,10
){
  # Volume Analysis: Consider the bid and ask volume. Higher volumes usually imply
  # increased market interest, making it easier to buy or sell options at desired prices.
  # Analyzing volume patterns can reveal trends and potential price movements.

  # df = .e$alphavantage_options
  if(!is.null(df) && nrow(df) >= 1){

    df <- df[df$open_interest > 0, ]
    df_call <- df[df$type == "call",]
    df_put <- df[df$type == "put",]

    df_call <- df_call[df_call$strike <= spotprice,]
    df_put <- df_put[df_put$strike >= spotprice,]

    df1 = rbind(df_call, df_put)
    df1$ttm = round(difftime(df1$expiration, df1$date, units="days"),1)
    df1$ttm <- factor(as.character(df1$ttm), levels = unique(sort(df1$ttm)))

    df1$volume <- as.numeric(df1$volume)
    df1$strike <- as.numeric(df1$strike)

    gplt <- ggplot2::ggplot(df1, ggplot2::aes(x = df1$ttm, y = df1$strike, size = df1$volume,
                                              color = df1$type)) +
      ggplot2::geom_boxplot() +
      ggplot2::ggtitle("Volume") +
      ggplot2::xlab("Days to Expiration Date") +
      ggplot2::ylab("Strike Price") +
      ggplot2::theme_bw()

  }else{
    gplt <- ggplot2::ggplot(data.frame()) + ggplot2::geom_point() + ggplot2::xlim(0, 10) +
      ggplot2::ylim(-20, 20) + ggplot2::theme_bw()
  }
  return(gplt)
}

# ---------------------------------------------------------------------------- #
# options_screener_create_volume_ggplt
# ---------------------------------------------------------------------------- #
options_screener_create_spread_ggplt <- function(df,
                                                 spotprice = 250){
  # Spread Analysis: Calculate the bid-ask spread by subtracting the bid price from the ask price.
  # A narrower spread indicates higher liquidity and tighter market conditions.
  # Monitoring spread trends can help identify optimal entry and exit points.

  # Option Pricing: Compare the bid and ask prices to the theoretical values derived
  # from an options pricing model, such as the Black-Scholes model. Assess whether the
  # options are overpriced or underpriced based on this comparison. Deviations may present
  # opportunities for trading strategies.

  if(!is.null(df) && nrow(df) >= 1){

    df <- df[df$open_interest > 0, ]
    df_call <- df[df$type == "call",]
    df_put <- df[df$type == "put",]

    df_call <- df_call[df_call$strike <= spotprice,]
    df_put <- df_put[df_put$strike >= spotprice,]

    df1 = rbind(df_call, df_put)
    df1$ttm = round(difftime(df1$expiration, df1$date, units="days"),1)
    df1$ttm <- factor(as.character(df1$ttm), levels = unique(sort(df1$ttm)))

    df1$bid <- as.numeric(df1$bid)
    df1$ask <- as.numeric(df1$ask)
    df1$strike <- as.numeric(df1$strike)
    df1$spread <- as.numeric(df1$ask) - as.numeric(df1$bid)

    # gplt <- ggplot(df1, aes(x = expiration, y = spread, group = type, color = type)) +
    #   geom_line(aes(color = type, linetype = type)) +
    #   theme_bw() + stat_summary(fun = mean, geom="line", size = 0.5)

    # gplt <- ggplot(df1, aes(x = strike, y = bid, group = type, color = type)) +
    #   geom_line(aes(color = type, linetype = type)) +
    #   facet_wrap(~expiration) +
    #   theme_bw()

    # gplt <- ggplot(df1, aes(x = strike, y = ask, group = type, color = type)) +
    #   geom_line(aes(color = type, linetype = type)) +
    #   facet_wrap(~expiration) +
    #   theme_bw()

    gplt <- ggplot2::ggplot(df1, ggplot2::aes(x = df1$strike, y = df1$spread, group = df1$type,
                                              color = df1$type)) +
      ggplot2::geom_line(ggplot2::aes(color = df1$type, linetype = df1$type)) +
      ggplot2::facet_wrap(~expiration) +
      ggplot2::theme_bw()

  }else{
    gplt <- ggplot2::ggplot(data.frame()) + ggplot2::geom_point() + ggplot2::xlim(0, 10) +
      ggplot2::ylim(-20, 20) + ggplot2::theme_bw()
  }
  return(gplt)
}

# ---------------------------------------------------------------------------- #
# options_screener_create_iv_ggplt
# ---------------------------------------------------------------------------- #
options_screener_create_iv_ggplt <- function(df,
                                             spotprice = 250){
  if(!is.null(df) && nrow(df) >= 1){

    df <- df[df$open_interest > 0, ]
    df_call <- df[df$type == "call",]
    df_put <- df[df$type == "put",]

    df_call <- df_call[df_call$strike <= spotprice,]
    df_put <- df_put[df_put$strike >= spotprice,]

    df1 = rbind(df_call, df_put)
    df1$ttm = round(difftime(df1$expiration, df1$date, units="days"),1)
    df1$ttm <- factor(as.character(df1$ttm), levels = unique(sort(df1$ttm)))

    df1$implied_volatility <- as.numeric(df1$implied_volatility)
    df1 <- as.data.frame(df1)

    gplt <- ggplot2::ggplot(df1, ggplot2::aes(x = df1$ttm, y = df1$implied_volatility, fill = df1$type)) +
      ggplot2::geom_boxplot(outlier.alpha = 0.4, outlier.size = 1) +
      ggplot2::xlab("Days to Expiration Date") +
      ggplot2::theme(legend.position="right") +
      ggplot2::ggtitle("Implied Volatility") +
      ggplot2::theme_bw()

  }else{
    gplt <- ggplot2::ggplot(data.frame()) + ggplot2::geom_point() + ggplot2::xlim(0, 10) +
      ggplot2::ylim(-20, 20) + ggplot2::theme_bw()
  }
  return(gplt)
}
# ---------------------------------------------------------------------------- #
# Parameters of Options Strategy Page
# ---------------------------------------------------------------------------- #
options_screener_dropdownButton_ui_func <- function(id, configs, .e=.e){

  ns <- NS(id)
  fluidRow(style = "font-size:10px;",
           column(12,
                  tags$h6("Derivatives Market View Parameters", style = "font-weight:bold;")
           ),
           column(12,
                  shinyWidgets::radioGroupButtons(
                         inputId = ns("derivatives_screener_view"),
                         label = "Market",
                         choices = c("Options", "Future", "Swap", "Others"),
                         selected = "Options",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-check-square",
                                        style = "color: steelblue"),
                           no = tags$i(class = "fa fa-square-o",
                                       style = "color: steelblue"))
                       )
           )
  ) # fluidRow
} # options_screener_dropdownButton_ui_func
