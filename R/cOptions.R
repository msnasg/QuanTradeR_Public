
#' cOptions: Class for Options Derivatives Analysis
#'
#' @field par data.frame.
#' @importFrom methods setRefClass new
#' @return Tables, plots
#' @export
cOptions <- setRefClass("cOptions",
                        fields = list(
                          parL = "list",
                          pardf = "data.frame",
                          configs = "list",
                          par = "data.frame")
                        )

# Class methods
cOptions$methods(
  initialize = function(...){
    # .par <- .self$par
    # callSuper(..., par=.par)
    # # parameters
    # uap: Underline Asset Price
    # k: Strike price
    # pr: Market Premium (price)
    # v or sigma: Volatility of the underlygin asset price
    # r: Annual continuously-compounded risk-free interest rate
    # tt: Time to maturity in years
    # d: Dividend yield, annualized, continuously-compounded
    .self$configs = list(
      pars =
        list(
        common = c("v"=0.3, "tt"=0.25, "r"=0.08, "d"=0, "s_low"=-100, "s_up"=100),
        # custom = c("k"=45,"pr"=5),

        call = c("uap"=40, "k"=45,"pr"=5),
        put = c("uap"=45, "k"=40,"pr"=4),
        straddle = c("uap_c"=40,"uap_p"=40,"k"=45,"pr_c"=4,"pr_p"=5),
        strangle = c("uap_c"=40,"uap_p"=40, "k_c"=45,"k_p"=40 ,"pr_c"=4,"pr_p"=5),
        put_spread = c("uap_lp"=40 ,"uap_sp"=40,"k_lp"=40 ,"k_sp"=50,"pr_lp"=4,"pr_sp"=5),
        call_spread = c("uap_lc"=40 ,"uap_sc"=40,"k_lc"=45,"k_sc"=50, "pr_lc"=4,"pr_sc"=5),
        butterfly_put = c("uap"=40,
                          "k_lp1"=50,"k_sp"=45,"k_lp2"=40,
                          "pr_lp1"=4,"pr_sp"=4.1,"pr_lp2"=4.2),
        butterfly_call =  c("uap"=40,
                            "k_lc1"=45,"k_sc"=50,"k_lc2"=55,
                            "pr_lc1"=4,"pr_sc"=4.1,"pr_lc2"=4.2),
        # arbitrage opportunities stemming from mispricing
        box_spread =  c("uap_lc"=45,"uap_sc"=50,"uap_lp"=40,"uap_sp"=50,
                        "k_lc"=45,"k_sc"=50,"k_lp"=40,"k_sp"=50,
                        "pr_lc"=4,"pr_sc"=5,"pr_lp"=6,"pr_sp"=7),
        # benefit from low market volatility
        iron_condour = c("uap_lc"=40,"uap_sc"=40,"uap_lp"=40,"uap_sp"=40,
                         "k_lc"=45,"k_sc"=50,"k_lp"=40,"k_sp"=50,
                         "pr_lc"=4,"pr_sc"=5,"pr_lp"=6,"pr_sp"=7)
        )#,  # par list
        # read_strateges_picfolder()
      #)
      ) # configs list
  },
  # -------------- #
  show = function(.self){
    cat("Class Name: ", class(.self), " initiated. \n", sep="")
    cat("par: ", class(.self$par), ", dim: (",
        paste0(dim(.self$par), collapse = ","),
        ") \n", sep="")
  },
  # -------------- # read_strateges_picfolder
  read_strateges_picfolder = function(strategy_direction = "Bullish"){

  },
  # -------------- # auto_numericInput_func
  auto_numericInput_func = function(par_name = "common",id){

  }, # auto_numericInput_func

  # -------------- # portfolio_showdf
  portfolio_showdf = function(inputsL){

  },
  # .......................................................................... #
  seperate_optparams = function(df){

  },
  # .......................................................................... #
  seperate_multi_optparams = function(df){

  },
  # .......................................................................... #
  selectoption_df = function(options_id, showdf){

  },
  # .......................................................................... #
  dnorm_blackscholes = function(x){

  },
  # .......................................................................... #
  blackscholes = function(S, K, r, T, sigma, call = TRUE){

  },
  # .......................................................................... #
  calc_implied_volatility = function(s,k,r,tt,d,price, type = "call"){

  },
  # .......................................................................... #
  calc_implied_price = function(s,k,v,r,tt,d,price, type = "call"){

  },
  # .......................................................................... #
  calc_profitprobability = function(S,K,v,r,tt,d) {

  },
  # .......................................................................... #
  delta_blackscholes = function(S, K, r, T, sigma, d, call = TRUE){

  },
  # .......................................................................... #
  charm_blackscholes = function(S, K, r, T, sigma, d, call = TRUE){

  },
  # .......................................................................... #
  gamma_blackscholes = function(S, K, r, T, sigma, d = 0){

  },
  # .......................................................................... #
  speed_blackscholes = function(S, K, r, T, sigma, d = 0){

  },
  # .......................................................................... #
  zomma_blackscholes = function(S, K, r, T, sigma, d = 0){

  },
  # .......................................................................... #
  vega_blackscholes = function(S, K, r, T, sigma, d) {

  },
  # .......................................................................... #
  vanna_blackscholes = function(S, K, r, T, sigma, d=0) {

  },
  # .......................................................................... #
  vomma_blackscholes = function(S, K, r, T, sigma, d=0) {

  },
  # .......................................................................... #
  theta_blackscholes = function(S, K, r, T, sigma, d, call = TRUE) {

  },
  # .......................................................................... #
  rho_blackscholes = function(S, K, r, T, sigma, call = TRUE) {

  },
  # .......................................................................... #
  psi_blackscholes = function(S, K, r, T, sigma, d, call = TRUE) {

  },
  # .......................................................................... #
  elasticity_blackscholes = function(S, K, r, T, sigma, d, call = TRUE){

  },
  # .......................................................................... #
  greeks_calc_id = function(options_id, showdf){

  },
  # .......................................................................... #
  # .self = cOptions_obj
  butterfly_greeks = function(df){

  },

  # .......................................................................... #
  all_greeks = function(S, K, r, tt, sigma, d, callflags = T){

  },
  # .......................................................................... #
  # All Greeks and Implied Volatility plots
  # .......................................................................... #
  greeks_plots = function(options_id, showdf){

  },
  # .......................................................................... #
  # Sensitivity plots for all Greeks and Implied Volatility
  # Based on input variables
  # .......................................................................... #
  sensitivity_plots = function(options_id, showdf,
                               var = "k",
                               whichgreek = "Delta"){

  }, # sensitivity_plots
  # .......................................................................... #
  binomplot = function(options_id, showdf, nstep = 5){

  },
  # .......................................................................... #
  binomopt = function(options_id, showdf, nstep = 5){


  },
  # .......................................................................... #
  payoffs_plots = function(options_id, showdf){

  }, # payoffs_plots method
  # .......................................................................... #
  plt_strgy_basic = function(optdf, SPrices){

  }, # plt_strgy_basic
  # .......................................................................... #
  plt_strgy_butterfly = function(optdf, SPrices){

  }, # plt_strgy_butterfly
  # .......................................................................... #
  payoff_longcall = function(s, k, p) {

  },
  # .......................................................................... #
  # Function to calculate long put payoff
  payoff_longput = function(s, k, p) {

  },
  # .......................................................................... #
  # Function to calculate short call payoff
  payoff_shortcall = function(s, k, p) {

  },
  # .......................................................................... #
  # Function to calculate short put payoff
  payoff_shortput = function(s, k, p) {

  },
  # .......................................................................... #
  # Function to calculate binary call payoff
  payoff_binarycall = function(s, k, p) {

  },
  # .......................................................................... #
  # Function to calculate binary put payoff
  payoff_binaryput = function(s, k, p) {

  },
  # .......................................................................... #
  payoff_bullspread = function(s, k1, k2, p1, p2) {

  },
  # .......................................................................... #
  payoff_bearspread = function(s, k1, k2, p1, p2) {

  },
  # .......................................................................... #
  payoff_straddle = function(s, k, p1, p2) {

  },
  # .......................................................................... #
  payoff_riskreversal = function(s, k1, k2, p1, p2) {

  },
  # .......................................................................... #
  payoff_strangle = function(s, k1, k2, p1, p2) {

  },
  # .......................................................................... #
  payoff_longcall_butterflyspread = function(s, k1, k2, k3, p1, p2, p3) {

  },
  # .......................................................................... #
  payoff_longput_butterflyspread = function(s, k1, k2, k3, p1, p2, p3) {

  },
  # .......................................................................... #
  payoff_strip = function(s, k, p1, p2) {

  },
  # .......................................................................... #
  plt_portfoliopayoff = function(.self){


  }
  # .......................................................................... #
)

