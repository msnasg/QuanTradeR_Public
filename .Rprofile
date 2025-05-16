
cat("Laod Custom functions to source when starting R ... \n")

# do not use scientific convention except for large numbers
options( scipen=8 )
options(help_type="html")


.First <- function() cat("\n   Welcome to QuanTradeR Package! \n\n")
.Last <- function()  cat("\n   Goodbye QuanTradeR! \n\n")

.onLoad <- function(...) {
  shiny::addResourcePath("www", system.file("app/www", package = pkgload::pkg_name()))
}

if (interactive()) {
  # will load these packages automatically in interactive sessions
  suppressMessages(suppressWarnings(require("devtools")))
  suppressMessages(suppressWarnings(require("usethis")))
  suppressMessages(suppressWarnings(require("testthat")))
  cat("Required library loaded. \n")
}

#------------------------------------------------------------------------------#
Authorization_Table <- data.frame(
  user = c(""),
  password = c(""),
  role = c("Admin"),
  id = c( "Mohsen"),
  name = c("Mohsen"),
  admin = c(TRUE),
  stringsAsFactors = FALSE,
  row.names = NULL)

# Authenticate Page Lables
shinymanager::set_labels(
  language = "en",
  "Please authenticate" = tags$h5("Unlock a World of Trading Opportunities"), # "QuanTradeR App",
  "Username:" = "Username",
  "Password:" = "Passwort",
  "Login" = "Login" )
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
#                             Base Functions
#------------------------------------------------------------------------------#
## Returns a logical vector TRUE for elements of X not in Y
"%nin%" <- function(x, y) {!(x %in% y)}
`%not_in%` <- Negate(`%in%`)
not_null <- Negate(is.null)
not_na <- Negate(is.na)
# Removes the null from a vector
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}
"%||%" <- function(x, y) {
  if (is.null(x)) { y
  } else { x }
}



## End(Not run)
