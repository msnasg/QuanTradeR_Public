
# ---------------------------------------------------------------------------- #
Run_ShinyApp <- function(app = pkgload::pkg_name(), host = "127.0.0.1",
                         launch.browser = NULL,
                         local = TRUE, .access_fs = FALSE,
                          ...) {
  #pkgload::load_all()
  dots <- list(...)
  tmp <- names(dots) %in% setdiff(methods::formalArgs(Run_ShinyApp), "appDir")
  extra <- dots[!tmp]
  runApp_args <- c(list(host = host, launch.browser = launch.browser), dots[tmp])

  # use ... to assign values in environment set by local
  if (typeof(local) == "environment") {
    if (local == FALSE) for (i in names(extra)) assign(i, value = dots[[i]], envir = local)
  } else {
    if (is.logical(local)) {
      if (local) {
        for (i in names(extra)) assign(i, value = dots[[i]])
      } else {
        local <- globalenv()
        for (i in names(extra)) assign(i, value = dots[[i]], envir = local)
      } # else local
    } else {
      stop("'local' should be either TRUE, FALSE or an environment")
    } # else is.logical
  } # else typeof

  # until exist ui.R , server.R and global.R
  files <- base::normalizePath(list.files(system.file(package = app, "app"),
                                           full.names = TRUE))
  files <- files[base::basename(files) %in% c("ui.R", "server.R", "global.R")]
  if (!all(c("ui.R", "server.R") %in% basename(files))) stop("can't find app")

  # source application files
  ui <- uifunc <- NULL
  server <- NULL

  # ui_Func <- NULL
  for (i in files) source(file = i, local = local, chdir = TRUE, echo = FALSE, verbose = FALSE)

  tryCatch(
    {
      if (length(launch.browser) == 0) {
        runApp_args <- runApp_args[names(runApp_args) != "launch.browser"]
      }
      suppressWarnings({
        app <- shiny::shinyApp(ui = ui_Func(.e = .e), server = server, options = runApp_args)
        shiny::runApp(app)
      })
    },
    error = function(e) print(e$message)
  )


}

