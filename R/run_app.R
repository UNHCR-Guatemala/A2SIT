#' Run the app
#'
#' @param ... A list of Options to be added to the app
#'
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @export
run_app <- function(...) {
  # # check if phantomJS installed
  # if(!webshot::is_phantomjs_installed()){
  #   webshot::install_phantomjs() |> suppressMessages()
  #   # check if succeeded
  #   if(!webshot::is_phantomjs_installed()){
  #     message("Cannot install phantomJS for some reason. This means that map downloads will be disabled.")
  #   }
  # }

  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server, enableBookmarking = "server"),
    golem_opts = list(...)
  )
}
