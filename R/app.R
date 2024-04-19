#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp runApp
run_app <- function() {
  runApp(shinyApp(ui = ui, server = server))
}