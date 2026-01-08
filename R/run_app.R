#' Run the Zenodo network explorer app
#'
#' @importFrom shiny shinyApp
#'
#' @return A Shiny app object.
#' @export
run_app <- function() {
  shiny::shinyApp(ui, server)
}
