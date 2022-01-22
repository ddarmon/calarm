#' main app ui
#'
#' @return shiny ui
#' @export
#'
ui <- function() {
  fluidPage(

  h2(plotOutput("currentTime"))

)}
