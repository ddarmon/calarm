#' main server of app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @return shiny server
#' @export
server <- function(input, output, session) {
  library(shiny)
  library(lubridate)
  library(glue)

  system('clear')

  today <- date(Sys.time())

  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0){
    time.string <- readLines('/Users/daviddarmon/Dropbox (Personal)/Reference/P/python/time-circles/time.txt')
    args <- strsplit(time.string, ' ')[[1]]
  }

  if (length(args) == 3){
    hour <- args[1]
    minute <- args[2]
    am.pm <- toupper(args[3])
  }else if (length(args) == 2){
    hour <- args[1]
    minute <- 0
    am.pm <- toupper(args[2])
  }

  writeLines(glue("{hour} {minute} {toupper(am.pm)}\n"), '/Users/daviddarmon/Dropbox (Personal)/Reference/P/python/time-circles/time.txt')

  end.time <- strptime(glue("{today} {hour} {minute} {am.pm}"),"%Y-%m-%d %I %M %p")

  # system('clear')

  output$currentTime <- renderPlot({
    invalidateLater(100, session)
    paste("The current time is", Sys.time())

    total.minutes <- difftime(end.time, Sys.time(), units = 'mins')
    full.hours <- floor(as.numeric(total.minutes/60))

    if(total.minutes < 0){
      stopApp()
    }

    par(pty = 's', xaxt = 'n', yaxt = 'n')
    plot(0, 0, cex = 0,
         xlim = c(-1, 1), ylim = c(-1, 1),
         xlab = '', ylab = '',
         main = strsplit(as.character(end.time), split = " ")[[1]][2])

    for (hour in full.hours:0){
      if (hour == 0){
        s <- ceiling(total.minutes)/60
      }else{
        s <- 1
      }

      theta <- seq(0, 2*pi*s, length.out = 10000) + pi/2

      hour.factor <- (hour+1)/(full.hours+1)

      x <- cos(theta)*hour.factor
      y <- sin(theta)*hour.factor

      if (hour == 0){
        polygon(c(0, x, 0), c(0, y, 0), col = hour + 2)
      }else{
        polygon(x, y, col = hour + 2)
      }

      total.minutes <- total.minutes - 60
    }
    abline(h = 0, v = 0, lty = 3)


    # Seconds timer:
    # s <- 2*pi*second(Sys.time())/60 + pi/2
    # segments(x0 = 0, x1 = -cos(s), y0 = 0, y1 = sin(s), lwd = 3)
  })
}
