width_choices <- c(
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
  "eleven", "twelve", "thirtheen", "fourteen", "fifteen", "sixteen"
)

# Utility function to get grid rows
ui_row <- function(...) {
  tags$div(
    class = "ui grid",
    tags$div(
      class = "row",
      ...
    )
  )
}

# Utility function to get grid columns
ui_col <- function(width, ...) {
  tags$div(
    class = paste(width_choices[width], "wide column"),
    ...
  )
}