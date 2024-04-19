rangeInput <- function(
  inputId, value, min = NULL, max = NULL, step = 1, labels = NULL
) {

  if (!is.null(labels)) {
    stopifnot(is.null(min))
    stopifnot(is.null(max))
    min <- 1
    max <- length(labels)
    data_label <- paste0(labels, collapse = ", ")
  } else {
    data_label <- NULL
  }

  input <- tags$input(
    type = "range", 
    min = min, 
    max = max, 
    value = value, 
    step = step, 
    `data-label` = data_label
  )

  form <- tags$div(
    class = "range-input",
    id = inputId,
    tags$div(
      tags$div(
        class = "range-input-controls",
        tags$div(class = "range-value"),
        input
      )
    )
  )

  deps <- htmltools::htmlDependency(
    name = "rangeInput",
    version = "1.0.0",
    src = c(file = app_sys("www", "range-input")),
    script = "binding.js",
    stylesheet = "styles.css"
  )
  htmltools::attachDependencies(form, deps)
}