#' @importFrom shiny tags

select_input_options <- list(
  render = I("
    {
      item: function(item, escape) { 
              var html = katex.renderToString(item.label);
              return '<div>' + html + '</div>'; 
            },
      option: function(item, escape) { 
                var html = katex.renderToString(item.label);
                return '<div>' + html + '</div>'; 
              }
    }"
  )
)

choices <- c("normal_1", "normal_2", "normal_3", "normal_mixture", "banana", "funnel")
choices_names <- c(
  "\\mathcal{N}\\left(\\begin{bmatrix} 0 \\\\ 0 \\end{bmatrix}, \\begin{bmatrix} 1 & 0 \\\\ 0 & 1 \\end{bmatrix}\\right)", 
  "\\mathcal{N}\\left(\\begin{bmatrix} 0 \\\\ 0 \\end{bmatrix}, \\begin{bmatrix} 1 & 0.3 \\\\ 0.3 & 1 \\end{bmatrix}\\right)", 
  "\\mathcal{N}\\left(\\begin{bmatrix} 0 \\\\ 0 \\end{bmatrix}, \\begin{bmatrix} 1 & 0.7 \\\\ 0.7 & 1 \\end{bmatrix}\\right)", 
  "\\frac{1}{2}\\mathcal{N}\\left(\\begin{bmatrix} -1.5 \\\\ -1.7 \\end{bmatrix}, 0.2 \\cdot \\boldsymbol{I}_2 \\right) + \\frac{1}{2}\\mathcal{N}\\left(\\begin{bmatrix} 0.9 \\\\ 0.5 \\end{bmatrix}, 1.25 \\cdot \\boldsymbol{I}_2 \\right)",
  "\\text{Rosenbrock's Banana}",
  "\\text{Neal's Funnel}"
)
choices <- setNames(choices, choices_names)

make_sidebar <- function() {
  tags$div(
    id = "sidebar",
    tags$div(
      class = "item",
      ui_row(tags$p("Choose a distribution", class = "sidebar-group-header")),
      ui_row(
        ui_col(
          width = 16,
          shiny::selectizeInput("distribution", NULL, choices, options = select_input_options)
        )
      )
    ),
    tags$div(
      class = "item",
      ui_row(tags$p("Controls", class = "sidebar-group-header")),
      ui_row(
        ui_col(
          width = 16,
          rangeInput("path_length", min = 0.1, max = 5, step = 0.1, value = 2),
          tags$p("Path length", class = "range-input-label")
        )
      ),
      ui_row(
        ui_col(
          width = 16,
          rangeInput("step_size", min = 0.01, max = 1, step = 0.01, value = 0.05),
          tags$p("Step size", class = "range-input-label")
        )
      ),
      ui_row(
        ui_col(
          width = 16,
          rangeInput("speed", step = 1, value = 3, labels = c("Slow", "Medium", "Fast")),
          tags$p("Animation speed", class = "range-input-label")
        )
      )
    ),
    tags$div(
      class = "item",
      style = "margin-top: -20px",
      ui_row(tags$p("Actions", class = "sidebar-group-header")),
      ui_row(
        ui_col(
          width = 16,
          shinyWidgets::checkboxGroupButtons(
            inputId = "keep_sampling", choices = "Sample", justified = TRUE
          )
        )
      )
    ),
    tags$div(
        class = "item",
        ui_row(
            ui_col(
              width = 16,
              shiny::actionButton("add_point", "Sample a single point", width = "100%")
            )
        )
    ),
    tags$div(
      class = "item",
      ui_row(
        ui_col(
          width = 16,
          shiny::actionButton("remove_points", "Remove points", width = "100%")
        )
      )
    )
  )
}

tex_panel_1 <- "U (\\boldsymbol{\\theta}) = - \\log[ p^*(\\boldsymbol{\\theta} \\mid \\boldsymbol{y})]"
tex_panel_2 <- "p (\\boldsymbol{\\theta} \\mid \\boldsymbol{y})"

make_body <- function() {
  tags$div(
    style = "margin: 10px",
    tags$h2("How does Hamiltonian Monte Carlo work?", class = "body-header"),
    tags$div(
      class = "plot-container",
      tags$div(
        htmltools::HTML(katex::katex_html(tex_panel_1, preview = FALSE)),
        class = "box",
        rgl::rglwidgetOutput("rglPlot", width = "100%")
      ),
      tags$div(
        htmltools::HTML(katex::katex_html(tex_panel_2, preview = FALSE)),
        class = "box",
        rgl::rglwidgetOutput("rglPlot2", width = "100%")
      )
    ),
    tags$div(
      style = "padding-left: 10px;",
      tags$h2("Details 🤓", class = "details-header"),

      tags$div(
        class = "accordion",
        accordionItem(
          "The Hamiltonian",
          htmltools::HTML(
            katex::render_math_in_html(app_sys("www/assets/hamiltonian.html"), include_css = FALSE)
          )
        ),
        tags$hr(),
        accordionItem(
          "Hamilton's equations",
          htmltools::HTML(
            katex::render_math_in_html(app_sys("www/assets/hamiltons.html"), include_css = FALSE)
          )
        ),
        tags$hr(),
        accordionItem(
          "Leapfrog integrator",
          htmltools::HTML(
            katex::render_math_in_html(app_sys("www/assets/leapfrog.html"), include_css = FALSE)
          )
        ),
        tags$hr(),
        accordionItem(
          "Metropolis acceptance criteria",
          htmltools::HTML(
            katex::render_math_in_html(app_sys("www/assets/accept.html"), include_css = FALSE)
          )
        ),
        tags$hr(),
        accordionItem(
          "Summary",
          htmltools::HTML(
            katex::render_math_in_html(app_sys("www/assets/summary.html"), include_css = FALSE)
          )
        ),
        tags$hr()
      )
    )
  )
}

accordionItem <- function(label, content) {
  tags$div(
    class = "accordion-container",
    tags$div(class = "label", label),
    tags$div(class = "content", content)
  )
}

ui <- function() {
  header <- tags$head(
    tags$title("Shiny HMC"),
    tags$link(rel = "icon", type = "image/png", sizes = "64x64", href = "www/alpine.png"),
    tags$script(type = "text/javascript", src = "www/hmc.js"),
    tags$script(type = "text/javascript", src = "www/sidebar.js"),
    tags$script(type = "text/javascript", src = "www/accordion.js"),
    tags$link(type="text/css", rel = "stylesheet", href = "www/styles.css"),
    tags$link(
      rel = "stylesheet", 
      href = "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css", 
      integrity = "sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH", 
      crossorigin = "anonymous"
    ),
    tags$script(
      src = "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js", 
      integrity = "sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm", 
      crossorigin = "anonymous"
    ),
  )

  body <- tags$body(
    shinyjs::useShinyjs(),
    make_sidebar(),
    make_body()
  )
  
  ui <- do.call(htmltools::tagList, list(body, header))
  htmltools::attachDependencies(ui, shiny::bootstrapLib())
}

# tags$div(
#   class = "item",
#   style = "margin-top:auto",
#   tags$div(
#     class = "ui grid",
#     style = "margin: 0; padding:bottom: 1em",
#     tags$div(
#       class = "row",
#       ui_col(
#         width = 16,
#         style = paste(
#           "font-weight: bold",
#           "text-align: center",
#           sep = ";"
#         ),
#         actionLink(
#           "how_to",
#           "How to use this app?",
#           style = "font-size:16px;",
#           class = "footer-link"
#         )
#       )
#     )
#   )
# )