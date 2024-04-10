render_md <- function(path, output_name) {
    command <- paste("pandoc", path, "--katex --from markdown")
    output <- paste(system(command, intern = TRUE), collapse = "\n")
    writeLines(output, output_name)
}

render_md("app/assets/hamiltonian.qmd", "app/assets/hamiltonian.html")
render_md("app/assets/hamiltons.qmd", "app/assets/hamiltons.html")
render_md("app/assets/leapfrog.qmd", "app/assets/leapfrog.html")
render_md("app/assets/accept.qmd", "app/assets/accept.html")
render_md("app/assets/summary.qmd", "app/assets/summary.html")

