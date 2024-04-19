app_sys <- function(...) {
  system.file(..., package = "shinyhmc", mustWork = TRUE)
}

.onLoad <- function(libname, pkgname) {
  options(rgl.useNULL = TRUE)
  shiny::addResourcePath(
    prefix = "www",
    directoryPath = app_sys("www")
  )
}

.onUnload <- function(libname, pkgname) {
  options(rgl.useNULL = FALSE)
  shiny::removeResourcePath("www")
}