.onLoad = function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "assets",
    directoryPath = shinymath_file("assets")
  )
}

.onUnload = function(libname, pkgname) {
  shiny::removeResourcePath("assets")
}
