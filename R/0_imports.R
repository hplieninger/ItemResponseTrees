.onLoad <- function(libname, pkgname) {
    backports::import(pkgname, c("isTRUE", "isFALSE"))
}
