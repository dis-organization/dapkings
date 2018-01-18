#' OpenDAP example URL
#'
#' @param ... ignored
#' @return character string, returns a test URL that should work if all is well
#' @export
#' @examples
#' canon_dap_url()
canon_dap_url <- function(...) {
  ## the canonical test URL (?) copied from http://pydap.readthedocs.io/en/latest/client.html#accessing-gridded-data
  "http://test.opendap.org/dap/data/nc/coads_climatology.nc"
}

#' OpenDAP capability
#'
#' Check that we are online and we can connect
#' @param u OpenDAP source string
#' @param ... ignored
#'
#' @seealso canon_dap_url
#' @return logical, TRUE if we have capability
#' @export
#'
#' @examples
#' src_url <- canon_dap_url()
#' have_dap(src_url)
have_dap <- function(u, ...) {
  stopifnot(capabilities("http/ftp"))
  nc_con <- try(RNetCDF::open.nc(u), silent = TRUE)
  !inherits(nc_con, "try-error")
}

#' Open a connection
#'
#' OpenDAP connection
#'
#' @param src OpenDAP source string
#' @param ... arguments passed to `RNetCDF::open.nc`, `share` and `prefill`
#'
#' @return open_dap connection, a NetCDF connection with some extras
#' @export
#'
open_dap <- function(src, ...) {
  stopifnot(have_dap(src))
  con <- RNetCDF::open.nc(src, ...)
  structure(con, class = c("open_dap", "NetCDF"), source = src)
}

#' Revert to NetCDF connection class
#'
#' @param x open_dap connection
#' @param ... ignored
#'
#' @return NetCDF connection
#' @export
as_nc <- function(x, ...) {
  class(x) <- "NetCDF"
  x
}
#' @name open_dap
#' @export
print.open_dap <- function(x, ...)  {
  is_open <- dap_live(x)
  cat("OpenDAP connection\n")
  cat(sprintf("source: %s\n", attr(x, "source")))
  cat(sprintf("open: %s\n", is_open))
  if (is_open) {
    cat("variables:\n")
    print(ncmeta::nc_vars(as_nc(x)))
    cat("dimensions:\n")
    print(ncmeta::nc_dims(as_nc(x)))

  }
}
#' @name open_dap
#' @export
dap_live <- function(x, ...) {
  chk <- try(RNetCDF::file.inq.nc(as_nc(x)), silent = TRUE)
  if (inherits(chk, "try-error")) return(FALSE)
  all(c("ndims", "nvars", "ngatts", "unlimdimid") %in% names(chk))
}
