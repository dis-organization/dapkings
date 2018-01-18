# example URL
canon_dap_url <- function(...) {
  ## the canonical test URL (?) copied from http://pydap.readthedocs.io/en/latest/client.html#accessing-gridded-data
  "http://test.opendap.org/dap/data/nc/coads_climatology.nc"
}
# we are online and we can connect
have_dap <- function(u, ...) {
  stopifnot(capabilities("http/ftp"))
  nc_con <- try(RNetCDF::open.nc(u), silent = TRUE)
  !inherits(nc_con, "try-error")
}
