library(ncdf4)
nc <- nc_open("http://tds.hycom.org/thredds/dodsC/GLBu0.08/expt_91.1/ts3z")

lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
depth <- ncvar_get(nc, "depth")

#units: hours since 2000-01-01 00:00:00
time <- (ncvar_get(nc, "time") * 3600) + as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
range(time)
#[1] "2014-04-07 UTC" "2015-09-07 UTC"

readhycom <- function(date, xlim = NULL, ylim = NULL, zlim = NULL) {
  date <- as.POSIXct(date, tz = "UTC")
  tind <- which.min(abs(time - date))
  xind <- c(1, length(lon))
  yind <- c(1, length(lat))
  zind <- c(1, length(depth))
  if (!is.null(xlim)) {
     xlim <- (xlim + 180) %% 180 + 180

    xind <- c(findInterval(xlim[1], lon), findInterval(xlim[2], lon))
    }
  if (!is.null(ylim)) yind <- c(findInterval(ylim[1], lat), findInterval(ylim[2], lat))
  if (!is.null(zlim)) zind <- c(findInterval(zlim[1], depth), findInterval(zlim[2], depth))
  
  a <- ncvar_get(nc, "water_temp", start = c(xind[1], yind[1], zind[1], tind[1]), count = c(diff(xind) + 1, diff(yind) + 1, diff(zind) + 1, 1))
  xx <- lon[xind[1]:xind[2]]
  yy <- lat[yind[1]:yind[2]]
  setZ(brick(a, xmn = min(xx), xmx = max(xx), ymn = min(yy), ymx = max(yy), transpose = TRUE, crs = "+proj=longlat +ellps=WGS84 +over"), depth)
 }
 
x <-  readhycom("2015-01-01", xlim = c(-70, -30), ylim = c(-70, -45))
 


