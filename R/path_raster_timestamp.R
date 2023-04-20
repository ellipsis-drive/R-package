#' Service we deliver: We return an R raster object
#' @export
path.raster.timestamp.getRaster <- function(pathId, timestampId, extent, style = NULL, threads = 1, token = NULL, showProgress = TRUE, epsg = 3857)
{
  bounds <- extent
  threads <- validInt("threads", threads, TRUE)
  token <- validString("token", token, FALSE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  bounds <- validBounds("bounds", bounds, TRUE)
  style <- validObject("style", style, FALSE)
  showProgress <- validBool("showProgress", showProgress, TRUE)
  xMin <- bounds[["xMin"]]
  yMin <- bounds[["yMin"]]
  xMax <- bounds[["xMax"]]
  yMax <- bounds[["yMax"]]
  epsg_string <- toString(epsg)
  res <- getActualExtent(xMin, xMax, yMin, yMax, glue::glue("EPSG:{epsg_string}"))
  if (res[["status"]] == 400)
    stop("ValueError: Invalid epsg and extent combination")

  bounds <- res[["message"]]
  xMinWeb <- bounds[["xMin"]]
  xMaxWeb <- bounds[["xMax"]]
  yMinWeb <- bounds[["yMin"]]
  yMaxWeb <- bounds[["yMax"]]
  info <- httr::content(apiManager_get(glue::glue("/path/{pathId}"), NULL, token))
  bands <- info[["raster"]][["bands"]]
  if (is.null(style))
  {
    num_bands <- length(bands)
    dtype <- info[["raster"]][["format"]]
  }
  else
  {
    num_bands <- 4
    dtype <- "uint8"
  }

  timestamps <- info[["raster"]][["timestamps"]]
  all_timestamps <- list()
  for (item in timestamps)
    all_timestamps <- append(all_timestamps, item[["id"]])

  if (!(toString(timestampId) %in% all_timestamps))
    stop("ValueError: given timestamp does not exist")

  zoom <- NULL
  found <- FALSE
  for (item in timestamps)
  {
    if (item[["id"]] == timestampId)
    {
      # No NULL checking??
      zoom <- item[["zoom"]]
    }
  }
  body <- list("style" = style)
  LEN <- 2.003751e+07

  x_start <- 2**zoom * (xMinWeb + LEN) / (2*LEN)
  x_end <- 2**zoom * (xMaxWeb + LEN) / (2*LEN)
  y_end <- 2**zoom * (LEN - yMinWeb) / (2*LEN)
  y_start <- 2**zoom * (LEN - yMaxWeb) / (2*LEN)

  x1_osm <- floor(x_start)
  x2_osm <- floor(x_end)
  y1_osm <- floor(y_start)
  y2_osm <- floor(y_end)

  x_tiles <- as.list(seq(x1_osm, x2_osm))
  y_tiles <- as.list(seq(y1_osm, y2_osm))

  r_total <- array(0, c(256*(y2_osm - y1_osm + 1) ,256*(x2_osm - x1_osm + 1), num_bands))

  tiles = c()
  for (tileY in y_tiles)
    for (tileX in x_tiles)
      tiles <- append(tiles, list(c(tileX, tileY)))
  subTiles <- function(tiles)
  {
    N <- 0
    for (tile in tiles)
    {
      tileX <- tile[[1]]
      tileY <- tile[[2]]
      x_index <- tileX - x1_osm
      y_index <- tileY - y1_osm

      zoom_string <- toString(zoom)
      tileX_string <- toString(tileX)
      tileY_string <- toString(tileY)

      r <- apiManager_get(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}/tile/{zoom_string}/{tileX_string}/{tileY_string}"), body, token, FALSE)
      if (httr::status_code(r) == 403)
      {
        stop("ValueError: insufficient access")
      }
      if (httr::status_code(r) != 200)
      {
        r <- array(0, c(256, 256, num_bands))
      }
      else
      {
        r <- raster::stack(raster::brick(httr::content(r)))
        #if (!is.null(style))
        #  r <- raster::t(r)
      }
      r_total[seq((y_index*256)+1,((y_index+1)*256)),seq((x_index*256)+1,((x_index+1)*256)),] <- raster::as.array(r)
      # Add progress bar later
      N <- N + 1
    }
    return(r_total)
  }
  # Multithread later on
  r_total <- subTiles(tiles)

  min_x_index <- as.integer(floor((x_start - x1_osm)*256))
  max_x_index <- max(as.integer(floor((x_end- x1_osm)*256 + 1 )), min_x_index + 1 )
  min_y_index <- as.integer(floor((y_start - y1_osm)*256))
  max_y_index <- max(as.integer(floor((y_end- y1_osm)*256 +1)), min_y_index + 1)
  r_total <- r_total[seq(min_y_index,max_y_index),seq(min_x_index,max_x_index),]

  mercatorExtent <- list('xMin' = xMinWeb, 'yMin'= yMinWeb, 'xMax'= xMaxWeb, 'yMax'= yMaxWeb)
  if (epsg == "3857")
  {
    trans <- affineFromBounds(xMinWeb, yMinWeb, xMaxWeb, yMaxWeb, dim(r_total)[3], dim(r_total)[2])
    raster_object <- raster::brick(r_total)
    raster::crs(raster_object) = "+init=epsg:3857"
    raster::extent(raster_object) <- raster::extent(xMinWeb, xMaxWeb, yMinWeb, yMaxWeb)
    return(list("raster" = raster_object, "transform" = trans, "extent" = mercatorExtent, "epsg" = 3857))
  }
  else
  {
    trans <- affineFromBounds(extent[["xMin"]], extent[["yMin"]], extent[["xMax"]], extent[["yMax"]], dim(r_total)[3], dim(r_total)[2])
    #raster_object <- recolorize::array_to_RasterStack(r_total, type = "stack")
    raster_object <- raster::stack(raster::brick(r_total))
    plot.new()
    raster::crs(raster_object) = glue::glue("+init=epsg:{epsg_string}")
    raster::extent(raster_object) <- raster::extent(extent[["xMin"]], extent[["xMax"]], extent[["yMin"]], extent[["yMax"]])
    raster_object <- raster::projectRaster(raster_object, crs=glue::glue("+init=epsg:{epsg}"), method="ngb", resolution = 1000)
    # TODO: extent might be skewed and add bilinear and resolutioun options
    return(list("raster" = raster_object, "transform" = trans, "extent" = extent, "epsg" = epsg))
  }
}

#' @export
path.raster.timestamp.add <- function(pathId, token, description = NULL, date = list("from" = Sys.time(), "to" = Sys.time()))
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  date <- validDateRange("date", date, TRUE)
  description <- validString("description", description, FALSE)
  body = list("date" = date, "description" = description)
  return(apiManager_post(glue::glue("/path/{pathId}/raster/timestamp"), body, token))
}


