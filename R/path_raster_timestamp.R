# TODO: during tests, see if crs transformations in analyse and alongline work out

#' Service we deliver: We return an R raster object
#' Get a downsampled raster
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param extent Mandatory (named list) a named list with properties xMin, xMax, yMin, yMax of type double
#' @param width Mandatory (int)
#' @param height Mandatory (int)
#' @param style Optional (uuid or named list describing a style ) If no style given raw data is returned. Also see https://docs.ellipsis-drive.com/developers/api-v3/path-raster/styles/add-style
#' @param epsg Optional (int) default 3857 (webmercator)
#' @param token Optional (string)
#' @return Named list with property "raster" of type R raster object containing the downsampled raster
#' @export
path.raster.timestamp.getDownsampledRaster <- function(pathId, timestampId, extent, width, height, epsg=3857, style = NULL, token = NULL)
{
  return(path.raster.timestamp.getSampledRaster(pathId, timestampId, extent, width, height, epsg, style, token))
}

#' Get a downsampled raster
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param extent Mandatory (named list) a named list with properties xMin, xMax, yMin, yMax of type double
#' @param width Mandatory (int)
#' @param height Mandatory (int)
#' @param style Optional (uuid or named list describing a style ) If no style given raw data is returned. Also see https://docs.ellipsis-drive.com/developers/api-v3/path-raster/styles/add-style
#' @param epsg Optional (int) default 3857 (webmercator)
#' @param token Optional (string)
#' @return Named list with property "raster" of type R raster object containing the downsampled raster
#' @export
path.raster.timestamp.getSampledRaster <- function(pathId, timestampId, extent, width, height, epsg = 3857, style = NULL, token = NULL)
{
  bounds <- extent
  token <- validString("token", token, FALSE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  bounds <- validBounds("bounds", bounds, TRUE)
  style <- validObject("style", style, FALSE)
  epsg <- validInt("epsg", epsg, TRUE)

  body <- list("pathId" = pathId, "timestampId" = timestampId, "extent" = bounds, "width" = width, "height" = height, "style" = style, "epsg" = epsg)

  r <- apiManager_get(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}/rasterByExtent"), body, token, crash = FALSE)
  if (httr::status_code(r) != 200)
  {
    message <- httr::http_status(r)[["message"]]
    stop(glue::glue("ValueError: {message}"))
  }
  r <- raster::stack(raster::brick(httr::content(r)))

  xMin <- bounds[["xMin"]]
  yMin <- bounds[["yMin"]]
  xMax <- bounds[["xMax"]]
  yMax <- bounds[["yMax"]]

  trans <- affineFromBounds(xMin, yMin, xMax, yMax, dim(r)[3], dim(r)[2])

  return(list("raster" = r, "transform" = trans, "extent" = list("xMin" = xMin, "yMin" = yMin, "xMax" = xMax, "yMax" = yMax), "crs" = glue::glue("EPSG:{epsg}")))
}

#' Request to obtain the raster value for each point along a line
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param line Mandatory (sf geometry) of type line in WGS84
#' @param asRaster Optional (logical) default FALSE, if TRUE, an R raster object is returned instead of a matrix
#' @param epsg Optional (int) default 4326
#' @param token Optional (string)
#' @return A named list with a "raster" property of type Raster, or Matrix depending on asRaster parameter containing the raster for points along a line
#' @export
getValuesAlongLine <- function(pathId, timestampId, line, token = NULL, epsg = 4326, asRaster = FALSE)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, FALSE)
  line <- validSFGeometry("line", line, TRUE)
  asRaster <- validBool("asRaster", asRaster, TRUE)

  if(class(line)[[2]] != "LINESTRING")
    stop("ValueError: line must be a LINESTRING simple feature geometry")

  temp <- sf::st_sf(geometry = sf::st_sfc(line))
  temp <- sf::st_set_crs(temp, glue::glue("EPSG:{epsg}"))
  temp <- sf::st_set_crs(temp, "EPSG:4326")

  extent <- list("xMin" = sf::st_bbox(temp)[["xmin"]], "yMin" = sf::st_bbox(temp)[["ymin"]], "xMax" = sf::st_bbox(temp)[["xmax"]], "yMax" = sf::st_bbox(temp)[["ymax"]])
  size <- 1000

  r <- path.raster.timestamp.getSampledRaster(pathId = pathId, timestampId = timestampId, extent = extent, width = size, height = size, epsg = 4326)
  raster <- r[["raster"]]

  # Maybe first store raster in memory???
  values <- raster::sampleRandom(raster, size = 1000, asRaster = asRaster)

  return(values)
}

#' Get a Raster
#' @param extent Mandatory (named list) named list with properties xMin, xMax, yMin, yMax of type double
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Optional (string)
#' @param style Optional (uuid or named list describing a style ) If no style given raw data is returned. Also see https://docs.ellipsis-drive.com/developers/api-v3/path-raster/styles/add-style
#' @return An R Raster object containing the raster
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
    raster_object <- raster::stack(raster::brick(r_total))
    raster::crs(raster_object) = glue::glue("+init=epsg:{epsg_string}")
    raster::extent(raster_object) <- raster::extent(extent[["xMin"]], extent[["xMax"]], extent[["yMin"]], extent[["yMax"]])
    raster_object <- raster::projectRaster(raster_object, crs=glue::glue("+init=epsg:{epsg}"), method="ngb", resolution = 1000)
    return(list("raster" = raster_object, "transform" = trans, "extent" = extent, "epsg" = epsg))
  }
}

#' Request to obtain pixel values within a certain geometry
#' @param pathId Mandatory (uuid)
#' @param timestampIds Mandatory (vector, list, or array of uuids)
#' @param geometry Mandatory (simple feature geometry) in WGS84
#' @param approximate Optional (logical) default TRUE
#' @param token Optional (string)
#' @param returnType Optional (string) either "all" or "statistics", default "all"
#' @return ...
#' @export
path.raster.timestamp.analyse <- function(pathId, timestampIds, geometry, returnType = "all", approximate = TRUE, token = NULL, epsg = 4326)
{
  token <- validString("token", token, FALSE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampIds <- validUuidArray("timestampIds", timestampIds, TRUE)
  approximate <- validBool("approximate", approximate, TRUE)
  geometry <- validSFGeometry("geometry", geometry, TRUE)
  returnType <- validString("returnType", returnType, TRUE)

  temp <- sf::st_sf(geometry = sf::st_sfc(geometry))
  temp <- sf::st_set_crs(temp, glue::glue("EPSG:{epsg}"))
  temp <- sf::st_set_crs(temp, "EPSG:4326")
  geometry <- temp$geometry

  sh <- tryCatch(
    {
      sh <- geojsonsf::sf_geojson(temp)
      sh <- sh[[1]]
    }, error=function(cond)
    {
      stop("ValueError: geometry must be a valid simple features geometry")
    }
    )

  body <- list("timestampIds" = timestampIds, "geometry" = geometry, "approximate" = approximate, "returnType" = returnType)
  r <- apiManager_get(glue::glue("/path/{pathId}/raster/timestamp/analyse"), body, token)
  r <- httr::content(r)
  return(r)
}

#' Add a timestamp to a raster
#' @param token Mandatory (string)
#' @param pathId Mandatory (uuid)
#' @param date Optional (named list) named list containing properties "to" and "from" both of type date
#' @param description Optional (string)
#' @return ...
#' @export
path.raster.timestamp.add <- function(pathId, token, description = NULL, date = list("from" = Sys.time(), "to" = Sys.time()))
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  date <- validDateRange("date", date, TRUE)
  description <- validString("description", description, FALSE)
  body = list("date" = date, "description" = description)
  return(httr::content(apiManager_post(glue::glue("/path/{pathId}/raster/timestamp"), body, token)))
}

#' Edit a timestamp of a raster map
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Mandatory (string)
#' @param date Optional (named list) with properties "to" and "from" both of type date
#' @param description Optional (string)
#' @return ...
#' @export
path.raster.timestamp.edit <- function(pathId, timestampId, token, date = NULL, description = NULL)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  description <- validString("description", description, FALSE)
  date <- validDateRange("date", date, FALSE)

  body = list("date" = date, "description" = description)
  r <- apiManager_patch(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}"), body, token)
  r <- httr::content(r)
  return(r)
}

#' Request to obtain the aggregated data for a certain geometry
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Optional (string)
#' @return simple feature dataframe (sf) containing the bounds
#' @export
path.raster.timestamp.getBounds <- function(pathId, timestampId, token = NULL)
{
  token <- validString("token", token, FALSE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)

  r <- apiManager_get(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}/bounds"), NULL, token)
  r <- httr::content(r)
  r <- sf::st_sf(id = 0, properties = list(), geometry = r)
  return(r)
}

#' Activate a timestamp
#' @param token Mandatory (string)
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @return ...
#' @export
path.raster.timestamp.activate <- function(pathId, timestampId, token)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)

  r <- apiManager_post(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}/activate"), NULL, token)
  r <- httr::content(r)
  return(r)
}

#' Deactivate a given timestamp
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Mandatory (string)
#' @return ...
#' @export
path.raster.timestamp.deactivate <- function(pathId, timestampId, token)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)

  r <- apiManager_post(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}/deactivate"), NULL, token)
  r <- httr::content(r)
  return(r)
}

#' Move a given timestamp to the trash
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Mandatory (string)
#' @return ...
#' @export
path.raster.timestamp.trash <- function(pathId, timestampId, token)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)

  body = list("trashed" = TRUE)
  r <- apiManager_put(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}/trashed"), body, token)
}

#' Recover a given timestamp from the trash
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Mandatory (string)
#' @return ...
#' @export
path.raster.timestamp.recover <- function(pathId, timestampId, token)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)

  body = list("trashed" = FALSE)
  r <- apiManager_put(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}/trashed"), body, token)
}
