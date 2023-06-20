#' Adds a vector timestamp
#' @param pathId Mandatory (uuid)
#' @param token Mandatory (string) your token
#' @param date Optional (named list with names to and from both of type date)
#' @param description Optional (string)
#' @return ...
#' @export
path.vector.timestamp.add <- function(pathId, token, properties = NULL, description = NULL, date = list("from" = Sys.time(), "to" = Sys.time()))
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, TRUE)
  properties <- validObject("properties", properties, FALSE)
  description <- validString("description", description, FALSE)
  date <- validDateRange("date", date, TRUE)

  body <- list("properties" = properties, "date" = date, "description" = description)
  r <- httr::content(apiManager_post(glue::glue("/path/{pathId}/vector/timestamp"), body, token))
  return(r)
}

#' Edits a vector timestamp
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Mandatory (string) your token
#' @param date Optional (named list with names to and from both of type date)
#' @param description Optional (string)
#' @return ...
#' @export
path.vector.timestamp.edit <- function(pathId, timestampId, token, description = NULL, date = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  description <- validString("description", description, FALSE)
  date <- validDateRange("date", date, FALSE)

  body <- list("date" = date, "description" = description)
  r <- httr::content(apiManager_patch(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}"), body, token))
  return(r)
}

#' Adds a vector timestamp to the trash
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Mandatory (string)
#' @return ...
#' @export
path.vector.timestamp.trash <- function(pathId, timestampId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, TRUE)
  body <- list("trashed"=TRUE)
  r <- httr::content(apiManager_put(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/trashed"), body, token))
}

#' Recovers a vector timestamp
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Mandatory (string)
#' @return ...
#' @export
path.vector.timestamp.recover <- function(pathId, timestampId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, TRUE)
  body <- list("trashed"=FALSE)
  r <- httr::content(apiManager_put(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/trashed"), body, token))
}

#' Delete a vector timestamp
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Mandatory (string)
#' @return ...
#' @export
path.vector.timestamp.delete <- function(pathId, timestampId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, TRUE)
  r <- apiManager_delete(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}"), NULL, token)
  return(httr::content(r))
}

#' Activate a timestamp
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Mandatory (string)
#' @return ...
#' @export
path.vector.timestamp.activate <- function(pathId, timestampId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, TRUE)
  r <- apiManager_post(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/activate"), NULL, token)
  return(httr::content(r))
}

#' Deactivate a timestamp
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Mandatory (string)
#' @return ...
#' @export
path.vector.timestamp.deactivate <- function(pathId, timestampId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, TRUE)
  r <- apiManager_post(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/deactivate"), NULL, token)
  return(httr::content(r))
}

#' Get the bounds of a vector timestamp
#' @param pathId Mandatory (uuid)
#' @param timestapmId Mandatory (uuid)
#' @param token Optional (string)
#' @return a simple feature sf object
#' @export
path.vector.timestamp.getBounds <- function(pathId, timestampId, token = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, FALSE)

  r <- httr::content(apiManager_get(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/bounds"), NULL, token))
  coordinates <- unlist(r[["coordinates"]][[1]], recursive = FALSE)
  matrix_coordinates <- matrix(unlist(coordinates), ncol = 2, byrow = TRUE)
  geometry <- sf::st_polygon(list(matrix_coordinates))
  sf_object <- sf::st_sf(id = 0, geometry = sf::st_sfc(geometry))
  return(sf_object)
}

#' Get the changelog of a vector timestamp
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param listAll Mandatory (logical)
#' @param token Optional (string)
#' @param actions Optional (object)
#' @param pageStart optional (object)
#' @return ...
#' @export
path.vector.timestamp.getChanges <- function(pathId, timestampId, token = NULL, pageStart = NULL, listAll = FALSE, actions = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, FALSE)
  listAll <- validBool("listAll", listAll, TRUE)
  pageStart <- validObject("pageStart", pageStart, FALSE)
  actions <- validObject("actions", actions, FALSE)
  body <- list("pageStart" = pageStart)
  f <- function(body)
  {
    r <- apiManager_get(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/changelog"), body, token)
    r <- httr::content(r, as = "parsed")

    return(r)
  }

  r <- recurse(f, body, listAll)

  temp_list = list()

  for (x in r[["result"]])
  {
    temp_list <- append(temp_list, list(x, "date" = stringToDate(x[["date"]])))
  }
  r[["result"]] <- temp_list

  return(r)
}

#' Get features of a vector timestamp by the id of the features
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param featureIds Mandatory (array of uuids)
#' @param showProgress Mandatory (logical)
#' @param token Mandatory (string)
#' @return a simple features 'sf' object containing the features as a geometry
#' @export
path.vector.timestamp.getFeaturesByIds <- function(pathId, timestampId, featureIds, token = NULL, showProgress = FALSE)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, FALSE)
  featureIds <- validUuidArray("featureIds", featureIds, TRUE)
  showProgress <- validBool("showProgress", showProgress, TRUE)

  id_chunks = chunks(featureIds, 10)
  r <- list("size" = 0, "result" = list(), "nextPageStart" = NULL)

  pandas <- reticulate::import("pandas")
  for (ids in id_chunks)
  {
    body <- list("geometryIds" = ids)
    r_new <- apiManager_get(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/featuresByIds"), body, token)
    r_new <- httr::content(r_new)
    r[["result"]] <- append(r[["result"]], r_new[["result"]][["features"]])
    r[["size"]] <- r[["size"]] + r_new[["size"]]
  }

  feature <- r[["result"]][[1]]
  coordinates <- unlist(feature[["geometry"]][["coordinates"]], recursive = FALSE)
  matrix_coordinates <- matrix(unlist(coordinates), ncol = 2, byrow = TRUE)
  geometry <- sf::st_polygon(list(matrix_coordinates))
  feature$properties$geometry <- NULL
  sh <- sf::st_sf(id = 0, properties = feature[["properties"]], geometry = sf::st_sfc(geometry))
  count <- 0
  for (feature in r[["result"]])
  {
    if (count == 0)
    {
      count <- count +1
      next
    }
    coordinates <- unlist(feature[["geometry"]][["coordinates"]], recursive = FALSE)
    matrix_coordinates <- matrix(unlist(coordinates), ncol = 2, byrow = TRUE)
    matrix_coordinates <- rbind(matrix_coordinates, matrix_coordinates[1,])
    geometry <- sf::st_polygon(list(matrix_coordinates))
    sf_object <- sf::st_sf(id = count, properties = feature[["properties"]], geometry = sf::st_sfc(geometry))
    sh <- rbind(sh, sf_object)
    count <- count + 1
  }
  r[["result"]] <- sh

  return(r)
}

#' Get features of a vector timestamp by the extent of the features
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param extent Mandatory (named list) with names xMin xMax yMin yMax of tyoe double
#' @param token Optional (string)
#' @param propertyFilter Optional (object)
#' @param pageStart Optional (object)
#' @param listAll Optional (logical) whether to list all results (default TRUE)
#' @return a simple features 'sf' object containing the features as a geometry
#' @export
path.vector.timestamp.getFeaturesByExtent <- function(pathId, timestampId, extent, propertyFilter = NULL, token = NULL, listAll = TRUE, pageStart = NULL, epsg = 4326, coordinateBuffer = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, FALSE)
  extent <- validBounds("extent", extent, TRUE)
  propertyFilter <- validObject("propertyFilter", propertyFilter, FALSE)
  listAll <- validBool("listAll", listAll, TRUE)
  pageStart <- validObject("pageStart", pageStart, FALSE)
  coordinateBuffer <- validFloat("coordinateBuffer", coordinateBuffer, FALSE)

  if (is.null(coordinateBuffer))
  {
    info <- path.get(pathId, token)
    ts <- list()
    for (x in info[["vector"]][["timestamps"]])
    {
      if (x[["id"]] == timestampId)
      {
        ts <- append(ts, list(x))
      }
    }
    if (length(ts) == 0)
      stop("ValueError: Given timestampId does not exist")
    t <- ts[[1]]
    zoom <- t[["zoom"]]
    coordinateBuffer <- .5*360/ 2 ** zoom
  }
  minx <- extent[["xMin"]]
  maxx <- extent[["xMax"]]
  miny <- extent[["yMin"]]
  maxy <- extent[["yMax"]]
  p <- sf::st_polygon(list(rbind(c(extent$xMin, extent$yMin),
                                 c(extent$xMin, extent$yMax),
                                 c(extent$xMax, extent$yMin),
                                 c(extent$xMax, extent$yMax),
                                 c(extent$xMin, extent$yMin))))
  p <- sf::st_sf(sf::st_sfc(p))

  res <- getActualExtent(extent[["xMin"]], extent[["xMax"]], extent[["yMin"]], extent[["yMax"]], glue::glue("EPSG:{epsg}"))
  if (res[["status"]] == 400)
    stop("ValueError: Invalid epsg and extent combination")

  extent <- res[["message"]]

  extent[["xMin"]] <- min(-180, extent[["xMin"]] - coordinateBuffer)
  extent[["xMax"]] <- max(180, extent[["xMax"]] + coordinateBuffer)
  extent[["yMin"]] <- min(-85, extent[["yMin"]] - coordinateBuffer)
  extent[["yMax"]] <- max(85, extent[["yMax"]] + coordinateBuffer)
  print(extent)
  p <- tryCatch(
    {
      sf::st_crs(p) <- 4326
      p <- sf::st_transform(p, 4326)
    },
    error = function(cond)
    {
      stop("ValueError: Invalid crs given")
    }
  )

  extent <- sf::st_bbox(p)
  extent = list("xMin" = extent[[1]], "xMax" = extent[[3]], "yMin" = extent[[2]], "yMax" = extent[[4]])

  body <- list("pageStart" = pageStart, "propertyFilter" = propertyFilter, "extent" = extent)
  f <- function(body)
  {
    r <- apiManager_get(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/featuresByExtent"), body, token)
    r <- httr::content(r, as= "parsed")
    return(r)
  }

  r <- recurse(f, body, listAll, extraKey = "features")
  if (length(r[["result"]][["features"]]) < 1)
    return(NULL)

  feature <- r[["result"]][["features"]][[1]]
  coordinates <- unlist(feature[["geometry"]][["coordinates"]], recursive = FALSE)
  matrix_coordinates <- matrix(unlist(coordinates), ncol = 2, byrow = TRUE)
  geometry <- sf::st_polygon(list(matrix_coordinates))
  feature$properties$geometry <- NULL
  sh <- sf::st_sf(id = 0, properties = feature[["properties"]], geometry = sf::st_sfc(geometry))
  count <- 0
  for (feature in r[["result"]][["features"]])
  {
    if (count == 0)
    {
      count <- count +1
      next
    }
    coordinates <- unlist(feature[["geometry"]][["coordinates"]], recursive = FALSE)
    matrix_coordinates <- matrix(unlist(coordinates), ncol = 2, byrow = TRUE)
    matrix_coordinates <- rbind(matrix_coordinates, matrix_coordinates[1,])
    geometry <- sf::st_polygon(list(matrix_coordinates))
    sf_object <- sf::st_sf(id = count, properties = feature[["properties"]], geometry = sf::st_sfc(geometry))
    sh <- rbind(sh, sf_object)
    count <- count + 1
  }
  sh <- sf::st_set_crs(sh, 4326)


  if (dim(sh)[[1]] == 0)
  {
    r[["result"]] <- sh
    return(r)
  }

  bounds <- sf::st_bbox(sh)
  px <- (bounds[["xmin"]] + bounds[["xmax"]]) / 2
  py <- (bounds[["ymin"]] + bounds[["ymax"]]) / 2
  # sh <- sh[sf::st_within(sh, sf::st_as_sfc(sf::st_bbox(c(extent['xMin'], extent['yMin'], extent['xMax'], extent['yMax']))))]
  sh <- sf::st_set_crs(sh, epsg)
  r[["result"]] <- sh
  return(r)
}

#' List available features of a vector timestamp
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Optional (string)
#' @return a simple features 'sf' object containing the features and the geometry. Append "properties." ("properties.id" for the featureId for instance) to the requested property
#' @export
path.vector.timestamp.listFeatures <- function(pathId, timestampId, token = NULL, listAll = TRUE, pageStart = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, FALSE)
  listAll <- validBool("listAll", listAll, TRUE)
  pageStart <- validObject("pageStart", pageStart, FALSE)

  body <- list("pageStart" = pageStart)

  f <- function(body)
  {
    return(httr::content(apiManager_get(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/listFeatures"), body, token)))
  }

  r <- recurse(f, body, listAll, "features")
  feature <- r[["result"]][["features"]][[1]]
  coordinates <- unlist(feature[["geometry"]][["coordinates"]], recursive = FALSE)
  matrix_coordinates <- matrix(unlist(coordinates), ncol = 2, byrow = TRUE)
  geometry <- sf::st_polygon(list(matrix_coordinates))
  feature$properties$geometry <- NULL
  sh <- sf::st_sf(id = 0, properties = feature[["properties"]], geometry = sf::st_sfc(geometry))
  count <- 0
  for (feature in r[["result"]][["features"]])
  {
    if (count == 0)
    {
      count <- count +1
      next
    }
    coordinates <- unlist(feature[["geometry"]][["coordinates"]], recursive = FALSE)
    matrix_coordinates <- matrix(unlist(coordinates), ncol = 2, byrow = TRUE)
    matrix_coordinates <- rbind(matrix_coordinates, matrix_coordinates[1,])
    geometry <- sf::st_polygon(list(matrix_coordinates))
    sf_object <- sf::st_sf(id = count, properties = feature[["properties"]], geometry = sf::st_sfc(geometry))
    sh <- rbind(sh, sf_object)
    count <- count + 1
  }
  sh <- sf::st_set_crs(sh, 4326)
  r[["result"]] <- sh
  return(r)
}
