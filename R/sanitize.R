#library(glue)
#source("R/util/root.R")

#TODO: Check when list is used, vector does not cause errors

validUuid <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  value <- tryCatch(
    {
      value = uuid::as.UUID(value)
    },
    error=function(cond)
    {
      stop(glue::glue("{name} must be of type string and be a uuid"))
    },
    warning=function(cond)
    {
      stop(glue::glue("{name} must be of type string and be a uuid"))
    },
    finally=
    {

    }
  )
  return(value)
}

validResolution <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  if (!isNamedList(value))
    stop(glue::glue("Value error: {name} must be a named list with properties min and max as double"))

  if (!"min" %in% names(value) || !"max" %in% names(value))
    stop(glue::glue("Value error: {name} must be a named list with properties min and max as double"))

  value <- tryCatch(
    {
      value <- list("min" = as.double(value[["min"]]), "max" = as.double(value[["max"]]))
    },
    error=function(cond)
    {
      stop(glue::glue("Value error: {name} must be a named list with properties min and max as double"))
    })

  return(value)
}

validDateRange <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  if (!isNamedList(value) || !"from" %in% names(value) || !"to" %in% names(value))
    stop(glue::glue("Value error: date must be a named list with properties from and to that should be of type datetime"))

  dateFrom <- value[["from"]]
  dateTo <- value[["to"]]

  dateFrom <- validDate("dateFrom", dateFrom, FALSE)
  dateTo <- validDate("dateTo", dateTo, FALSE)
  return(list("from" = dateFrom, "to" = dateTo))
}

validUuidArray <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  if (!is.list(value) || !is.vector(value))
    stop("ValueError: {name} must be an iterable of uuids")

  for (Uuid in value)
  {
    Uuid <- validUuid("uuid", Uuid, TRUE)
  }
  return(value)
}

validString <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  if (!isSingleString(value))
    stop(glue::glue("Value error: {name} must be of type string"))
  return(value)
}

validSFGeometry <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  if (!inherits(value, "sf"))
    stop(glue::glue("ValueError: {name} must be a simple feature geometry"))
  return(value)
}

validBoundCrsCombination <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  bounds <- value[1]
  crs <- value[2]

  value <- bounds
  keys <- list("xMin", "xMax", "yMin", "yMax")
  if (!isNamedList(value))
    stop(glue::glue("ValueError: {name} must be a dictionary with keys xMin, xMax, yMin, yMax, whose values must be of type float"))

  for (key in keys)
  {
    if (!key %in% names(value) || (!is.numeric(value[[key]]) & length(value[[key]]) <= 0))
      stop(glue::glue("ValueError: {name} must be a dictionary with keys xMin, xMax, yMin, yMax, whose values must be of type float"))
    value[[key]] = as.double(value[[key]])
  }

  if(!isSingleString(crs))
    stop(glue::glue("ValueError: crs must be of type string"))

  target_tile <- sf::st_polygon(x = c(c(bounds[["xMin"]], bounds[["yMin"]]), c(bounds[["xMin"]], bounds[["yMax"]]), c(bounds[["xMax"]], bounds[["yMax"]]), c(bounds[["xMax"]], bounds[["yMin"]])))
  sh <- sf::st_sf(geometry = target_tile)

  d <- tryCatch(
    {
      sf::st_crs(sh) <- sf::st_crs(crs)
    },
    error = function(cond)
    {
      error_var1 <- name[1]
      error_var2 <- name[2]
      stop(glue::glue("ValueError: {error_var} does not fit with the projection given by {error_var2}"))
    }
  )
  return(list(bounds, crs))
}

validImage <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  if (!is.matrix(value) || !is.array(value))
    stop(glue::glue("Value error: {name} must be of type array or matrix"))

  if (length(dim(value)) != 2 && length(dim(value)) != 3)
    stop(glue::glue("Value error: {name} must be of type array or matrix with either 2 or 3 dimensions"))

  if (length(dim(value)) == 3 && dim(value)[[3]] != 3 && dim(value)[[3]] != 1)
    stop(glue::glue("Value error: {name} must have either 1 or 3 bands"))

  return(value)
}

validDataframe <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  if (!is.data.frame(value))
    stop(glue::glue("Value error: {name} must be a data frame"))

  return(value)
}

validList <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)
  if (!is.list(value) || !is.vector(value))
    stop(glue::glue("ValueError: {name} must be a vector or a list"))

  return(value)
}

validGeoSeries <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  value <- tryCatch(
    {
      value <- sf::st_sf(sf::st_sfc(value))
    },
    error = function(cond)
    {
      stop(glue::glue("Value error: {name} must be a valid list of geometries"))
    }
  )
  value <- validSimplefeature(name, value, TRUE, TRUE)
  return(value)
}

validSimplefeature <- function(name, value, required, custom = FALSE, cores = 1)
{
  if (!required & is.null(value))
    return(NULL)

  if (!custom & class(value)[[2]] != "data.frame")
    stop(glue::glue("Value error: {name} must be a simple features dataframe"))

  if (is.na(sf::st_crs(value)) & min(sf::st_bbox(value)["xmin"]) > -180 & max(sf::st_bbox(value)[["xmax"]] < 180 & min(sf::st_bbox(value)[["ymin"]]) > -90 & max(sf::st_bbox(value)[["ymax"]])))
  {
    print(glue::glue("assuming WGS84 coordinates for {name}"))
    value <- sf::st_set_crs(value, "epsg:4326")
  }
  else if (is.na(sf::st_crs(value)))
  {
    stop(glue::glue("Value error: Please provide crs for {name}"))
  }
  else
  {
    value <- sf::st_transform(value, 4326)
  }
  if ("id" %in% colnames(as.data.frame(value)))
    value[["id"]] <- NULL
  if (!is.null(value[["userId"]]))
    value[["userId"]] <- NULL
  if (!is.null(value[["attribution"]]))
    value[["atribution"]] <- NULL
  if (!is.null(value[["radius"]]))
    value[["radius"]] <- NULL
  if (!is.null(value[["color"]]))
    value[["color"]] <- NULL

  return(value)

}

validArray <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  if (!is.matrix(value) || !is.array(value))
    stop(glue::glue("Value error: {name} must be of type array or matrix"))
  return(value)
}

validBool <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  if (!is.logical(value))
    stop(glue::glue("ValueError: {name} must be of type boolean"))

  value <- as.logical(value)
  return(value)
}

validObject <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

    value <- tryCatch(
    {
      value <- jsonlite::serializeJSON(value)
      value <- jsonlite::unserializeJSON(value)
    },
    error=function(cond)
    {
      stop(glue::glue("{name} must be json serializable"))
    },
    warning=function(cond)
    {
      stop(glue::glue("{name} must be json serializable"))
    },
    finally=
    {

    }
  )
  return(value)
}

validInt <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)
  if (!is.numeric(value))
    stop(glue::glue("Value error: {name} must be of type int"))
  if (value != as.integer(value))
    stop(glue::glue("Value error: {name} must be of type int"))

  value <- as.integer(value)
}

validFloat <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  if (!is.numeric(value) & length(value) <= 0)
    stop(glue::glue("Value error: {name} must be of type double"))

  value <- as.double(value)
  return(value)
}

validBounds <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  keys = c("xMin", "xMax", "yMin", "yMax")
  if (!isNamedList(value))
    stop(glue::glue("Value error: {name} must be a named list with keys {keys} whose values must be of type float"))

  for (key in keys)
  {
    if (!key %in% names(value) || !is.numeric(value[[key]]))
      stop(glue::glue("Value error: {name} must be a named list with keys {keys} whose values must be of type float"))
    value[[key]] = as.double(value[[key]])
  }
  return(value)
}

validStringArray <- function(name, value, required)
{
  if (required == FALSE & is.null(value))
    return(NULL)

  value <- tryCatch(
    {
      value <- list(value)
    },
    error=function(cond)
    {
      stop(glue::glue("Value error: {name} must be an iterable"))
    })


  if (any(!sapply(value, is.character)) == FALSE)
    stop(glue::glue("ValueError: {name} must be an iterable of strings"))
  return(value)
}

validDate <- function(name, value, required)
{
  if (required == FALSE & is.null(value))
    return(NULL)

  value <- tryCatch(
    {
      value <- strftime(value, "%Y-%m-%dT%H:%M:%S")
    },
    error=function(cond)
    {
      stop(glue::glue("Value error: {name} must be a valid date"))
    })

  return(value)
}
