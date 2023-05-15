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

  value <- tryCatch(
    {
      value <- list(value)
    },
    error=function(cond)
    {
      stop(glue::glue("Value error: {name} must be an iterable"))
    })

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

  if (!is(value, "sfg"))
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
  sh <- sf::st_sf(geometry = target_file)

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


  for (x in value)
  {
    if (!isSingleString(x))
      stop(glue::glue("Value error: {name} must be of type string"))
  }

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
