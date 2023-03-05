#library(glue)
#source("R/util/root.R")
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
      value <- jsonDumps(value)
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

  if (!is.int(value))
    stop(glue::glue("Value error: {name} must be of type int"))
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
