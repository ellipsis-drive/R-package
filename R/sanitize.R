#library(glue)
#source("R/util/root.R")
validUuid <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  value <- tryCatch(
    {
      uuid::uuid(value)
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
