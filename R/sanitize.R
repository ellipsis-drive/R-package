#library(glue)
#source("R/util/root.R")
validString <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  if (!isSingleString(value))
    stop("Value error: {name} must be of type string")
  return(value)
}

validInt <- function(name, value, required)
{
  if (!required & is.null(value))
    return(NULL)

  if (!is.int(value))
    stop("Value error: {name} must be of type int")
}
