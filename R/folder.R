#' export
listFolder <- function(pathId, pathTypes = NULL, pageStart = NULL, listAll = TRUE, includeTrashed = FALSE, token = NULL)
{
  pathId <- validUuid('pathId', pathId, True)
  token <- validString('token', token, False)
  pageStart <- validUuid('pageStart', pageStart, False)
  listAll <- validBool('listAll', listAll, False)
  includeTrashed <- validBool('includeTrashed', includeTrashed, True)
  pathTypes <- validObject('pathTypes', pathTypes, False)

  if (is.null(pathTypes))
    pathTypes <- list("folder", "raster", "vector", "file")

  body <- list("pageStart" = pageStart, "type" = pathTypes, "includeTrashed" = includeTrashed)

  f <- function(body)
  {
    return(apiManager_get(glue::glue("/path/{pathId}/list"), body, token))
  }

  r <- recurse(f, body, listAll)
  return(r)
}
