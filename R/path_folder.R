#' @export
path.folder.listFolder <- function(pathId, pathTypes = NULL, pageStart = NULL, listAll = TRUE, includeTrashed = FALSE, token = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  pageStart <- validUuid("pageStart", pageStart, FALSE)
  listAll <- validBool("listAll", listAll, FALSE)
  includeTrashed <- validBool("includeTrashed", includeTrashed, TRUE)
  pathTypes <- validObject("pathTypes", pathTypes, FALSE)

  if (is.null(pathTypes))
    pathTypes = list("folder", "raster", "vector", "file")

  body = list("pageStart" = pageStart, "type" = pathTypes, "includeTrashed" = includeTrashed)

  f <- function(body)
  {
    return(httr::content(apiManager_get(glue::glue("/path/{pathId}/list"), body, token)))
  }

  r <- recurse(f, body, listAll)

  return(r)
}

#' @export
path.folder.add <- function(name, token, parentId = NULL, publicAccess = NULL, metadata = NULL)
{
  name <- validString("name", name, TRUE)
  token <- validString("token", token, TRUE)
  parentId <- validUuid("parentId", parentId, FALSE)
  metadata <- validObject("metadata", metadata, FALSE)
  publicAccess <- validBool("publicAccess", publicAccess, FALSE)

  body = list("name" = name, "parentId" = parentId, "publicAccess" = publicAccess, "metadata" = metadata)

  return(httr::content(apiManager_post("/path/folder", body, token)))
}
