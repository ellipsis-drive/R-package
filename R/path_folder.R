#' @export
#' List the contents of a folder
#' @param pathId Mandatory (uuid) id of the folder
#' @param pathTypes Optional (list) list containing 'file', 'folder', 'raster', or 'vector'
#' @param pageStart Optional (uuid) from where to start the listing
#' @param listAll Optional (logical) default TRUE, whether to get all results or only the first page
#' @param token Optional (string)
#' @roxygen_header1
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
#' Adds a folder
#' @param name Mandatory (string) name for the folder
#' @param token Mandatory (string)
#' @param parentId Optional (uuid) id of the folder to place the new folder in
#' @param publicAccess (named list) named list describing the public access of the folder
#' @param metadata (named list) named list describing the metadata of the folder
#' @roxygen_header1
path.folder.add <- function(name, token, parentId = NULL, publicAccess = NULL, metadata = NULL)
{
  name <- validString("name", name, TRUE)
  token <- validString("token", token, TRUE)
  parentId <- validUuid("parentId", parentId, FALSE)
  metadata <- validObject("metadata", metadata, FALSE)
  publicAccess <- validObject("publicAccess", publicAccess, FALSE)

  body = list("name" = name, "parentId" = parentId, "publicAccess" = publicAccess, "metadata" = metadata)

  return(httr::content(apiManager_post("/path/folder", body, token)))
}
