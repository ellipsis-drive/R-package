#' Adds a vector
#' @param name Mandatory (string) name for the folder
#' @param token Mandatory (string) your token
#' @param parentId Optional (uuid) id of folder to place new folder in
#' @param publicAccess Optional (named list) list describing the public access of the folder
#' @param metadata Optional (named list) list describing metadata of the folder
#' @return
#' @export
path.vector.add <- function(name, token, parentId = NULL, publicAccess = NULL, metadata = NULL)
{
  name <- validString("name", name, TRUE)
  token <- validString("token", token, TRUE)
  parentId <- validUuid("parentId", parentId, FALSE)
  metadata <- validObject("metadata", metadata, FALSE)
  publicAccess <- validObject("publicAccess", publicAccess, FALSE)

  body <- list("name" = name, "parentId" = parentId, "publicAccess" = publicAccess, "metadata" = metadata)

  r <- apiManager_post(glue::glue("/path/vector"), body, token)
  return(httr::content(r))
}

#' Adds indices to a vector so that you can filter on properties
#' @param pathId Mandatory (uuid) id of the vector to edit
#' @param propertyFilter Mandatory (list) a list of named lists describing the filter
#' @param token Mandatory (string) your token
#' @return
#' @export
path.vector.editFilter <- function(pathId, propertyFilter, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, TRUE)
  propertyFilter <- validObject("propertyFilter", propertyFilter, TRUE)

  body = list("filter" = propertyFilter)

  r <- apiManager_post(glue::glue("/path/{pathId}/vector/filter"), body, token)
  return(httr::content(r))
}
