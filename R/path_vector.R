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

#' @export
path.vector.editFilter(pathId, propertyFilter, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, TRUE)
  propertyFilter <- validObject("propertyFilter", propertyFilter, TRUE)

  body = list("filter" = propertyFilter)

  r <- apiManager_post(glue::glue("/path/{pathId}/vector/filter"), body, token)
  return(httr::content(r))
}
