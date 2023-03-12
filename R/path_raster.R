#' @export
path.raster.add <- function(name, token, parentId = NULL, publicAccess = NULL, metadata = NULL)
{
  name <- validString("name", name, TRUE)
  token <- validString("token", token, TRUE)
  parentId <- validUuid("parentId", parentId, FALSE)
  metadata <- validObject("metadata", metadata, FALSE)
  publicAccess <- validObject("publicAccess", publicAccess, FALSE)

  body <- list("name" = name, "parentId" = parentId, "publicAccess" = publicAccess, "metadata" = metadata)

  return(apiManager_post("/path/raster", body, token))
}

#' @export
path.raster.edit <- function(pathId, token, interpolation = NULL, includesTransparent = NULL)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  interpolation <- validString("interpolation", interpolation, FALSE)
  includesTransparent <- validBool("includesTransparent", includesTransparent, FALSE)

  body = list("interpolation" = interpolation, "includesTransparent" = includesTransparent)
  return(apiManager_patch(glue::glue("/path/{pathId}/raster"), body, token))
}
