#' Add a raster
#' @param name Mandatory (string) name for the raster
#' @param token Mandatory (string)
#' @param parentId Optional (uuid) id of the folder to place the raster in
#' @param publicAccess Optional (named list) named list describing the raster's public access
#' @param metadata Optional (named list) named list describing the raster's metadata
#' @return ...
#' @export
path.raster.add <- function(name, token, parentId = NULL, publicAccess = NULL, metadata = NULL)
{
  name <- validString("name", name, TRUE)
  token <- validString("token", token, TRUE)
  parentId <- validUuid("parentId", parentId, FALSE)
  metadata <- validObject("metadata", metadata, FALSE)
  publicAccess <- validObject("publicAccess", publicAccess, FALSE)

  body <- list("name" = name, "parentId" = parentId, "publicAccess" = publicAccess, "metadata" = metadata)

  return(httr::content(apiManager_post("/path/raster", body, token)))
}

#' Change the name of a band
#' @param pathId Mandatory (uuid)
#' @param token Mandatory (string)
#' @param bandNumber Mandatory (integer)
#' @param name Mandatory (string)
#' @return ...
#' @export
path.raster.editBand <- function(pathId, bandNumber, name, token)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  bandNumber <- validInt("bandNumber", bandNumber, TRUE)
  name <- validString("name", name, TRUE)

  body = list("name" = name)
  r <- apiManager_put(glue::glue("/path/{pathId}/raster/band/{bandNumber}/name"), body, token)
  r <- httr::content(r)
  return(r)
}

#' Edit raster map attributes
#' @param pathId Mandatory (uuid)
#' @param token Mandatory (string)
#' @param interpolation Optional (string)
#' @param includesTransparent Optional (logical) default FALSE
#' @return ...
#' @export
path.raster.edit <- function(pathId, token, interpolation = NULL, includesTransparent = FALSE)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  interpolation <- validString("interpolation", interpolation, FALSE)
  includesTransparent <- validBool("includesTransparent", includesTransparent, FALSE)

  body = list("interpolation" = interpolation, "includesTransparent" = includesTransparent)
  return(httr::content(apiManager_patch(glue::glue("/path/{pathId}/raster"), body, token)))
}
