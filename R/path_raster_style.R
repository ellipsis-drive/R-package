#' Adds a raster style
#' @param pathId Mandatory (uuid)
#' @param method Mandatory (string)
#' @param parameters Mandatory (object) See https://docs.ellipsis-drive.com/developers/api-v3/path-raster/styles/add-style for how to format the paramters
#' @param token Mandatory (string)
#' @param default Optional (logical) default TRUE
#' @return ...
#' @export
path.raster.style.add <- function(pathId, name, method, parameters, token, default = TRUE)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  method <- validString("method", method, TRUE)
  name <- validString("name", name, TRUE)
  default <- validBool("default", default, TRUE)
  parameters <- validObject("parameters", parameters, TRUE)

  body = list("name" = name, "method" = method, "parameters" = parameters, "default" = default)
  return(httr::content(apiManager_post(glue::glue("/path/{pathId}/raster/style"), body, token)))
}

#' Delete a raster style
#' @param pathId Mandatory (uuid)
#' @param styleId Mandatory (uuid)
#' @param token Mandatory (string)
#' @return ...
#' @export
path.raster.style.delete <- function(pathId, styleId, token)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  styleId <- validUuid("styleId", styleId, TRUE)

  return(httr::content(apiManager_delete(glue::glue("/path/{pathId}/raster/style/{styleId}"), NULL, token)))
}

#' Edit a raster style
#' @param pathId Mandatory (uuid)
#' @param styleId Mandatory (uuid)
#' @param token Mandatory (string)
#' @param method Optional (string)
#' @param parameters Optional (object)
#' @param default Optional (logical) default = TRUE
#' @return ...
#' @export
path.raster.style.edit <- function(pathId, styleId, method, parameters, default = TRUE)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  styleId <- validUuid("styleId", styleId, TRUE)
  method <- validString("method", method, TRUE)
  parameters <- validObject("parameters", parameters, TRUE)
  default <- validBool("default", default, FALSE)

  body = list("method" = method, "parameters" = parameters, "default" = default)
  return(httr::content(apiManager_patch(glue::glue("/path/{pathId}/raster/style/{styleId}"), body, token)))
}
