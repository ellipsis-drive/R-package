#' Add style to a vector
#' @param pathId Mandatory (uuid)
#' @param name Mandatory (string)
#' @param method Mandatory (string)
#' @param parameter Mandatory (object) see https://docs.ellipsis-drive.com/developers/api-v3/path-vector/styles/add-style on how to format the parameters
#' @param token Mandatory (string)
#' @param default Optional (logical) default TRUE
#' @return ...
#' @export
path.vector.style.add <- function(pathId, name, method, parameters, token, default = TRUE)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  method <- validString("method", method, TRUE)
  parameters <- validObject("parameters", parameters, TRUE)
  token <- validString("token", token, TRUE)
  default <- validBool("default", default, TRUE)
  # Notify Daniel missing in python below
  name <- validString("name", name, TRUE)

  body <- list("name" = name, "default" = default, "method" = method, "parameters" = parameters)

  r <- apiManager_post(glue::glue("/path/{pathId}/vector/style"), body, token)
  return(httr::content(r))
}

#' Edit the style of a vector
#' @param pathId Mandatory (uuid)
#' @param styleId Mandatory (uuid)
#' @param token Mandatory (string)
#' @param method Optional (string)
#' @param parameters Optional (object)
#' @param default Optional (logical) default TRUE
#' @return ...
#' @export
path.vector.style.edit <- function(pathId, styleId, token, name = NULL, method = NULL, parameters = NULL, default = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  styleId <- validUuid("styleId", styleId, TRUE)
  method <- validString("method", method, FALSE)
  parameters <- validObject("parameters", parameters, FALSE)
  token <- validString("token", token, TRUE)
  default <- validBool("default", default, FALSE)
  name <- validString("name", name, FALSE)

  body <- list("name" = name, "default" = default, "method" = method, "parameters" = parameters)

  r <- apiManager_patch(glue::glue("/path/{pathId}/vector/style/{styleId}"), body, token)
  return(httr::content(r))
}

#' Delete the style for a given vector
#' @param pathId Mandatory (uuid)
#' @param styleId Mandatory (uuid)
#' @param token Mandatory (string)
#' @return ...
#' @export
path.vector.style.delete <- function(pathId, styleId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  styleId <- validUuid("styleId", styleId, TRUE)
  token <- validString("token", token, TRUE)

  r <- apiManager_delete(glue::glue("/path/{pathId}/vector/style/{styleId}"), NULL, token)
  return(httr::content(r))
}
