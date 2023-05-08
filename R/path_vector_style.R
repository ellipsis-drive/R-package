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

#' @export
path.vector.style.delete <- function(pathId, styleId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  styleId <- validUuid("styleId", styleId, TRUE)
  token <- validString("token", token, TRUE)

  r <- apiManager_delete(glue::glue("/path/{pathId}/vector/style/{styleId}"), NULL, token)
  return(httr::content(r))
}
