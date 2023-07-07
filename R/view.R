#' @export
view.listViews <- function(token)
{
  token <- validString("token", token, TRUE)

  r <- apiManager_get("/view", list(), token)
  return(httr::content(r))
}

#' @export
view.get <- function(viewId, token = NULL)
{
  viewId <- validUuid("viewId", viewId, TRUE)
  token <- validString("token", token, FALSE)

  r <- apiManager_get(glue::glue("/view/{viewId}"), list(), token)
  return(httr::content(r))
}

#' @export
view.add <- function(pathId, layers, name = NULL, persistent = FALSE, dems = list(), token = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  layers <- validObject("layers", layers, TRUE)
  token <- validString("token", token, FALSE)
  name <- validString("name", name, FALSE)
  persistent <- validBool("persistent", persistent, TRUE)
  dems <- validObject("dems", dems, TRUE)

  r <- apiManager_post("/view", list("name" = name, "layers" = layers, "dems" = dems, "pathId"= pathId, "persistent" = persistent), token)
  return(httr::content(r))
}

#' @export
view.edit <- function(viewId, token, layers = NULL, name = NULL, dems = NULL)
{
  viewId <- validUuid("viewId", viewId, TRUE)
  token <- validString("token", token, FALSE)
  layers <- validObject("layers", layers, FALSE)
  dems <- validObject("dems", dems, FALSE)
  name <- validString("name", name, FALSE)

  r <- apiManager_patch(glue::glue("/view/{viewId}"), list("layers" = layers, "dems" = dems, "name" = name), token)
  return(httr::content(r))
}

#' @export
view.delete <- function(viewId, token)
{
  viewId <- validUuid("viewId", viewId, TRUE)
  token <- validString("token", token, FALSE)

  r <- apiManager_delete(glue::glue("/view/{viewId}"), list(), token)
  return(httr::content(r))
}
