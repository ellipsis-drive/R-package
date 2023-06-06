#' @export
path.vector.timestamp.add <- function(pathId, token, properties = NULL, description = NULL, date = list("from" = Sys.time(), "to" = Sys.time()))
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, TRUE)
  properties <- validObject("properties", properties, FALSE)
  description <- validString("description", description, FALSE)
  date <- validDateRange("date", date, TRUE)

  body <- list("properties" = properties, "date" = date, "description" = description)
  r <- httr::content(apiManager_post(glue::glue("/path/{pathId}/vector/timestamp"), body, token))
  return(r)
}

#' @export
path.vector.timestamp.edit <- function(pathId, timestampId, token, description = NULL, date = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, TRUE)
  properties <- validObject("properties", properties, FALSE)
  description <- validString("description", description, FALSE)
  date <- validDateRange("date", date, TRUE)

  body <- list("date" = date, "description" = description)
  r <- httr::content(apiManager_patch(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}"), body, token))
  return(r)
}

#' @export
path.vector.timestamp.trash <- function(pathId, timestampId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, TRUE)
  body <- list("trashed"=TRUE)
  r <- httr::content(apiManager_put(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/trashed"), body, token))
}

#' @export
path.vector.timestamp.recover <- function(pathId, timestampId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, TRUE)
  body <- list("trashed"=FALSE)
  r <- httr::content(apiManager_put(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/trashed"), body, token))
}

#' @export
path.vector.timestamp.delete <- function(pathId, timestampId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, TRUE)
  r <- apiManager_delete(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}"), NULL, token)
  return(httr::content(r))
}

#' @export
path.vector.timestamp.activate <- function(pathId, timestampId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, TRUE)
  r <- apiManager_post(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/activate"), NULL, token)
  return(httr::content(r))
}

#' @export
path.vector.timestamp.deactivate <- function(pathId, timestampId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, TRUE)
  r <- apiManager_post(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/deactivate"), NULL, token)
  return(httr::content(r))
}

#' @export
path.vector.timestamp.getBounds(pathId, timestampId, token = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, FALSE)

  r <- httr::content(apiManager_get(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/bounds"), NULL, token))

  r <- list("id" = 0, "properties" = list(), "geometry" = r)
  return(r)
}
