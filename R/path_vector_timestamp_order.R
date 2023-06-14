#' Retrieve information on all your vector downloads
#' @param token Mandatory (string)
#' @return ...
#' @export
path.vector.timestamp.order.get <- function(token)
{
  token <- validString("token", token, TRUE)
  r <- httr::content(apiManager_get("/path/vector/timestamp/order"), NULL, token)
  return(r)
}

#' Order a download
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Mandatory (string)
#' @param extent Optional (named list) with names xMin, xMax, yMin, and yMax
#' @param uploadId Optional (uuid)
#' @return the orderId (uuid)
#' @export
path.vector.timestamp.order.add <- function(pathId, timestampId, token, extent = NULL, uploadId = NULL)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  extent <- validBounds("extent", extent, FALSE)
  uploadId <- validUuid("uploadId", uploadId, FALSE)

  body = list("uploadId" = uploadId, "extent" = extent, "format" = "geojson")

  r <- httr::content(apiManager_post(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/order"), body, token))

  return(r)
}

#' Download the ordered file
#' @param orderId Mandatory (uuid)
#' @param filePath Mandatory (string) path to local file system where file will be saved
#' @param token Mandatory (string)
#' @return ...
#' @export
path.vector.timestamp.order.download <- function(orderId, filePath, token)
{
  token <- validString("token", token, TRUE)
  filePath <- validString("filePath", filePath, TRUE)
  orderId <- validUuid("pathId", orderId, TRUE)

  if (substr(filePath, nchar(filePath)-5, nchar(filePath)) != ".zip")
    stop("ValueError: filePath must end with .zip")

  apiManager_download(glue::glue("/path/vector/timestamp/order/{orderId}/data"), filePath, token)
}

