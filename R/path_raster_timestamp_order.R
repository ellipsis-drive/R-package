#' @export
path.raster.timestamp.order.get <- function(token)
{
  token <- validString("token", token, TRUE)
  r <- apiManager_get("/path/raster/timestamp/order", NULL, token)
  r <- httr::content(r)
  return(r)
}

#' @export
path.raster.timestamp.order.add <- function(pathId, timestampId, extent = NULL, epsg = 4326)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  extent <- validBounds("extent", extent, TRUE)
  epsg <- validInt("epsg", epsg, TRUE)

  body <- list("extent" = extent, "epsg" = epsg)
  r <- apiManager_post(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}/order"), body, token)
  r <- httr::content(r)
  return(r)

}

#' @export
path.raster.timestamp.order.add <- function(orderId, filePath, token)
{
  token <- validString("token", token, TRUE)
  orderId <- validUuid("orderId", orderId, TRUE)
  filePath <- validString("filePath", filePath, TRUE)

  if (substr(filePath, nchar(filePath)-3, nchar(filePath)) != ".tif")
    stop("valueError: filePath must end with .tif")

  apiManager_download(glue::glue("/path/raster/timestamp/order/{orderId}/data"), filePath, token)
}
