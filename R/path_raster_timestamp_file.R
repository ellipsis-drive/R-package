#' Upload a raster file
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param filePath Mandatory (string)
#' @param token Mandatory (string)
#' @param noDataValue Optional (double)
#' @param epsg Optional (int)
#' @param fileFormat Optional (string)
#' @return ...
#' @export
path.raster.timestamp.file.add <- function(pathId, timestampId, filePath, token, fileFormat, epsg = NULL, noDataValue = NULL)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  filePath <- validString("filePath", filePath, TRUE)
  noDataValue <- validFloat("noDataValue", noDataValue, FALSE)
  epsg <- validInt("epsg", epsg, FALSE)
  fileFormat <- validString("fileFormat", fileFormat, TRUE)

  fileName <- basename(filePath)

  body <- list("name" = fileName, "epsg" = epsg, "noDataValue" = noDataValue, "format" = fileFormat)
  return(apiManager_upload(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}/file"), filePath, body, token))
}

#' Get all uploads for a given timestamp
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Mandatory (string)
#' @param pageStart Optional (uuid)
#' @param listAll Optional (logical) default TRUE
#' @return ...
#' @export
path.raster.timestamp.file.get <- function(pathId, timestampId, token, pageStart = NULL, listAll = TRUE)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  pageStart <- validUuid("pageStart", pageStart, FALSE)

  f <- function(body)
  {
    r <- httr::content(apiManager_get(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}/file"), NULL, token))
    for (i in seq(lengths(r[["result"]])))
    {
      if (!is.null(r[["result"]][[i]][["info"]] & !is.null(r[["result"]][[i]][["info"]][["bounds"]])))
          r[["result"]][[i]][["info"]][["bounds"]] <- sf::st_combine(sf::st_sf(id = 0, properties = list(), geometry = r[["result"]][[i]][["info"]][["bounds"]]))
    }
    return(r)
  }

  r <- recurse(f, list("pageStart" = pageStart), listAll)
  return(r)
}

#' Move an uploaded raster file to the trash
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Mandatory (string)
#' @param fileId Mandatory (uuid)
#' @return ...
#' @export
path.raster.timestamp.file.trash <- function(pathId, timestampId, fileId, token)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  fileId <- validUuid("fileId", fileId, TRUE)

  r <- apiManager_put(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}/file/{fileId}/trashed"), list("trashed" = TRUE), token)
  return(httr::content(r))
}

#' Recover an uploaded raster file from trash
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param fileId Mandatory (uuid)
#' @param token Mandatory (string)
#' @return ...
#' @export
path.raster.timestamp.file.recover <- function(pathId, timestampId, fileId, token)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  fileId <- validUuid("fileId", fileId, TRUE)

  r <- apiManager_put(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}/file/{fileId}/trashed"), list("trashed" = FALSE), token)
  return(httr::content(r))
}

#' Delete a given upload
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param fileId Mandatory (uuid)
#' @param token Mandatory (string)
#' @return ...
#' @export
path.raster.timestamp.file.delete <- function(pathId, timestampId, fileId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  fileId <- validUuid("fileId", fileId, TRUE)
  token <- validString("token", token, TRUE)

  r <- httr::content(apiManager_delete(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}/file/{fileId}"), NULL, token))
  return(r)
}

#' Donwload a previously uploaded raster file
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param fileId Mandatory (uuid)
#' @param token Mandatory (string)
#' @param filePath Mandatory (string)
#' @return ...
#' @export
path.raster.timestamp.file.download <- function(pathId, timestampId, fileId, filePath, token)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  fileId <- validUuid("fileId", fileId, TRUE)
  filePath <- validString("filePath", filePath, TRUE)

  if (substr(filePath, nchar(filePath)-3, nchar(filePath)) != ".tif")
    stop("valueError: filePath must end with .tif")

  apiManager_download(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}/file/{fileId}/data"), filePath, token)
}

