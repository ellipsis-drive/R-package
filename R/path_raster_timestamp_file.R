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

#' @export
path.raster.timestamp.file.get <- function(pathId, timestampId, token, pageStart = NULL, listAll = TRUE)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  pageStart <- validUuid("pageStart", pageStart, FALSE)

  f <- function(body)
  {
    r <- apiManager_get(glue::glue("/path/{pathId}/raster/timestamp/{timestampId}/file"), NULL, token)
    for (i in seq(lengths(r[["result"]])))
    {
      if (!is.null(r[["result"]][[i]][["info"]] & !is.null(r[["result"]][[i]][["info"]][["bounds"]])))
          r[["result"]][[i]][["info"]][["bounds"]] <- st_combine(st_sf(id = 0, properties = list(), geometry = r[["result"]][[i]][["info"]][["bounds"]]))
    }
    return(r)
  }

  r <- recurse(f, list("pageStart" = pageStart), listAll)
  return(r)
}
