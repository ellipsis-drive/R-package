#' @export
path.vector.timestamp.file.add <- function(pathId, timestampId, filePath, token, fileFormat, epsg = NULL, dateColumns = NULL, datePatterns = NULL, method = "simplify", fastUpload = TRUE)
{
  token <- validString('token', token, TRUE)
  pathId <- validUuid("pathId", pathId, TURE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  filePath <- validString("filePath", filePath, TRUE)
  epsg <- validInt("epsg", epsg, FALSE)
  method <- validString("method", method, TRUE)
  fileFormat <- validString("fileFormat", fileFormat, TRUE)
  dateColumns <- validStringArray("dateColumns", dateColumns, FALSE)
  datePatterns <- validStringArray("datePatterns", datePatterns, FALSE)
  fastUpload <- validBool("fastUpload", fastUpload, TRUE)

  if (fastUpload)
    fastUpload <- "true"
  else
    fastUpload <- "false"

  seperator <- "/"
  fileName <- tail(strsplit(filePath, seperator)[[1]], 1)

  body <- list("name" = fileName, "epsg" = epsg, "format" = fileFormat, "dateColumns" = dateColumns, "datePatterns" = datePatterns, "fastUpload" = fastUpload)
  r <- apiManager_upload(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/file"), filePath, body, token)
  return(httr::content(r))
}

#' @export
path.vector.timestamp.file.get <- function(pathId, timestampId, token, pageStart = NULL, listAll = TRUE)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  pageStart <- validUuid("pageStart", pageStart, FALSE)
  listAll <- validBool("listAll", listAll, TRUE)

  body <- list("pageStart" = pageStart)
  f <- function(body)
  {
    r <- apiManager_get(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}"), body, token)

    for (i in seq(length(r[["result"]])))
    {
      if (!is.null(r[["result"]][[i]][["info"]]) & "bounds" %in% names(r[["result"]][[i]][["info"]]) & class(r[["result"]][[i]][["info"]][["bounds"]]) != "NULL")
      {
        coordinates <- unlist(r[["result"]][[i]][["info"]][["bounds"]][["coordinates"]][[1]], recursive = FALSE)
        matrix_coordinates <- matrix(unlist(coordinates), ncol = 2, byrow = TRUE)
        geometry <- sf::st_polygon(list(matrix_coordinates))
        sf_object <- sf::st_sf(id = 0, geometry = sf::st_sfc(geometry))
        r$result[[i]]$info$bounds <- sf_object
      }
    }
    return(r)
  }

  r <- recurse(f, list("pageStart" = pageStart), listAll)
  return(r)
}

#' @export
path.vector.timestamp.file.download <- function(pathId, timestampId, fileId, filePath, token)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  fileId <- validUuid("fileId", fileId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  filePath <- validString("filePath", filePath, TRUE)

  apiManager_download(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/file/{fileId}/data"), filePath, token)
}
