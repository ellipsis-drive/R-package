#' @export
#' Downloads a file
#' @param pathId Mandatory (uuid) id of the folder
#' @param filePath Mandatory (string) location to download file to
#' @param token Optional (string)
#' @roxygen_header1
path.file.download <- function(pathId, filePath, token = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  filePath <- validString("filePath", filePath, TRUE)

  apiManager_download(uPaste(uPaste("/path/", pathId), "/file/data"), filePath, token)
}

#' @export
#' Adds a file
#' @param filePath Mandatory (string) path to the file to upload
#' @param token Mandatory (string)
#' @param parentId Optional (uuid) id of folder to place file in
#' @param publicAccess Optional (named list) named list desctibing public access of the file
#' @param metadata Optional (named list) named list describing metadata of the file
#' @roxygen_header1
path.file.add <- function(filePath, token, parentId = NULL, publicAccess = NULL, metadata = NULL)
{
  token <- validString("token", token, TRUE)
  filePath <- validString("filePath", filePath, TRUE)
  parentId <- validUuid("parentId", parentId, FALSE)
  publicAccess <- validObject("publicAccess", publicAccess, FALSE)
  metadata <- validObject("metadata", metadata, FALSE)

  fileName <- basename(filePath)

  body = list("name" = fileName, "publicAccess" = publicAccess, "metadata" = metadata, "parentId" = parentId)
  r <- apiManager_upload("/path/file", filePath = filePath, body = body, token = token, key = "data")
  return(r)
}

