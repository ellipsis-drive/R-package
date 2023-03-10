#' @export
path.file.download <- function(pathId, filePath, token = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  filePath <- validString("filePath", filePath, TRUE)

  apiManager_download(uPaste(uPaste("/path/", pathId), "/file/data"), filePath, token)
}

#' @export
path.file.add <- function(filePath, token, parentId = NULL, publicAccess = NULL, metadata = NULL)
{
  token <- validString("token", token, TRUE)

  fileName <- basename(filePath)

  body = list("name" = fileName, "publicAccess" = publicAccess, "metadata" = metadata, "parentId" = parentId)
  r <- apiManager_upload("/path/file", filePath, body, token, key = "data")
  return(r)
}

