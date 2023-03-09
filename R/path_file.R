#' @export
path.file.download <- function(pathId, filePath, token = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  filePath <- validString("filePath", filePath, TRUE)

  apiManager_download(uPaste(uPaste("/path/", pathId), "/file/data"), filePath, token)
}
