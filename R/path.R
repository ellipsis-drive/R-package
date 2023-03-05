get <- function(pathId, token = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  r <- apiManager_get(glue::glue("/path/{pathId}"), NULL, token)
  r <- convertPath(r)

  return(r)
}

convertPath <- function(path)
{
  if (path[["type"]] == "raster")
  {
    # Python: [ {**x, 'date' : {'from':stringToDate(x['date']['from']), 'to' : stringToDate(x['date']['to']) }} for x in path['raster']['timestamps'] ]
    dateDict <- list()
    for (x in names(path[["raster"]][["timestamps"]]))
      dateDict[["date"]] = list("from" = stringToDate(x[["date"]][["from"]]), "to" = stringToDate(x[["date"]][["to"]]))
    path[["raster"]][["timestamps"]] <- append(path[["raster"]][["timestamps"]], dateDict)
  }
  if (path[["type"]] == "vector")
  {
    dateDict <- list()
    for (x in names(path[["vector"]][["timestamps"]]))
      dateDict[["date"]] = list("from" = stringToDate(x[["date"]][["from"]]), "to" = stringToDate(x[["date"]][["to"]]))
    path[["vector"]][["timestamps"]] <- append(path[["vector"]][["timestamps"]], dateDict)
  }

  return(path)
}
