#' @export
path.search <- function(pathTypes = c("raster", "vector", "file", "folder"), root = NULL, text = NULL, active = NULL, userId = NULL, pageStart = NULL, hashtag = NULL, extent = NULL, resolution = NULL, date = NULL, listAll = FALSE, token = NULL)
{
  token <- validString("token", token, FALSE)
  pathTypes <- validStringArray("pathTypes", pathTypes, TRUE)
  root <- validStringArray("root", root, FALSE)
  text <- validString("text", text, FALSE)
  userId <- validUuid("userId", userId, FALSE)
  pageStart <- validUuid("pageStart", pageStart, FALSE)
  hashtag <- validString("hashtag", hashtag, FALSE)


  listAll <- validBool("listAll", listAll, TRUE)

  active <- validBool("active", active, FALSE)

  extent <- validBounds("extent", extent, FALSE)

  date <- validDateRange("date", date, FALSE)


  resolution <- validResolution("resolution", resolution, FALSE)

  body = list(
    "type" = pathTypes,
    "root" = root,
    "text" = text,
    "userId" = userId,
    "active" = active,
    "pagestart" = pageStart,
    "hashtag" = hashtag,
    "extent" = extent,
    "resolution" = resolution,
    "date" = date
  )

  f <- function(body)
  {
    return(apiManager_get("/path", body, token))
  }

  r <- recurse(f, body, listAll)
  return(r)
}

#' Get information about folder
#'
#' This function retrieves information about a folder. The user provides the folderId
#' referencing the folder itself.
#'
#' @param pathID a Uuid referencing a folder or the path to that folder
#' @param token a string used to authenticate the user
#' @return A name value list containing information about the folder
#' @export
path.get <- function(pathId, token = NULL)
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
