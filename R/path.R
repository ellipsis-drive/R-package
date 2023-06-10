#' Search for a path to a folder
#'
#' @param pathTypes a list with strings used to indicate the file types included in the search
#' @param root a list of strings indicating the rootname of the folder you would like to search
#' @param text a string indicating ...
#' @param active a boolean indicating ...
#' @param userId a uuid referencing the ...
#' @param ....
#' @return A name value list containing information about the folder
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
    return(httr::content(apiManager_get("/path", body, token)))
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
  r <- httr::content(apiManager_get(glue::glue("/path/{pathId}"), NULL, token))
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

#' Update the metadata of a path
#' @param pathId Mandatory (uuid)
#' @param token Mandatory (string)
#' @param attribution Optional (string)
#' @param description Optional (string)
#' @param properties Optional (object)
#' @return content of the http patch request
#' @roxygen_header1
path.editMetaData <- function(pathId, token, description = NULL, attribution = NULL, licenseString = NULL, properties = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, TRUE)
  attribution <- validString("attribution", attribution, FALSE)
  description <- validString("description", description, FALSE)
  properties <- validObject("properties", properties, FALSE)
  licenseString <- validString("licenseString", licenseString, FALSE)
  return(httr::content(apiManager_patch(glue::glue("/path/{pathid}/metadata"), list(
    "description" = description,
    "attribution" = attribution,
    "properties" = properties,
    "license" = licenseString
  ), token)))
}

#' Rename a path
#' @param pathId Mandatory (uuid)
#' @param token Optional (string)
#' @param name Optional (string)
#' @return Content of the http put request
#' @roxygen_header1
path.rename <- function(pathId, name, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  name <- validString("name", name, FALSE)

  return(httr::content(apiManager_put(glue::glue("/path/{pathId}/name"), list(
    "name" = name
  ), token)))
}

#' @export
#' Moves paths to a different folder
#' @param pathIds Mandatory (array of uuids)
#' @param token Optional (string)
#' @param parentId Optional (uuid)
#' @roxygen_header1
path.move <- function(pathIds, parentId, token)
{
  token <- validString("token", token, FALSE)
  pathIds <- validUuidArray("pathIds", pathIds, TRUE)
  parentId <- validUuid("parentId", parentId, FALSE)

  return(httr::content(apiManager_put("/path/parentId", list(
    "pathIds" = pathIds,
    "parentId" = parentId
  ), token)))
}

#' @export
#' Place path in trash
#' @param pathId Mandatory (uuid)
#' @param token Optional (string)
#' @roxygen_header1
path.trash <- function(pathId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)

  return (apiManager_put(glue::glue("/path/{pathId}/trashed"), list(
    "trashed" = TRUE
  ), token))
}

#' @export
#' Recover a path
#' @param pathId Mandatory (uuid)
#' @param token Optional (string)
#' @roxygen_header1
path.recover <- function(pathId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)

  return (apiManager_put(glue::glue("/path/{pathId}/trashed"), list(
    "trashed" = FALSE
  ), token))
}

#' @export
#' Delete a path
#' @param pathId Mandatory (uuid)
#' @param token Optional (string)
#' @param recursive Optional (logical)
#' @roxygen_header1
path.delete <- function(pathId, token, recursive = FALSE)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  recursive <- validBool("recursive", recursive, TRUE)

  if (recursive)
  {
    info <- path.get(pathId, token)
    if (info[["type"]] == folder)
    {
      folders <- path.folder.listFolder(pathId = pathId, includeTrashed=True, pathTypes=list("folder"), token=token)[['result']]
      for (f in folders)
      {
        path.delete(f[["id"]], token, TRUE)
      }
      maps <- path.folder.listFolder(pathId=pathId, pathTypes=list('raster','vector','file'), includeTrashed=True, token=token)[['result']]
      for (m in maps)
      {
        path.delete(m[["id"]], token, TRUE)
      }
    }
    apiManager_delete(glue::glue("/path/{pathId}"), NULL, token)
  }
  else
    apiManager_delete(glue::glue("/path/{pathId}"), NULL, token)
}

#' @export
#' Update the public access of a path
#' @param pathId Mandatory (uuid)
#' @param token Mandatory (string)
#' @param access Optional (named list)
#' @param hidden Optional (logical)
#' @roxygen_header1
path.editPublicAccess <- function(pathId, token, accessLevel = NULL, hidden = NULL, processingUnits = NULL, geoFence = NULL)
{
  pathId <- validUuid('pathId', pathId, TRUE)
  token <- validString('token', token, FALSE)
  geoFence <- validObject('geoFence', geoFence, FALSE)
  accessLevel <- validInt('accessLevel', accessLevel, FALSE)
  processingUnits <- validInt('processingUnits', processingUnits, FALSE)
  hidden <- validBool('hidden', hidden, FALSE)
  body = list('accessLevel' = accessLevel, 'processingUnits' = processingUnits, 'geoFence' = geoFence, 'hidden'= hidden)

  return(apiManager_patch(glue::glue("/path/{pathId}/publicAccess"), body, token))
}

#' @export
#' Add path to favorites
#' @param pathId Mandatory (uuid)
#' @param token Mandatory (string)
#' @roxygen_header1
path.favorite <- function(pathId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)

  body = list("favorite" = TRUE)

  return(apiManager_put(glue::glue("/path/{pathId}/favorite"), body, token))
}

#' @export
#' Removes path from favorites
#' @param pathId Mandatory (uuid)
#' @param token Mandatory (string)
#' @roxygen_header1
path.unfavorite <- function(pathId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)

  body = list("favorite" = FALSE)

  return(apiManager_put(glue::glue("/path/{pathId}/favorite"), body, token))
}

path.convertPath <- function(path)
{
  if (path[["type"]] == "raster")
  {
    temp <- path[["raster"]][["timestamps"]]
    path[["raster"]][["timestamps"]] = list()
    for (x in temp)
    {
      timestamp <- temp
      timestamp[[date]] = list("from" = stringToDate(temp[["date"]][["from"]]), "to" = stringToDate(temp[["date"]][["to"]]))
      path[["raster"]][["timestamps"]] <- append(path[["raster"]][["timestamps"]], timestamp)
    }
  }
  if (path[["type"]] == "vector")
  {
    temp <- path[["vector"]][["timestamps"]]
    path[["vector"]][["timestamps"]] = list()
    for (x in temp)
    {
      timestamp <- temp
      timestamp[[date]] = list("from" = stringToDate(temp[["date"]][["from"]]), "to" = stringToDate(temp[["date"]][["to"]]))
      path[["vector"]][["timestamps"]] <- append(path[["vector"]][["timestamps"]], timestamp)
    }
  }
  return(path)
}
