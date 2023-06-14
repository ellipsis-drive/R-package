#' Retrieves the users a path has been shared with
#' @param pathId Mandatory (uuid)
#' @param token Optional (string)
#' @param memberType Optional (array, vector, or list of strings) can contain "inherited" and "direct"
#' @return
#' @export
path.member.get <- function(pathId, token = NULL, memberType = list("inherited", "direct"))
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  memberType <- validStringArray("memberType", memberType, FALSE)

  body = list(
    "type" = memberType
  )

  return(httr::content(apiManager_get(glue::glue("/path/{pathId}/member"), body, token)))
}

#' Removes a member from a path
#' @param pathId Mandatory (uuid)
#' @param userId Mandatory (uuid)
#' @param token Mandatory (string)
#' @return
#' @export
path.member.delete <- function(pathId, userId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  userId <- validUuid("userId", userId, TRUE)
  token <- validString("token", token, TRUE)

  return(httr::content(apiManager_delete(glue::glue("/path/{pathId}/member/{userId}"), NULL, token)))
}

#' Edits permissions of a user on a given path
#' @param pathId Mandatory (uuid)
#' @param userId Mandatory (uuid)
#' @param access Mandatory (object) Object with optional properties accessLevel, processingUnits, canShare, geoFence, with the changes in access. geoFence should be an object with tiles and maxZoom. Tiles should be an array of objects with tileX, tileY and zoom.
#' @param token Mandatory (string)
#' @return
#' @export
path.member.edit <- function(pathId, userId, access, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, TRUE)
  userId <- validUuid("userId", userId, TRUE)
  access <- validObject("access", access, TRUE)

  return(httr::content(apiManager_patch(glue::glue("/path/{pathId}/member/{userId}"), list(
    "access" = access
  ), token)))
}
