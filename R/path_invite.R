#' Invites someone to a path. They will need to accept before they are added
#' @param pathId Mandatory (uuid) the id of the block or folder to share
#' @param access Mandatory (object)  should be an object with properties canShare, geoFence, monthlyFee, accessLevel and processingUnits defining the access given to the user. geoFence should be an object with tiles and maxZoom. Tiles should be an array of objects with tileX, tileY and zoom.
#' @param token Mandatory (string)
#' @param userId Semi-Mandatory, at least one required (uuid) user id to send the invite to
#' @param email Semi-Mandatory at least one required (string) email of the user to send the invite to
#' @param sendMail Optional (logical) default FALSE
#' @return
#' @export
path.invite.send <- function(pathId, access, token, userId = NULL, email = NULL, sendMail = FALSE)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, TRUE)
  userId <- validUuid("userId", userId, FALSE)
  email <- validString("email", email, FALSE)
  access <- validObject("access", access, TRUE)
  sendMail <- validBool("sendMail", sendMail, FALSE)

  return(httr::content(apiManager_post(glue::glue("/path/{pathId}/invite"), list(
    "userId" = userId,
    "email" = email,
    "access" = access,
    "sendMail" = sendMail
  ), token)))
}

#' Revoke a sent invitation
#' @param pathId Mandatory (uuid) the id of the map or folder
#' @param inviteId Mandatory (uuid) the id of the invite you wish to revoke
#' @param token Mandatory (string)
#' @return
#' @export
path.invite.revoke <- function(pathId, inviteId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  inviteId <- validUuid("inviteId", inviteId, TRUE)
  return(httr::content(apiManager_delete(glue::glue("/path/{pathId}/invite/{inviteId}"), NULL, token)))
}

#' Accept an invite to a map or a folder
#' @param pathId Mandatory (uuid) the id of the map or folder
#' @param inviteId Mandatory (uuid) the id of the invite you wish to accept
#' @param token Mandatory (string)
#' @return
#' @export
path.invite.accept <- function(pathId, inviteId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  inviteId <- validUuid("inviteId", inviteId, TRUE)

  return(httr::content(apiManager_post(glue::glue("/path/{pathId}/invite/{inviteId}/accept"), list(
    "accept" = TRUE
  ), token)))
}

#' Decline an invite to a map or a folder
#' @param pathId Mandatory (uuid) the id of the map or folder
#' @param inviteId Mandatory (uuid) the id of the invite you wish to accept
#' @param token Mandatory (string)
#' @return
#' @export
path.invite.decline <- function(pathId, inviteId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  inviteId <- validUuid("inviteId", inviteId, TRUE)

  return(httr::content(apiManager_post(glue::glue("/path/{pathId}/invite/{inviteId}/accept"), list(
    "accept" = FALSE
  ), token)))
}

#' Retrieve pending invites
#' @param token Mandatory (string)
#' @return
#' @export
path.invite.getYourInvites <- function(token)
{
  token <- validString("token", token, TRUE)
  return(httr::content(apiManager_get("/path/invite", NULL, token)))
}

#' Retrieve pending invites to a specific map
#' @param pathId Mandatory (uuid) the id of the block or folder
#' @param token Mandatory (string)
#' @return
#' @export
path.invite.getPathInvites <- function(pathId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  return(httr::content(apiManager_get(glue::glue("/path/{pathId}/invite"), NULL, token)))
}
