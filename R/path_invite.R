#' @export
path.invite.send <- function(pathId, access, token, userId = NULL, email = NULL, sendMail = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, TRUE)
  userId <- validUuid("userId", userId, FALSE)
  email <- validString("email", email, FALSE)
  access <- validObject("access", access, TRUE)
  sendMail <- validBool("sendMail", sendMail, FALSE)

  return(apiManager_post(glue::glue("/path/{pathId}/invite"), list(
    "userId" = userId,
    "email" = email,
    "access" = access,
    "sendMail" = sendMail
  ), token))
}

#' @export
path.invite.revoke <- function(pathId, inviteId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  inviteId <- validUuid("inviteId", inviteId, TRUE)
  return(apiManager_delete(glue::glue("/path/{pathId}/invite/{inviteId}"), NULL, token))
}

#' @export
path.invite.accept <- function(pathId, inviteId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  inviteId <- validUuid("inviteId", inviteId, TRUE)

  return(apiManager_post(glue::glue("/path/{pathId}/invite/{inviteId}/accept"), list(
    "accept" = TRUE
  ), token))
}

#' @export
path.invite.decline <- function(pathId, inviteId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  inviteId <- validUuid("inviteId", inviteId, TRUE)

  return(apiManager_post(glue::glue("/path/{pathId}/invite/{inviteId}/accept"), list(
    "accept" = FALSE
  ), token))
}

#' @export
path.invite.getYourInvites <- function(token)
{
  token <- validString("token", token, TRUE)
  return(apiManager_get("/path/invite", NULL, token))
}

#' @export
path.invite.getPathInvites <- function(pathId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  return(apiManager_get(glue::glue("/path/{pathId}/invite"), NULL, token))
}
