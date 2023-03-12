#' @export
path.member.get <- function(pathId, token = NULL, memberType = list("inherited", "direct"))
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  memberType <- validStringArray("memberType", memberType, FALSE)

  body = list(
    "type" = memberType
  )

  return(apiManager_get(glue::glue("/path/{pathId}/member"), body, token))
}

#' @export
path.member.delete <- function(pathId, userId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  userId <- validUuid("userId", userId, TRUE)
  token <- validString("token", token, TRUE)

  return(apiManager_delete(glue::glue("/path/{pathId}/member/{userId}"), NULL, token))
}

#' @export
path.member.edit <- function(pathId, userId, access, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, TRUE)
  userId <- validUuid("userId", userId, TRUE)
  access <- validObject("access", access, TRUE)

  return(apiManager_patch(glue::glue("/path/{pathId}/member/{userId}"), list(
    "access" = access
  ), token))
}
