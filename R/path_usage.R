#' @export
#' Get users that used the given path
#' @param pathId Mandatory (uuid)
#' @param hashtag Mandatory (string), the hashtag to add
#' @param token Mandatory (string)
#' @param pageStart Optional (uuid)
#' @param listAll Optional (logical) default TRUE
#' @roxygen_header1
path.usage.getActiveUsers <- function(pathId, token, listAl = TRUE, pageStart = NULL)
{
  pageStart <- validUuid("pageStart", pageStart, FALSE)
  token <- validString("token", token, TRUE)
  listAll <- validBool("listAll", listAll, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)

  body <- list("pageStart" = pageStart)

  f <- function(body)
  {
    return(httr::content(apiManager_get(glue::glue("/path/{pathId}/usage/user"), body, token)))
  }

  r <- recurse(f, body, listAll)
  return(r)
}

#' @export
#' Get usage history of a specific user for a given path
#' @param pathId Mandatory (uuid)
#' @param userId Mandatory (uuid)
#' @param token Mandatory (string)
#' @roxygen_header1
path.usage.getUsage <- function(pathId, userId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  userId <- validUuid("userId", userId, TRUE)
  token <- validString("token", token, TRUE)

  r <- apiManager_get(glue::glue("/path/{pathId}/usage/user/{userId}/processingUnits"), NULL, token)
  return(httr::content(r))
}

#' @export
#' Get aggregated usage history
#' @param pathId Mandatory (uuid)
#' @param token Mandatory (string)
#' @param loggedIn Optional (logical) default TRUE
#' @roxygen_header1
path.usage.getAggregatedUsage <- function(pathId, loggedIn = TRUE, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  loggedIn <- validBool("loggedIn", loggedIn, TRUE)
  token <- validString("token", token, TRUE)

  r <- apiManager_get(glue::glue("/path/{pathId}/processingUnits"), list("loggedIn" = loggedIn), token)
  return(httr::content(r))
}
