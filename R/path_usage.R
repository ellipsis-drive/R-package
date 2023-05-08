#' @export
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
path.usage.getUsage <- function(pathId, userId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  userId <- validUuid("userId", userId, TRUE)
  token <- validString("token", token, TRUE)

  r <- apiManager_get(glue::glue("/path/{pathId}/usage/user/{userId}/processingUnits"), NULL, token)
  return(httr::content(r))
}

#' @export
path.usage.getAggregatedUsage(pathId, loggedIn, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  loggedIn <- validBool("loggedIn", loggedIn, TRUE)
  token <- validString("token", token, TRUE)

  r <- apiManager_get(glue::glue("/path/{pathId}/processingUnits"), list("loggedIn" = loggedIn), token)
  return(httr::content(r))
}
