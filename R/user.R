#' @export
user.search <- function(username, fuzzySearch = TRUE)
{
  username <- validString("username", username, TRUE)
  fuzzySearch <- validBool("fuzzySearch", fuzzySearch, TRUE)

  body = list("username" = username, "fuzzySearch" = fuzzySearch)
  r <- apiManager_get("/user", body, NULL)
  r <- httr::content(r)

  return(r)
}

#' @export
user.get <- function(userId)
{
  userId <- validUuid("userId", userId, TRUE)

  r <- apiManager_get(glue::glue("/user/{userId}"), NULL, NULL)
  return(httr::content(r))
}
