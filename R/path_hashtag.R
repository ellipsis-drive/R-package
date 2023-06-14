#' Adds a hashtag to a map
#' @param pathId Mandatory (uuid) the id of the map to add the hashtag to
#' @param hashtag Mandatory (string) the hashtag to add
#' @param token Mandatory (string)
#' @return ...
#' @export
path.hashtag.add <-function(pathId, hashtag, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, TRUE)
  hashtag <- validString("hashtag", hashtag, TRUE)

  return(httr::content(apiManager_post(glue::glue("/path/{pathId}/hashtag"), list(
    "hashtag" = hashtag
  ), token)))
}

#' Delete a hashtag from a given path
#' @param pathId Mandatory (uuid) the id of the map to remove the hashtag from
#' @param hashtag Mandatory (string) the hashtag to remove
#' @param token Mandatory (string)
#' @return ...
#' @export
path.hashtag.delete <- function(pathId, hashtag, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  hashtag <- validString("hashtag", hashtag, TRUE)
  return(httr::content(apiManager_delete(glue::glue("/path/{pathId}/hashtag/{hashtag}"), NULL, token)))
}

#' Retrieves all existing hashtags that contain the given string
#' @param hashtag Mandatory (string) the string to search for
#' @return ...
#' @export
path.hashtag.search <- function()
{
  hashtag <- validString("hashtag", hashtag, TRUE)
  return(httr::content(apiManager_get("/path/hashtag", list(
    "hashtag" = hashtag
  ), NULL)))
}
