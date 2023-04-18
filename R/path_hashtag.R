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

#' @export
path.hashtag.delete <- function(pathId, hashtag, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  token <- validString("token", token, FALSE)
  hashtag <- validString("hashtag", hashtag, TRUE)
  return(httr::content(apiManager_delete(glue::glue("/path/{pathId}/hashtag/{hashtag}"), NULL, token)))
}

#' @export
path.hashtag.search <-function()
{
  hashtag <- validString("hashtag", hashtag, TRUE)
  return(httr::content(apiManager_get("/path/hashtag", list(
    "hashtag" = hashtag
  ), NULL)))
}
