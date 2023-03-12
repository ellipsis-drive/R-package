#' @export
path.raster.timestamp.add <- function(pathId, token, description = NULL, date = list("from" = Sys.time(), "to" = Sys.time()))
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  date <- validDateRange("date", date, TRUE)
  description <- validString("description", description, FALSE)
  body = list("date" = date, "description" = description)
  return(apiManager_post(glue::glue("/path/{pathId}/raster/timestamp"), body, token))
}

