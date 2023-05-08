#' @export
path.vector.timestamp.feature.message.get <- function(pathId, timestampId, featureIds = NULL, userId = NULL, messageIds = NULL, listAll = TRUE, deleted = FALSE, extent = NULL, pageStart = NULL, token = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid('timestampId', timestampId, TRUE)
  token <- validString("token", token, FALSE)
  userId <- validUuid("userId", userId, FALSE)
  messageIds <- validUuidArray("messageIds", messageIds, FALSE)
  featureIds <- validUuidArray("featureIds", featureIds, FALSE)
  listAll <- validBool("listAll", listAll, TRUE)
  deleted <- validBool("deleted", deleted, TRUE)
  extent <- validBounds("extent", extent, FALSE)
  pageStart <- validUuid("pageStart", pageStart, FALSE)

  body <- list("userId" = userId, "messageIds" = messageIds, "deleted" = deleted, "extent" = extent, "featureIds" = featureIds, "pageStart" = pageStart)

  f <- function(body)
  {
    r <- apiManager_get(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/message"), body, token)
    return(httr::content(r))
  }

  r <- recurse(f, body, listAll)

  dateList <- list()
  for (x in dateList[["result"]])
  {
    x[["date"]] <- stringToDate(x[["date"]])
    # If wrong, maybe remove list command
    dateList <- append(dateList, list(x))
  }
  r[["result"]] <- dateList
  return(r)
}


#' @export
path.vector.timestamp.feature.message.getImage <- function(pathId, timestampId, messageId, token = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  messageId <- validUuid("messageId", messageId, TRUE)
  token <- validString("token", token, FALSE)

  r <- apiManager_get(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/message/{messageId}/image"), NULL, token, FALSE)
  img_type <- httr::http_type(r)
  image_data <- httr::content(r)

  if (httr::status_code(r) != 200)
  {
    errorMessage <- httr::http_status(r)
    stop(glue::glue("ValueError: {errorMessage}"))
  }

  if (img_type == "image/png")
  {
    img <- png::readPNG(img_data)
  }
  else if(img_type == "image/jpeg")
  {
    img <- jpeg::readJPEG(img_data)
  }
  else
  {
    img <- img_data
  }

  return(img)
}


#' @export
path.vector.timestamp.feature.message.add <- function(pathId, timestampId, featureId, token, text = NULL, image = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  featureId <- validUuid("featureId", featureId, TRUE)
  token <- validString("token", token, TRUE)
  text <- validString("text", text, FALSE)
  image <- validImage("image", image, FALSE)

  if (!is.null(image))
  {
    image <- jpeg::writeJPEG(image, raw())
    img_str <- base64encode::base64encode(image)
    img_str <- glue::glue("data:image/jpeg;base64{img_str}")
  }
  else
  {
    img_str <- NULL
  }

  body <- list("image" = img_str, "text" = text)
  r <- apiManager_post(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/{featureId}/message"), body, token)
  return(httr::content(r))
}

#' @export
path.vector.timestamp.feature.message.trash <- function(pathId, timestampId, messageId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  messageId <- validUuid("featureId", messageId, TRUE)
  token <- validString("token", token, TRUE)

  body <- list("trashed" = TRUE)
  r <- apiManager_put(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/message/{messageId}/trashed"), body, token)
}

#' @export
path.vector.timestamp.feature.message.recover <- function(pathId, timestampId, messageId, token)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  messageId <- validUuid("featureId", messageId, TRUE)
  token <- validString("token", token, TRUE)

  body <- list("trashed" = FALSE)
  r <- apiManager_put(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/message/{messageId}/trashed"), body, token)
}
