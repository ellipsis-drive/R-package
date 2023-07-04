#' Get all messages of a feature
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param token Optional (string)
#' @param messageIds Optional (list, array, or vector of uuids)
#' @param userId Optional (uuid)
#' @param extent Optional (named list) with properties xMin, xMax, yMin, yMax
#' @param pageStart Optional (uuid)
#' @param listAll Optional (logical) default FALSE
#' @param deleted Optional (logical) default FALSE
#' @return ...
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
  for (x in r[["result"]])
  {
    x[["date"]] <- stringToDate(x[["date"]])
    # If wrong, maybe remove list command
    dateList <- append(dateList, list(x))
  }
  r[["result"]] <- dateList
  return(r)
}


#' Get the image of a message
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param messageId Mandatory (uuid)
#' @param token Optional (string)
#' @return Depending on img_type (jpeg, png, raw bytes)
#' @export
path.vector.timestamp.feature.message.getImage <- function(pathId, timestampId, messageId, token = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  messageId <- validUuid("messageId", messageId, TRUE)
  token <- validString("token", token, FALSE)
  r <- apiManager_get(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/message/{messageId}/image"), NULL, token, FALSE)
  img_type <- httr::http_type(r)
  img_data <- httr::content(r)
  if (httr::status_code(r) != 200)
  {
    errorMessage <- httr::http_status(r)
    stop(glue::glue("ValueError: {errorMessage}"))
  }
  image_file <- tempfile(fileext = ".jpg")
  if (img_type == "image/png")
  {
    png::writePNG(img_data, target = image_file)
    img <- png::readPNG(image_file)
    unlink(image_file)
  }
  else if(img_type == "image/jpeg")
  {
    jpeg::writeJPEG(img_data, image_file)
    img <- jpeg::readJPEG(image_file)
    unlink(image_file)
  }
  else
  {
    img <- img_data
  }

  return(img)
}


#' Add message to a feature
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param featureId Mandatory (uuid)
#' @param token Mandatory (string)
#' @param text Optional (string)
#' @param image Optional (array, matrix)
#' @return ...
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
    # Save image to a temporary file
    tmp_file <- tempfile(fileext = ".png")
    png::writePNG(image, tmp_file)

    # Read the image back as a raw vector
    buffered <- readBin(tmp_file, "raw", file.info(tmp_file)$size)
    file.remove(tmp_file)

    # Convert buffer to base64-encoded string
    img_str <- openssl::base64_encode(buffered)
    img_str <- paste0("data:image/png;base64,", img_str)
  }
  else
  {
    img_str <- NULL
  }

  body <- list("image" = img_str, "text" = text)
  print(img_str)
  r <- apiManager_post(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/{featureId}/message"), body, token)
  return(httr::content(r))
}

#' Move message of a feature to the trash
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param messageId Mandatory (uuid)
#' @param token Mandatory (string)
#' @return ...
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

#' Recover message of a feature from the trash
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param messageId Mandatory (uuid)
#' @param token Mandatory (string)
#' @return ...
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
