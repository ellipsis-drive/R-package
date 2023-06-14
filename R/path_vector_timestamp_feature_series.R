#' Get series of a vector timestamp feature
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param featureId Mandatory (uuid)
#' @param pageStart Optional (uuid)
#' @param dateTo Optional (date)
#' @param userId Optional (uuid)
#' @param seriesProperty Optional (string)
#' @param trashed Optional (logical)
#' @param listAll Optional (logical)
#' @param token Optional (string)
#' @return (data.frame) containing the series
#' @export
path.vector.timestamp.feature.series.get <- function(pathId, timestampId, featureId, pageStart = NULL, dateTo = NULL, userId = NULL, seriesProperty = NULL, deleted = FALSE, listAll = TRUE, token = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  featureId <- validUuid("featureId", featureId, TRUE)
  pageStart <- validUuid("pageStart", pageStart, FALSE)
  dateTo <- validDate("dateTo", dateTo, FALSE)
  userId <- validUuid("userId", userId, FALSE)
  seriesProperty <- validString("seriesProperty", seriesProperty, FALSE)
  deleted <- validBool("deleted", deleted, TRUE)
  listAll <- validBool("listAll", listAll, TRUE)
  token <- validString("token", token, FALSE)

  body = list("pageStart" = pageStart, "dateTo" = dateTo, "userId" = userId, "seriesProperty" = seriesProperty, "deleted" = deleted, "listAll" = listAll)

  f <- function(body)
  {
    r <- apiManager_get(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/{featureId}/series/element"), body, token)
    return(httr::content(r))
  }

  r <- recurse(f, body, listAll)
  series <- r[["result"]]
  seriesList <- list()
  for (k in series)
  {
    seriesList <- append(seriesList, list("id" = k[["id"]], "property" = k[["property"]], "value" = k[["value"]], "date" = stringToDate(k[["date"]])))
  }
  series <- data.frame(seriesList)
  r[["result"]] <- series

  return(r)
}

#' Get info about series of a vector timestamp feature
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param featureId Mandatory (uuid)
#' @param token Optional (string)
#' @return
#' @export
path.vector.timestamp.feature.series.info <- function(pathId, timestampId, featureId, token = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  featureId <- validUuid("featureId", featureId, TRUE)
  token <- validString("token", token, FALSE)

  r <- apiManager_get(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/{featureId}/series/info"), NULL, token)
  r <- httr::content(r)
  r[["dateFrom"]] <- stringToDate(r[["dateFrom"]])
  r[["dateTo"]] <- stringToDate(r[["dateTo"]])

  return(r)
}

#' Add series to a vector timestamp feature
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param featureId Mandatory (uuid)
#' @param seriesData Mandatory (data.frame)
#' @param token Mandatory (string)
#' @return
#' @export
path.vector.timestamp.feature.series.add <- function(pathId, timestampId, featureId, seriesData, token, showProgress = TRUE)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  featureId <- validUuid("featureId", featureId, TRUE)
  token <- validString("token", token, TRUE)
  seriesData <- validDataframe("seriesData", seriesData, TRUE)
  showProgress <- validBool("showProgress", showProgress, TRUE)

  if ("date" %in% colnames(seriesData))
  {
    is.date <- function(x) inherits(x, "Date")
    checks <- sapply(as.Date(seriesData[["date"]]), is.date)
    if (!FALSE %in% checks)
    {
      seriesData[["date"]] <- as.POSIXct(as.Date(seriesData[["date"]]), format = '%d%b%Y:%H:%M:%S')
      dates <- toString(seriesData[["date"]])
      seriesData <- seriesData[ ,  !names(seriesData) %in% c("date")]
    }
    else
    {
      stop("ValueError: datetime column must be of type datetime")
    }
  }
  else
  {
    stop("ValueError: seriesDatamust have a column date of type datetime")
  }

  # Isn't this unsafe
  for (name in colnames(seriesData))
  {
    seriesData[[name]] <- tryCatch(
      {
        seriesData[[name]] <- as.double(seriesData[[name]])
      },
      error=function(cond)
      {
        stop(glue::glue("ValueError: could not cast data in column {name} to type double"))
      }
    )
  }

  values <- list()

  for (i in seq(dim(seriesData)[[1]]))
  {
    for (c in colnames(seriesData))
    {
      value <- seriesData[[c]][[i]]
      if (!is.nan(value))
      {
        values <- append(values, list("property" = c, "value" = seriesData[[c]][[i]], "date" = dates[[i]]))
      }
    }
  }

  chunks_values <- chunks(values)
  N <- 0
  r_total <- list()
  for (values_sub in chunks_values)
  {
    body <- list("values" = values_sub)
    r <- apiManager_post(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/{featureId}/series/element"), body, token)

    r_total <- append(r_total, list(httr::content(r)))

    N <- N + 1
  }

  return(r_total)
}

#' Move series of a vector timestamp feature to the trash
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param featureId Mandatory (uuid)
#' @param seriesIds Mandatory (array, list, or vector of uuids)
#' @param token Mandatory (string)
#' @return
#' @export
path.vector.timestamp.feature.series.trash <- function(pathId, timestampId, featureId, seriesIds, token, showProgress = TRUE)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  featureId <- validUuid("featureId", featureId, TRUE)
  token <- validString("token", token, TRUE)
  seriesIds <- validUuidArray("seriesIds", seriesIds, TRUE)
  showProgress <- validBool("showProgress", showProgress, TRUE)

  chunks_values <- chunks(seriesIds)
  N <- 0
  for (seriesIds_sub in chunks_values)
  {
    body <- list("seriesIds" = seriesIds_sub, "trashed" = TRUE)
    r <- apiManager_put(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/{featureId}/series/element/trashed"), body, token)

    r_total <- append(r_total, list(httr::content(r)))

    N <- N + 1
  }
}

#' Recover series of a vector timestamp feature from the trash
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param featureId Mandatory (uuid)
#' @param seriesIds Mandatory (array, vector, or list of uuids)
#' @param token Mandatory (string)
#' @return
#' @export
path.vector.timestamp.feature.series.recover <- function(pathId, timestampId, featureId, seriesIds, token, showProgress = TRUE)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  featureId <- validUuid("featureId", featureId, TRUE)
  token <- validString("token", token, TRUE)
  seriesIds <- validUuidArray("seriesIds", seriesIds, TRUE)
  showProgress <- validBool("showProgress", showProgress, TRUE)

  chunks_values <- chunks(seriesIds)
  N <- 0
  for (seriesIds_sub in chunks_values)
  {
    body <- list("seriesIds" = seriesIds_sub, "trashed" = FALSE)
    r <- apiManager_put(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/{featureId}/series/element/trashed"), body, token)

    r_total <- append(r_total, list(httr::content(r)))

    N <- N + 1
  }
}

#' Get changelog of series of a vector timestamp feature
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param featureId Mandatory (uuid)
#' @param listAll Optional (logical) default FALSE
#' @param actions Optional (array, list, or vector of strings)
#' @param userId Optional (uuid)
#' @param pageStart Optional (uuid)
#' @param token Optional (string)
#' @return
#' @export
path.vector.timestamp.feature.series.changelog <- function(pathId, timestampId, featureId, listAll = FALSE, actions = NULL, userId = NULL, pageStart = NULL, token = NULL)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId <- validUuid("timestampId", timestampId, TRUE)
  featureId <- validUuid("featureId", featureId, TRUE)
  listAll <- validBool("listAll", listAll, TRUE)
  actions <- validStringArray("actions", actions, FALSE)
  userId <- validUuid("userId", userId, FALSE)
  pageStart <- validUuid("pageStart", pageStart, FALSE)
  token <- validString("token", token, FALSE)

  body <- list("userId" = userId, "actions" = actions, "pageStart" = pageStart)

  f <- function(body)
  {
    r <- apiManager_get(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/{featureId}/series/changelog"), body, token)
    return(httr::content(r))
  }

  r <- recurse(f, body, listAll)

  result_list <- list()
  for (x in r[["result"]])
  {
    x[["date"]] <- stringToDate(x[["date"]])
    result_list <- append(result_list, list(x))
  }
  r[["result"]] <- result_list
  return(r)
}
