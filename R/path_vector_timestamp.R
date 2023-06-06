#' @export
path.vector.timestamp.manageLevels <- function(levelOfDetail1, levelOfDetail2, levelOfDetail3, levelOfDetail4, levelOfDetail5)
{
  levelOfDetail1 <- validGeoSeries("levelOfDetail1", levelOfDetail1, FALSE)
  levelOfDetail2 <- validGeoSeries("levelOfDetail2", levelOfDetail2, FALSE)
  levelOfDetail3 <- validGeoSeries("levelOfDetail3", levelOfDetail3, FALSE)
  levelOfDetail4 <- validGeoSeries("levelOfDetail4", levelOfDetail4, FALSE)
  levelOfDetail5 <- validGeoSeries("levelOfDetail5", levelOfDetail5, FALSE)

  if (!is.null(levelOfDetail1) & length(levelOfDetail1) != length(features))
    stop(glue::glue("ValueError: levelOfDetail1 must have same length as number of rows in features"))
  if (!is.null(levelOfDetail2) & length(levelOfDetail2) != length(features))
    stop(glue::glue("ValueError: levelOfDetail2 must have same length as number of rows in features"))
  if (!is.null(levelOfDetail3) & length(levelOfDetail3) != length(features))
    stop(glue::glue("ValueError: levelOfDetail3 must have same length as number of rows in features"))
  if (!is.null(levelOfDetail4) & length(levelOfDetail4) != length(features))
    stop(glue::glue("ValueError: levelOfDetail4 must have same length as number of rows in features"))
  if (!is.null(levelOfDetail5) & length(levelOfDetail5) != length(features))
    stop(glue::glue("ValueError: levelOfDetail5 must have same length as number of rows in features"))

  if (!is.null(levelOfDetail2) & is.null(levelOfDetail1))
    stop("ValueError: If levelOfDetail2 is defined, so must levelOfDetail1")
  if (!is.null(levelOfDetail3) & is.null(levelOfDetail2))
    stop("ValueError: If levelOfDetail3 is defined, so must levelOfDetail2")
  if (!is.null(levelOfDetail4) & is.null(levelOfDetail3))
    stop("ValueError: If levelOfDetail4 is defined, so must levelOfDetail3")
  if (!is.null(levelOfDetail5) & is.null(levelOfDetail4))
    stop("ValueError: If levelOfDetail5 is defined, so must levelOfDetail4")

  if (!is.null(levelOfDetail1))
  {
    temp = list()
    for (x in levelOfDetail1[["features"]])
    {
      temp = list.append(temp, st_geometry(x))
    }
    levelOfDetail1 <- array(unlist(temp))
  }
  if (!is.null(levelOfDetail2))
  {
    temp = list()
    for (x in levelOfDetail2[["features"]])
    {
      temp = list.append(temp, st_geometry(x))
    }
    levelOfDetail2 <- array(unlist(temp))
  }
  if (!is.null(levelOfDetail3))
  {
    temp = list()
    for (x in levelOfDetail3[["features"]])
    {
      temp = list.append(temp, st_geometry(x))
    }
    levelOfDetail3 <- array(unlist(temp))
  }
  if (!is.null(levelOfDetail4))
  {
    temp = list()
    for (x in levelOfDetail4[["features"]])
    {
      temp = list.append(temp, st_geometry(x))
    }
    levelOfDetail4 <- array(unlist(temp))
  }
  if (!is.null(levelOfDetail5))
  {
    temp = list()
    for (x in levelOfDetail5[["features"]])
    {
      temp = list.append(temp, st_geometry(x))
    }
    levelOfDetail5 <- array(unlist(temp))
  }

  return(list(levelOfDetail1, levelOfDetail2, levelOfDetail3, levelOfDetail4, levelOfDetail5))
}

#' @export
#' Maybe use aslist (we want to combine geometries I guess...)
path.vector.timestamp.zipLevels <- function(levelOfDetail1, levelOfDetail2, levelOfDetail3, levelOfDetail4, levelOfDetail5)
{
  if (!is.null(levelOfDetail5))
    levels <- mapply(list, levelOfDetail1, levelOfDetail2, levelOfDetail3, levelOfDetail4, levelOfDetail5)
  if (!is.null(levelOfDetail4))
    levels <- mapply(list, levelOfDetail1, levelOfDetail2, levelOfDetail3, levelOfDetail4)
  if (!is.null(levelOfDetail3))
    levels <- mapply(list, levelOfDetail1, levelOfDetail2, levelOfDetail3)
  if (!is.null(levelOfDetail2))
    levels <- mapply(list, levelOfDetail1, levelOfDetail2)
  levels <- as.list(levelOfDetail1)

  return(levels)
}

#' @export
path.vector.timestamp.add <- function(pathId, timestampId, features, token, showProgress = TRUE, levelOfDetail1 = NULL, levelOfDetail2 = NULL, levelOfDetail3 = NULL, levelOfDetail4 = NULL, levelOfDetail5 = NULL, cores = 1)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId < validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, TRUE)
  features <- validSimplefeature("features", features, TRUE)
  showProgress <- validBool("showProgress", showProgress, TRUE)
  cores <- validInt("cores", cores, TRUE)

  levels <- list[levelOfDetal1, levelOfDetail2, levelOfDetail3, levelOfDetail4, levelOfDetail5] <- path.vector.timestamp.manageLevels(levelOfDetail1, levelOfDetail2, levelOfDetail3, levelOfDetail4, levelOfDetail5, features)
  levelOfDetail1 <- levels[[1]]
  levelOfDetail2 <- levels[[2]]
  levelOfDetail3 <- levels[[3]]
  levelOfDetail4 <- levels[[4]]
  levelOfDetail5 <- levels[[5]]

  firstTime <- httr::content(apiManager_get(glue::glue("/path/{pathId}"), NULL, token))
  if (!"vector" $in$ names(firstTime))
    stop("Can only add features if path is of type vector")
  firstTime <- length(firstTime[["vector"]][["properties"]]) == 0

  if (firstTime)
  {
    print("no properties known for this timestamp. Adding them automatically")
    columns <- colnames(as.data.frame(features))
    for (c in columns)
    {
      if (sapply(features[[c]]) == "integer")
        propertyType <- "integer"
      else if (sapply(features[[c]]) == "double")
        propertyType <- "double"
      else if (sapply(features[[c]]) == "logical")
        propertyType <- "logical"
      else if (sapply(features[[c]]) == "Date")
        propertyType <- "Date"
      else
        propertyType <- "String"

      body <- list("names" = c, "type" = propertyType, "required" = FALSE, "private"= FALSE)
      apiManager_post(glue::glue("/path/{pathId}/vector/property"), body, token)
    }
  }
  # Parallelize later?

  levels <- path.vector.timestamp.zipLevels(levelOfDetail1, levelOfDetail2, levelOfDetail3, levelOfDetail4, levelOfDetail5)
  if (!is.null(levels))
  {
    featuresBody <- list("feature" = feature, "levelsOfDetail" = levels)
    body <- list("feature" = featuresBody)
    r <- httr::content(apiManager_post(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature"), body, token))

  }
  return(r)
}

#' @export
path.vector.timestamp.edit <- function(pathId, timestampId, featureIds, features = NULL, token, showProgress = TRUE, levelOfDetail1 = NULL, levelOfDetail2 = NULL, levelOfDetail3 = NULL, levelOfDetail4 = NULL, levelOfDetail5 = NULL, cores = 1)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId < validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, TRUE)
  features <- validSimplefeature("features", features, FALSE)
  showProgress <- validBool("showProgress", showProgress, TRUE)
  cores <- validInt("cores", cores, TRUE)
  featureIds <- validUuidArray("featureIds", featureIds, TRUE)

  levels <- list[levelOfDetal1, levelOfDetail2, levelOfDetail3, levelOfDetail4, levelOfDetail5] <- path.vector.timestamp.manageLevels(levelOfDetail1, levelOfDetail2, levelOfDetail3, levelOfDetail4, levelOfDetail5, features)
  levelOfDetail1 <- levels[[1]]
  levelOfDetail2 <- levels[[2]]
  levelOfDetail3 <- levels[[3]]
  levelOfDetail4 <- levels[[4]]
  levelOfDetail5 <- levels[[5]]

  if (!is.null(features) & length(features) != length(featureIds))
    stop(glue::glue("featureIds must be of same length as the simple features dataframe"))

  levels <- path.vector.timestamp.zipLevels(levelOfDetail1, levelOfDetail2, levelOfDetail3, levelOfDetail4, levelOfDetail5)
  changes = list()
  if (is.null(levels))
  {
    for (i in seq_along(featureIds, features))
    {
      changes = append(changes, list("featureId" = featureIds[[i]], "newProperties" = features[[i]][["properties"]], "newGeometry" = st_geometry(features[[i]])))
    }
  }
  else
  {
    for (i in seq_along(featureIds, features))
    {
      changes = append(changes, list("featureId" = featureIds[[i]], "levelsOfDetail" = levels,"newProperties" = features[[i]][["properties"]], "newGeometry" = st_geometry(features[[i]])))
    }
  }

  body = list("changes" = changes)
  r <- httr::content(apiManager_patch(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature"), body, token))
  return(r)
}

path.vector.timestamp.trash <- function(pathId, timestampId, featureIds, token, showProgress = TRUE)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId < validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, TRUE)
  showProgress <- validBool("showProgress", showProgress, TRUE)
  featureIds <- validUuidArray("featureIds", featureIds, TRUE)

  body = list("featureIds" = featureIds, "trashed" = TRUE)
  r <- httr::content(apiManager_put(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/trashed"), body, token))
  return(r)
}

path.vector.timestamp.recover <- function(pathId, timestampId, featureIds, token, showProgress = TRUE)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId < validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, TRUE)
  showProgress <- validBool("showProgress", showProgress, TRUE)
  featureIds <- validUuidArray("featureIds", featureIds, TRUE)

  body = list("featureIds" = featureIds, "trashed" = FALSE)
  r <- httr::content(apiManager_put(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/trashed"), body, token))
  return(r)
}

path.vector.timestamp.versions <- function(pathId, timestampId, featureId, token = NULL, pageStart = NULL, listAll = TRUE)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  timestampId < validUuid("timestampId", timestampId, TRUE)
  token <- validString("token", token, FALSE)
  featureId <- validUuid("featureId", featureId, TRUE)
  pageStart <- validUuid("pageStart", pageStart, FALSE)
  listAll <-validBool("listAll", listAll, TRUE)

  body= list("returnType" = "all")
  f <- function(body)
  {
    return(httr::content(apiManager_get(glue::glue("/path/{pathId}/vector/timestamp/{timestampId}/feature/{featureId}/version"), body, token)))
  }

  r <- recurse(f, body, listAll)

  features <- list()
  for (x in r[["result"]])
    features <- append(features, list(x[["feature"]]))

  dates <- list()
  for (x in r[["result"]])
    dates <- append(dates, list(x[["date"]]))

  usernames <- list()
  for (x in r[["result"]])
    usernames <- append(usernames, list(x[["user"]][["usernames"]]))

  userIds <- list()
  for (x in r[["result"]])
    userIds <- append(userIds, list(x[["user"]][["id"]]))

  # Hopefully this works (otherwise look at other instance of from features)
  sh <- features
  sh[["username"]] <- usernames
  sh[["userId"]] <- userIds
  sh[["dates"]] <- dates

  sh <- sf::st_set_crs(sh, 4326)
  r[["result"]] <- sh

  return(r)

}
