#' @export
#' Add a feature property to a given vector
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param name Mandatory (string)
#' @param featureProperty Mandatory (string)
#' @param token Mandatory (string)
#' @param private Optional (logical) default FALSE
#' @param required Optional (logical) default FALSE
#' @roxygen_header1
path.vector.featureProperty.add <- function(pathId, name, featurePropertyType, token, private = FALSE, required = FALSE)
{
  pathId <- validUuid("pathId", pathId, TRUE)
  name <- validString("name", name, TRUE)
  featurePropertyType <- validString("featurePropertyType", featurePropertyType, TRUE)
  private <- validBool("private", private, TRUE)
  required <- validBool("required", required, TRUE)
  token <- validString("token", token, TRUE)

  body <- list("name" = name, "private" = private, "required" = required, "type" = featurePropertyType)

  r <- apiManager_post(glue::glue("/path/{pathId}/vector/property"), body, token)
  return(httr::content(r))
}

#' @export
#' Edit the feature property for a given vector
#' @param pathId Mandatory (uuid)
#' @param timestampId Mandatory (uuid)
#' @param featurePropertyId Mandatory (uuid)
#' @param token Mandatory (string)
#' @param private Optional (logical) default FALSE
#' @param required Optional (logical) default FALSE
#' @roxygen_header1
path.vector.featureProperty.edit <- function(pathId, featurePropertyId, token, private = FALSE, required = FALSE)
{
  private <- validBool("private", private, TRUE)
  required <- validBool("required", required, TRUE)
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  featurePropertyId <- validUuid("featurePropertyId", featurePropertyId, TRUE)

  body <- list("private" = private, "required" = required)

  r <- apiManager_patch(glue::glue("/path/{pathId}/vector/property/{featurePropertyId}"), body, token)
  return(httr::content(r))
}

#' @export
#' Move a feature property for a given vector to the trash
#' @param pathId Mandatory (uuid)
#' @param featurePropertyId Mandatory (uuid)
#' @param token Mandatory (string)
#' @roxygen_header1
path.vector.featureProperty.trash <- function(pathId, featurePropertyId, token)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  featurePropertyId <- validUuid("featurePropertyId", featurePropertyId, TRUE)

  body <- list("trashed" = TRUE)
  r <- apiManager_put(glue::glue("/path/{pathId}/vector/property/{featurePropertyId}/trashed"), body, token)
  return(httr::content(r))
}

#' @export
#' Recover a feature property for a given vector
#' @param pathId Mandatory (uuid)
#' @param featurePropertyId Mandatory (uuid)
#' @param token Mandatory (string)
#' @roxygen_header1
path.vector.featureProperty.recover <- function(pathId, featurePropertyId, token)
{
  token <- validString("token", token, TRUE)
  pathId <- validUuid("pathId", pathId, TRUE)
  featurePropertyId <- validUuid("featurePropertyId", featurePropertyId, TRUE)

  body <- list("trashed" = FALSE)
  r <- apiManager_put(glue::glue("/path/{pathId}/vector/property/{featurePropertyId}/trashed"), body, token)
  return(httr::content(r))
}
