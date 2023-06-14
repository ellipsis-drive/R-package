#' Create an access token
#' @param description Mandatory (string) a description for the token to be created
#' @param list Mandatory (list, vector) a list of named lists. Each named list must contain the properties pathId and accessLevel
#' @param token Mandatory (string)
#' @param validFor Optional (int) number of seconds that the token should remain valid. If NULL token will remain valid forever.
#' @return
#' @export
account.accessToken.create <- function(description, accessList, token, validFor = NULL)
{
  token <- validString("token", token, TRUE)
  description <- validString("description", description, TRUE)
  accessList <- validObject("accessList", accessList, TRUE)
  validFor <- validInt("validFor", validFor, FALSE)

  return(httr::content(apiManager_post("/account/security/accessToken"), list("description" = description, "validFor" = validFor, "accessList" = accessList, "scope" = "all"), token))
}

#' @export
#' Revoke a personal access token
#' @param accessTokenId Mandatory (uuid), the id of the token to invalidate
#' @param token Mandatory (string)
#' @return
#' @export
account.accessToken.revoke <- function(accessTokenId, token)
{
  accessTokenId <- validUuid("accessTokenId", accessTokenId, TRUE)
  token <- validString("token", token, TRUE)

  return(httr::content(apiManager_delete(glue::glue("/account/security/accessToken/{accessTokenId}"), NULL, token)))
}

#' @export
#' Retrieve all current tokens
#' @param token Mandatory (string)
#' @param pageStart Optional (uuid)
#' @param listAll Optional (logical) default TRUE, whether to recursively fetch all tokens or only one page
#' @return
#' @export
account.accessToken.get <- function(token, pageStart = NULL, listAll = TRUE)
{
  token <- validString("token", token, TRUE)
  listAll <- validBool("listAll", listAll, TRUE)
  pageStart <- validObject("pageStart", pageStart, FALSE)

  body <- list("pageStart" = pageStart)
  f <- function(body)
  {
    return(httr::content(apiManager_get("/account/security/accessToken"), body, token))
  }

  r <- recurse(f, body, listAll)
  return(r)
}
