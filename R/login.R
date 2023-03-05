#source("R/sanitize.R")
#source("R/apiManager.R")

#' Login a user
#'
#' This function logs in a user, and returns a token the user can then use to
#' call restricted methods. It takes the strings username and password as input
#' and returns a string token as output. The user can provide an optional argument
#' validFor to indicate the time validity of the token.
#'
#' @param username a string representing the username
#' @param password a string representing the password
#' @param validFor an integer indicating how long the token is valid
#' @return a string representing the login token
#' @roxygen_header1
#' @export
account.logIn <- function(username, password, validFor = NULL)
{
  username <- validString("username", username, TRUE)
  password <- validString("password", password, TRUE)
  validFor <- validInt("validFor", validFor, FALSE)
  json <- list("username" = username, "password" = password, "validFor" = validFor)
  r <- apiManager_post("/account/login", json)
  token <- r$"token"
  return(token)
}

#' Retrieve all layers and folders inside a specific folder
#'
#' This function retrieves all layers and folders inside a specific folder,
#' and retrieves information about the folder itself.
#'
#' @param rootName a string representing the ID of the folder to be searched
#' @param token a string representing the login token
#' @param pathTypes a list representing the types of content that will be retrieved
#' @param pageStart a uuid representing the page on which the work starts
#' @param listAll a boolean controlling whether to retrieve only the first page or all pages
#' @return a dictionary/json table containing the content that is retrieved
#' @export
account.listRoot <- function(rootName, token, pathTypes = NULL, pageStart = NULL, listAll = TRUE)
{
  token = validString("token", token, TRUE)
  rootName = validString("rootName", rootName, TRUE)
  pageStart = validUuid("pageStart", pageStart, FALSE)
  listAll = validBool("listAll", listAll, TRUE)
  pathTypes = validObject("pathTypes", pathTypes, FALSE)
  if (is.null(pathTypes))
    pathTypes <- c("folder", "raster", "vector", "file")

  url <- uPaste("/account/root/", rootName)
  body <- list("type" = pathTypes, "pageStart" = pageStart)

  f <- function(body)
  {
    r <- apiManager_get(url, body, token)
    return(r)
  }
  r <- recurse(f, body, listAll)
  return(r)
}
