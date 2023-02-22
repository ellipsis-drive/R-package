uPaste <- function(string1, string2)
{
  return(gsub(" ", "", paste(string1, string2)))
}

jsonDumps <- function(value)
{
  return(cat(gsub(" \\]", "]", gsub("\\[ ", "[", gsub(" \\}", "}", gsub("\\{ ", "{", gsub("\ +", " ", gsub("\\n", "", value))))))))
}

isSingleString <- function(input) {
  is.character(input) & length(input) == 1
}

recurse <- function(f, body, listAll, extraKey = NULL)
{
  r <- f(body)
  if (listAll == TRUE)
  {
    nextPageStart <- r[["nextPageStart"]]
    while (!is.null(nextPageStart))
    {
      body[["pageStart"]] <- nextPageStart
      r_new <- f(body)
      nextPageStart <- r_new[["nextPageStart"]]
      if (!is.null(r[["size"]]))
        r[["size"]] <- uPaste(r[["size"]], r_new[["size"]])
      if (is.null(extraKey))
        r[["result"]] <- uPaste(r[["result"]], r_new[["result"]])
      else
        r[["result"]][[extraKey]] = uPaste(r[["result"]][[extraKey]], r_new[["result"]][[extraKey]])
    }
    r[["nextPageStart"]] <- NULL
  }
  return(r)
}
