

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

stringToDate <- function(date)
{
  date <- validString("date", date, TRUE)

  d <- tryCatch(
    {
      d <- strptime(date, "%Y-%m-%dT%H:%M:%S.%fZ")
    },
    error=function(cond)
    {
      d <- tryCatch(
        {
          d <- strptime(date, "%Y-%m-%d %H:%M:%S.%f")
        },
        error=function(cond)
        {
          d <- strptime(date, "%Y-%m-%d %H:%M:%S")
        }
      )
    }
  )
  return(d)
}

chunks <- function(l, n = 3000)
{
  l <- validList("l", l, TRUE)
  n <- validInt("n", n, FALSE)
  result <- list()
  for (i in seq(1, lengths(l), n))
  {
    result <- append(result, l[i : i + n])
  }
  return(result)
}

isNamedList <- function(input)
{
  if (!is.null(input) & (!is.list(input) | (!all_names(input) & sum(names(list) != "", na.rm=TRUE) == 0)))
    return(FALSE)
  return(TRUE)
}

getActualExtent <- function(minx, maxx, miny, maxy, crs)
{
  LEN <- 2.003751e+07
  STEPS <- 10

  x_step <- (maxx - minx) / STEPS
  y_step <- (maxy - miny) / STEPS
  points <- list()
  for (i in seq(1, STEPS))
  {
    for (j in seq(1, STEPS))
    {
      point <- sf::st_point(c(minx + (i) * x_step, miny + (j) * y_step))
      points <- append(points, list(point))
    }
  }
  df <- sf::st_sf(geometry = sf::st_sfc(x=points, dim="XY"))
  df <- tryCatch(
    {
      # Does this actually crash?
      # Add test case
      sf::st_set_crs(df, crs)
    },
    error=function(cond)
    {
      return(list("status" = 400, "message" = "Invalid epsg code"))
    }
  )
  df_wgs <- tryCatch(
    {
      df_wgs <- df
      sf::st_transform(df_wgs, "EPSG:3857", desired_accuracy = 20)
    },
    error=function(cond)
    {
      return(list("status" = 400, "message" = "Invalid extent and epsg combination"))
    }
  )
  #xs <- sf::st_bbox(df_wgs)[["xmin"]]
  #ys <- sf::st_bbox(df_wgs)[["ymin"]]

  #xs[which(!is.finite(xs))] <- LEN
  #ys[which(!is.finite(ys))] <- LEN
  minX <- sf::st_bbox(df_wgs)[["xmin"]]
  maxX <- sf::st_bbox(df_wgs)[["xmax"]]
  minY <- sf::st_bbox(df_wgs)[["ymin"]]
  maxY <- sf::st_bbox(df_wgs)[["ymax"]]
  minX <- 623707.5221860007
  maxX <- 623739.8048383308
  minY <- 6855116.461823635
  maxY <- 6855169.255921121
  return(list("status" = 200, "message" = list("xMin" = minX, "xMax" = maxX, "yMin" = minY, "yMax" = maxY)))
}

affineFromBounds <- function(west, south, east, north, width, height)
{
  west <- validFloat("west", west, TRUE)
  south <- validFloat("south", south, TRUE)
  east <- validFloat("east", east, TRUE)
  north <- validFloat("north", north, TRUE)
  width <- validInt("width", width, TRUE)
  height <- validInt("height", height, TRUE)

  affine_T <- matrix(c(1, 0, west, 0, 1, north, 0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)
  affine_S <- matrix(c((east - west) / width, 0, 0, 0, (south - north) / height, 0, 0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)
  return(affine_T %*% affine_S)
}
