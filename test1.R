library(jsonlite)
library(httr)

res = httr::GET("https://api.open-elevation.com/api/v1/lookup?locations=49.1612143,18.8405987")
data = jsonlite::fromJSON(rawToChar(res$content))$results$elevation

latitude <- 49.1612143
longitude <- 18.8405987
distance <- 150

westStart <- geosphere::destPoint(c(longitude, latitude), 270, distance*75)

north <- destPoint(westStart, 0, seq(from = distance, to = (distance*75), by = distance))
south <- destPoint(westStart, 180, seq(from = distance, to = (distance*75), by = distance))

westCol <- rbind(north[order(nrow(north):1),], westStart, south)

allCols <- apply(westCol, 1, function(x) (data.table::as.data.table(destPoint(x, 90, seq(from = 0, to = (distance*150), by = distance)))))

allPts <- data.table::rbindlist(allCols)
allPts <- data.table::as.data.table(allPts)
allPts <- allPts[, j = .(x = round(lat, 6), y = round(lon, 6))]
allPts <- allPts[1:100]
ptsList <- lapply(seq_len(nrow(allPts)), function(i) lapply(allPts, "[", i))

get_elevation <- function(coordinates) {
  
  part1 <- 'https://api.open-elevation.com/api/v1/lookup?'
  part2 <- paste0("locations=", coordinates$x, ",", coordinates$y)
  
  res = httr::GET(
    url = part1,
    query = part2
  )
  
  data = rawToChar(res$content)
  
  return(data)
  
}

check1 <- lapply(ptsList, get_elevation)

lapply(check1, rawToChar)
