# functions to calculate distances from points to rivershapefile
shapewater <- shapefile("Daten/DEU_water_lines_dcw.shx")
points <- readRDS("Daten/evidence.csv") %>%
  dplyr::select(lon, lat) %>%
  head(n = 100)
data(wrld_simpl, package = "maptools")
wrld_subset <- wrld_simpl[wrld_simpl@data$ISO2 %in% c("DE"), ]
# splitframe function to split data.frames into lists of equal length for parallelization
splitframe <- function(df, splits) {
  if (mod(nrow(df), splits) != 0) {
    stop("nrow is not a multiple of splits")
  }
  pointsliste <- list()
  j <- 0
  for (i in 1:splits) {
    pointsliste[[i]] <- df[(j + 1):(i * (nrow(df) / splits)), ]
    j <- j + (nrow(df) / splits)
  }
  return(pointsliste)
}

# function to calculate distances in parallel ----
pardist2water <- function(pnts, shapef = shapewater, splits = 10, workers = 4) {
  pnts <- as.data.frame(pnts)
  # split into equal parts
  pointsliste <- splitframe(pnts, splits)
  registerDoParallel(cores = workers)
  tmp <- foreach(i = 1:splits) %dopar% {
    geosphere::dist2Line(p = pointsliste[[i]], line = shapef)
  }
  # unlist tmp to data.frame
  tmp <- do.call(rbind, tmp)
  # bind results with original points
  tmp <- cbind(pnts, tmp)
  return(tmp)
}

riverdists <- pardist2water(points)
set.seed(123)
riverdists <- riverdists[sample(nrow(riverdists), 10), ]
saveRDS(riverdists, file = "Daten/riverdists.rds")

# function to plot points and shortest way to rivers as arrows ----
plot2water <- function(df, rows = 100, colorPoints = "red", colorArrows = "green", shp = shapewater, 
                       wid = 3) {
  if (rows > nrow(df)) {
    warning("n larger than nrow of data.frame. Setting n equal to nrow(data.frame).")
    rows <- nrow(df)
  }
  temp <- sp::SpatialPoints(
    coords = head(df[, c("lon", "lat")], n = rows), # order matters
    proj4string = wrld_subset@proj4string
  )
  plot(temp, col = colorPoints)
  plot(shp, add = TRUE, lwd = wid)
  # plot arrows to indicate the direction of the great-circle-distance
  for (i in 1:rows) {
    arrows(
      x0 = df[i, 1],
      y0 = df[i, 2],
      x1 = df[i, 4],
      y1 = df[i, 5],
      length = 0.1,
      col = colorArrows,
      lwd = wid
    )
  }
}
saveRDS(riverdists, file = "Daten/riverdists.rds")
plot2water(df = riverdists, rows = 10)
