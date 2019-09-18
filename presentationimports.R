# presentation imports
source("packageloader.R")
library("ROCR") 

preds <- readr::read_csv("Daten/predictions.csv")
preds <- preds[,2:ncol(preds)]
preds <- as.data.frame(preds)
predictions <- readRDS("Daten/predictions.rds")

raw_data <- readRDS("Daten/Daten_mit_Epoche.RDS")
raw_data <- raw_data[,!colnames(raw_data) %in% c("lng", "lat", "wkb_geom", "wkb_geom_wgs84")]
raw_data$lng_wgs84 <- as.numeric(as.vector(raw_data$lng_wgs84))
raw_data$lat_wgs84 <- as.numeric(as.vector(raw_data$lat_wgs84))
raw_data <- raw_data[order(raw_data$lng_wgs84, raw_data$lat_wgs84),]
raw_data <- dplyr::select(raw_data, -Landkreis, -DenkmalNummer, -Typ)
names(raw_data) <- c("Zeitstellung", "lng_wgs84", "lat_wgs84", "Hoehe", "Neigung")

predictors <- stack(c(
  "Daten/dem.grd",
  "Daten/temp_raster.grd",
  "Daten/rain_raster.grd",
  "Daten/water_raster.grd",
  "Daten/frostdays_raster.grd",
  "Daten/sunhours_raster.grd",
  "Daten/tpi_raster.grd",
  "Daten/slope_raster.grd",
  "Daten/aspect_raster.grd"))

# modified head function to display only first n rows AND n columns
head2d = function(x, n = 6L, ...) {
  indvecs = lapply(seq_along(dim(x)), function(i) {
    if (length(n) >= i) {
      ni = n[i]
    } else {
      ni =  dim(x)[i]
    }
    if (ni < 0L)
      ni = max(nrow(x) + ni, 0L)
    else
      ni = min(ni, dim(x)[i])
    seq_len(ni)
  })
  lstargs = c(list(x),indvecs, drop = FALSE)
  do.call("[", lstargs)
}

plot2water <- function(df, rows = 100, colorPoints = "red", colorArrows = "green", shp = shapewater, 
                       wid = 3) {
  if (rows > nrow(df)) {
    warning("n larger than nrow of data.frame. Setting n equal to nrow(data.frame).")
    rows <- nrow(df)
  }
  temp <- sp::SpatialPoints(
    coords = head(df[, c("lon", "lat")], n = rows),
    proj4string = wrld_subset@proj4string
  )
  plot(temp, col = colorPoints)
  plot(shp, add = TRUE, lwd = wid)
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

shapewater <- shapefile("Daten/DEU_water_lines_dcw.shx")
riverdists <- readRDS(file = "Daten/riverdists.rds")
data(wrld_simpl, package = "maptools")
wrld_subset <- wrld_simpl[wrld_simpl@data$ISO2 %in% c("DE"), ]

evidence <- readRDS("Daten/evidence.csv")
basefit <- glm(site ~ dem + temp + rain + distance_water + 
                 frostdays + sunhours + tpi + slope + 
                 as.factor(aspect), 
               family = binomial(), 
               data = evidence)
df <- as.data.frame(predictors)
df[c("x","y")] <- coordinates(predictors)
df <- df[complete.cases(df),]

pdata <- predict(basefit, newdata = df, type = "response")

#result1 <- readRDS("svm_sp_50it1.rds")

svmpred <- as.data.frame(values(predictors))
svmpred[c("x","y")] <- coordinates(predictors)
svm_df <- svmpred[complete.cases(svmpred),]
svm_df$aspect <- as.factor(svm_df$aspect)

datapred <- readRDS("ksvmpred.rds")
names(datapred) <- c("no", "yes")
svm_df$trueprob <- datapred[,2, drop = F]

