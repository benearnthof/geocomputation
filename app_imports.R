# app imports
source("packageloader.R")
sites <- readRDS("Daten/evidence.csv") %>% dplyr::filter(site == 1) %>% 
  dplyr::select(lon, lat)
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
# sampling with buffer
buffsample <- function(ssize = 2000, distance = 1000, within = FALSE, returnsize = 1000) {
  ret <- new("SpatialPoints",                                                          
             coords = structure(numeric(0), .Dim = c(0L, 2L),                          
                                .Dimnames = list(NULL, c("coords.x1", "coords.x2"))),  
             bbox = structure(c(1, 1, 1, 1), .Dim = c(2L, 2L),                         
                              .Dimnames = list(c("coords.x1", "coords.x2"),
                                               c("min", "max"))),
             proj4string = new("CRS", projargs = "+proj=utm +zone=32 ellps=WGS84 +ellps=WGS84"))
  sp_sites <- sp::SpatialPoints(coords = sites[,c("lon","lat")], proj4string = predictors@crs)
  utm_sites <- spTransform(sp_sites, CRS("+proj=utm +zone=32 ellps=WGS84"))
  sf_sites <- st_as_sf(utm_sites)
  sites_buff <- st_buffer(sf_sites, dist = distance)
  
  sp_polygons_buffer <- sf::as_Spatial(sites_buff$geometry)
  while (nrow(ret@coords) < returnsize) {
    sample <- sampleRandom(predictors[[1]], ssize, sp = T)
    sample_utm <- spTransform(sample, CRS("+proj=utm +zone=32 ellps=WGS84"))
    crs(sp_polygons_buffer) <- crs(sample_utm)
    over(sample_utm, sp_polygons_buffer)
    nrow(sample_utm)
    if (within == TRUE) {
      tmp <- sample_utm[!is.na(over(sample_utm, sp_polygons_buffer)),]
    } else {
      tmp <- sample_utm[is.na(over(sample_utm, sp_polygons_buffer)),]
    }
    ret <- maptools::spRbind(ret, tmp)
  }
  ret <- spTransform(ret, crs(sp_sites))
  ret <- ret[1:returnsize,]
  ret <- coordinates(ret)
  colnames(ret) <- c("lng_wgs84", "lat_wgs84")
  ret <- as.data.frame.matrix(ret)
  return(ret)
}

generateEvidenceApp <- function(sitesdata, nonsitesdata, predictorstack = predictors) {
  # selecting site points
  sites_temp <- sitesdata
  sites_temp$lon <- as.numeric(as.vector(sites_temp$lon))
  sites_temp$lat <- as.numeric(as.vector(sites_temp$lat))
  # convert to spatial data in order to extract the predictor values for all points
  coordinates(sites_temp) <- c("lon","lat")
  proj4string(sites_temp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  nonsites_temp <- nonsitesdata
  nonsites_temp$lon <- as.numeric(as.vector(nonsites_temp$lng_wgs84))
  nonsites_temp$lat <- as.numeric(as.vector(nonsites_temp$lat_wgs84))
  coordinates(nonsites_temp) <- c("lon", "lat")
  proj4string(nonsites_temp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  # extracting predictor values for sites and nonsites
  sSP <- SpatialPoints(sites_temp@coords)
  nsSP <- SpatialPoints(nonsites_temp@coords)
  values_sites <- raster::extract(predictorstack, sSP)
  values_nonsites <- raster::extract(predictorstack, nsSP)
  # converting back to data.frame for modeling
  coords_sites <- sites_temp@coords
  coords_sites <- as.data.frame(coords_sites)
  coords_nonsites <- nonsites_temp@coords
  coords_nonsites <- as.data.frame(coords_nonsites)
  values_sites <- as.data.frame(values_sites)
  values_nonsites <- as.data.frame(values_nonsites)
  values_sites$site <- 1
  values_nonsites$site <- 0
  values_sites$lon <- coords_sites$lon
  values_sites$lat <- coords_sites$lat
  values_nonsites$lon <- coords_nonsites$lon
  values_nonsites$lat <- coords_nonsites$lat
  evidence <- rbind(values_sites, values_nonsites)
  evidence <- na.omit(evidence)
  return(evidence)
}

# function to draw sets of equal size
finalizeEvidence <- function(evd){
  siedl_pts <- filter(evd, site == 1)
  nons_pts <- filter(evd, site == 0)
  sz <- nrow(siedl_pts)
  nons_pts_sub <- sample_n(nons_pts, size = sz)
  temp <- rbind(siedl_pts, nons_pts_sub)
  return(temp)
}
