# applying geocomputation to iron age sites
source("packageloader.R")

# In this script i apply the methods of chapter 11 and 14 to a dataset I got from 
# Dr. Stephan Luecke & Dr. Caroline von Nicolai. The dataset can be thought of as
# "Open Data" since it is part of the dissertation of Peer Fender (2017)
# Link to the dissertation: 
# https://archiv.ub.uni-marburg.de/ubfind/Record/urn:nbn:de:hebis:04-z2017-0774/Holdings#tabnav
# Link to the Dataset: 
# https://pma.gwi.uni-muenchen.de:8888/sql.php?server=9&db=vfpa_eisenzeit&table=fender_2017
# Username: EZD_statistik; Password: statistik@EZD
# I exported the data from the database as a .csv and did minimal preprocessing which i did not save. 
# All i saved is the .RDS file. 

# Importing the Data and cleaning everything up
raw_data <- readRDS("Daten/Daten_mit_Epoche.RDS")
raw_data <- raw_data[,!colnames(raw_data) %in% c("lng", "lat", "wkb_geom", "wkb_geom_wgs84")]
raw_data$lng_wgs84 <- as.numeric(as.vector(raw_data$lng_wgs84))
raw_data$lat_wgs84 <- as.numeric(as.vector(raw_data$lat_wgs84))
raw_data <- raw_data[order(raw_data$lng_wgs84, raw_data$lat_wgs84),]

# Loading data from Downloaded raster files and through R's interface for loading
# Raster and vector data

ger_file <- raster::getData("GADM", country = "Germany", level = 1)
plot(ger_file)
bay_file <- ger_file[match(toupper("Bayern"),toupper(ger_file$NAME_1)),]
# border of bavaria => useful for masking
plot(bay_file)

# Generating random raster as a basis for masking     
bay_raster <- raster(xmn = 8.975, xmx = 13.84167, ymn = 47.26667, ymx = 50.56667, nrow = 396, ncol = 584)
bay_raster[] <- runif(396*584)
plot(bay_raster)
# masking
bay_mask <- mask(bay_raster, bay_file)
plot(bay_mask)

# Importing a DEM for germany through 'raster::getData'
dem <- raster::getData(name = "alt", country = "Germany")
names(dem) <- "dem"
# masking
dem <- crop(dem, bay_mask)
dem <- mask(dem, bay_mask)
plot(dem)
writeRaster(dem, "Daten/dem", overwrite = TRUE)

# Percipitation and average temperature
# these files are 72.3 MB large in total so it can take a while until the download is finished
weatherdata <- raster::getData("worldclim", var = "bio", res = 0.5, lon =  12.101624, lat = 49.013432)
# at the moment I just added average temperature and rain but variables like solar
# radiation might also be of interest later on
weatherdata <- weatherdata[[c(1, 12)]]
names(weatherdata) <- c("temp", "rain")

# masking and converting the temperature units from .1 of a degree to whole degrees celsius
weatherdata <- crop(weatherdata, bay_mask)
weatherdata <- mask(weatherdata, bay_mask)
weatherdata <- stack(weatherdata[[1]] / 10, weatherdata[[2]])
plot(weatherdata)

writeRaster(weatherdata[[1]], "Daten/temp_raster", overwrite = TRUE)
writeRaster(weatherdata[[2]], "Daten/rain_raster", overwrite = TRUE)

# calculating terrain => slope aspect and tpi
# dem <- predictors$dem
env_data <- terrain(dem, opt = c("slope", "aspect", "tpi"), unit = "degrees")
env_data$aspect <- ceiling((env_data$aspect + 360/8/2)/(360/8))
env_data$aspect[env_data$aspect > 8] <- 1
plot(env_data)
# Turn into Cathegorial Variable
# since for aspect the values for 360 and 0 degrees should be the same i convert to 8 
# categories => North North-East East South-East South...

env_data <- crop(env_data, bay_mask)
env_data <- mask(env_data, bay_mask)
plot(env_data)
writeRaster(env_data[[1]], "Daten/tpi_raster", overwrite = TRUE)
writeRaster(env_data[[2]], "Daten/slope_raster", overwrite = TRUE)
writeRaster(env_data[[3]], "Daten/aspect_raster", overwrite = TRUE)

# calculating distance to the nearest water. 
# source for shapefiles https://biogeo.ucdavis.edu/data/diva/wat/DEU_wat.zip
river_shape <- shapefile("Daten/DEU_water_lines_dcw.shx")
lake_shape <- shapefile("Daten/DEU_water_areas_dcw.shx")
# masking here takes ~ 1minute
river_raster <- mask(bay_raster, river_shape)
lake_raster <- mask(bay_raster, lake_shape)
# todo: add visualizations
# calculating distances
# since these functions calculate distance values for every single raster pixel
# they take a very long time (~1 hour on my computer) 
# only run these lines once, the result is available in Daten/distance_water.RData
# distance_river <- distance(river_raster)
# distance_lake <- distance(lake_raster)
# save(distance_river, distance_lake, file = "Daten/distance_water.RData")
load("Daten/distance_water.RData")

# i am interested in the minimum distance to bodies of water  
distance_water <- min(distance_lake, distance_river)
names(distance_water) <- "distance_water"
distance_water <- mask(distance_water, bay_mask)
plot(distance_water)
writeRaster(distance_water, "Daten/water_raster", overwrite = TRUE)
# should be easier to peer review 

# frostdays per year
# (Source: https://maps.dwd.de/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?0)
# https://maps.dwd.de/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?0
# download as .tif and place in Daten directory

frostdays <- raster("Daten/dwd-Frostdays_annual_map_normals_1971_30.tif")
names(frostdays) <- "frostdays"
# reprojecting
frostdays <- projectRaster(frostdays, crs = crs(bay_mask))
frostdays <- raster::resample(frostdays, bay_mask)
frostdays <- mask(frostdays, bay_mask)
writeRaster(frostdays, "Daten/frostdays_raster", overwrite = TRUE)

# average sunhours per day per year
# (Source: https://maps.dwd.de/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?0)
# https://maps.dwd.de/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?0

sunhours <- raster("Daten/dwd-SDMS_17_1971_30.tif")
names(sunhours) <- "sunhours"
# reprojecting
sunhours <- projectRaster(sunhours, crs = crs(bay_mask))
sunhours <- raster::resample(sunhours, bay_mask)
sunhours <- mask(sunhours, bay_mask)

writeRaster(sunhours, "Daten/sunhours_raster", overwrite = TRUE)

# stacking all the rasters in a single predictorstack

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


# extract data on sites
# step one getting the coordinates of the sites
# I'm only interested in settlements from the iron age
raster::unique(raw_data$Epoche)
presence <- raw_data[raw_data$Epoche %in% c("Hallstattzeit", "Latènezeit"),]
nrow(presence)
sites <- dplyr::select(presence, lng_wgs84, lat_wgs84)
head(sites)
# selecting unique rows to combat biased models
sites <- unique(sites[,c("lng_wgs84", "lat_wgs84")])
# 6206 sites remaining
# creating buffers around points to sample nonsites correctly
point_buff_100m <- st_buffer(random_points, dist = 100)
plot(point_buff_100m)
# selecting those points that are within the buffer zone
random_points[point_buff_100m,]
# lets try it out with random samples from bavaria and a buffer around our sites
temp <- sampleRandom(predictors[[1]], 1000, sp = T)
sp_sites <- sp::SpatialPoints(coords      = sites[,c("lng_wgs84","lat_wgs84")], # order matters
                              proj4string = predictors@crs)
utm_sites <- spTransform(sp_sites, CRS("+proj=utm +zone=32 ellps=WGS84"))
sf_sites <- st_as_sf(utm_sites)

sites_buff_1000m <- st_buffer(sf_sites, dist = 1000)
temp_utm <- spTransform(temp, CRS("+proj=utm +zone=32 ellps=WGS84"))

tmp <- temp_utm[sites_buff_1000m$geometry,]

sp_polygons_buffer <- sf::as_Spatial(sites_buff_1000m$geometry)
crs(sp_polygons_buffer) <- crs(temp_utm)
over(temp_utm, sp_polygons_buffer)
nrow(temp_utm)
ret <- temp_utm[!is.na(over(temp_utm, sp_polygons_buffer)),]
# this returns the points that fall within the buffer zone. 
# lets try to increase the radius
sites_buff_2500m <- st_buffer(sf_sites, dist = 2500)
sp_polygons_2500 <- sf::as_Spatial(sites_buff_2500m$geometry)
crs(sp_polygons_2500) <- crs(temp_utm)
ret2500_within <- temp_utm[!is.na(over(temp_utm, sp_polygons_2500)),]
nrow(ret2500_within)
ret2500_within <- spTransform(ret2500_within, crs(temp))
ret2500_without <- temp_utm[is.na(over(temp_utm, sp_polygons_2500)),]
ret2500_without <- spTransform(ret2500_without, crs(temp))

# mapview(sp_sites) + 
#   mapview(ret2500_without, color = "red")
# # works as expected 

# function to sample around the points with a given buffer
buffsample <- function(ssize = 1000, distance = 1000, within = FALSE, returnsize = 1000) {
  ret <- new("SpatialPoints",                                                          
                     coords = structure(numeric(0), .Dim = c(0L, 2L),                          
                                        .Dimnames = list(NULL, c("coords.x1", "coords.x2"))),  
                     bbox = structure(c(1, 1, 1, 1), .Dim = c(2L, 2L),                         
                                      .Dimnames = list(c("coords.x1", "coords.x2"),
                                                       c("min", "max"))),
                     proj4string = new("CRS", projargs = "+proj=utm +zone=32 ellps=WGS84 +ellps=WGS84"))
  sp_sites <- sp::SpatialPoints(coords = sites[,c("lng_wgs84","lat_wgs84")], proj4string = predictors@crs)
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
set.seed(123)
funtest <- buffsample(ssize = 2000, distance = 1500, returnsize = 10000)
funtest5k <- buffsample(ssize = 2000, distance = 1500, returnsize = 5000)
sites_buff_1500m <- st_buffer(sf_sites, dist = 1500)
nrow(funtest)

coordinates(funtest5k) <- c("lng_wgs84", "lat_wgs84")
mapview(sites_buff_1500m) + 
  mapview(funtest5k, alpha.regions = 0.1, color = "red")
# works as intended and performs reasonably well
# finalizing evidence

# function to automate generation of evidence data
generateEvidence <- function(sitesdata, nonsitesdata, predictorstack = predictors) {
  # selecting site points
  sites_temp <- sitesdata
  sites_temp$lon <- as.numeric(as.vector(sites_temp$lng_wgs84))
  sites_temp$lat <- as.numeric(as.vector(sites_temp$lat_wgs84))
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

evidence <- generateEvidence(sitesdata = sites, nonsitesdata = funtest, predictorstack = predictors)
evidence <- finalizeEvidence(evidence)

saveRDS(evidence, file = "Daten/evidence.csv")
evidence <- readRDS("Daten/evidence.csv")
# fitting a generalized linear model to use as baseline +
names(evidence)
basefit <- glm(site ~ dem + temp + rain + distance_water + frostdays + sunhours + 
                 tpi + slope + as.factor(aspect), 
                 family = binomial(), 
                 data = evidence)
basesummary <- summary(basefit)
basesummary

# predictive mapping 
df <- as.data.frame(predictors)
df[c("x","y")] <- coordinates(predictors)
# complete cases causes bug here 
# df <- df[complete.cases(df),]

pdata <- predict(basefit, newdata = df, type = "response")
df$pdata <- pdata

x_pred <- predictors
x_pred$pred <- pdata

# predictive plot for glm
plot(x_pred$pred)

ggplot(data = df, aes(x,y, fill = pdata)) + 
  geom_tile() + 
  ggtitle("Predictive Map: Logistic Regression") +
  scale_fill_gradientn(colours = rev(colorRamps::matlab.like(100)), name = "") +
  coord_fixed() +
  ylab("Latitude") +
  xlab("Longitude") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))

# spatially repeated CV to assess predictive performance
coords = evidence[, c("lon", "lat")]
# select response and predictors to use in the modeling
data = dplyr::select(evidence, -lon, -lat)
# create task
data$site <- as.logical(data$site)
task = makeClassifTask(data = data, target = "site",
                       positive = "TRUE", coordinates = coords)

# specify a learner through
lrn = makeLearner(cl = "classif.binomial",
                  link = "logit",
                  predict.type = "prob",
                  fix.factors.prediction = TRUE)
# specifying a resampling method (Spacial repeated Crossvalidation)
perf_level = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 100)
# running the resampling to assess model performance
set.seed(37)
sp_cv = mlr::resample(learner = lrn, task = task,
                      resampling = perf_level, 
                      measures = mlr::auc)
boxplot(sp_cv$measures.test$auc)

perf_level_std <- makeResampleDesc(method = "RepCV", folds = 5, reps = 5)
std_cv <- mlr::resample(learner = lrn, task = task,
                        resampling = perf_level_std,
                        measures = mlr::auc)
# visualization of resampling methods and results
boxplot(std_cv$measures.test$auc, sp_cv$measures.test$auc)

library(hrbrthemes)
# resample visualization 
# takes a long time
resplots <- createSpatialResamplingPlots(task = task, resample = std_cv, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", repetitions = 1,
                                         x.axis.breaks = c(8.5, 14.5),
                                         y.axis.breaks = c(47, 51))
resplots[[1]][[1]]

library(mlr)
rdesc = makeResampleDesc("SpRepCV", folds = 5, reps = 4)
resamp = resample(makeLearner("classif.binomial"), task = task, rdesc)
##
plots = createSpatialResamplingPlots(task = task, resamp, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                     repetitions = 2, x.axis.breaks = c(8.5, 14.5),
                                     y.axis.breaks = c(47, 51))
plots[[1]][[3]]

# classic cv 
rdesc = makeResampleDesc("RepCV", folds = 5, reps = 4)
resamp = resample(makeLearner("classif.binomial"), task = task, rdesc)
##
plots.classic = createSpatialResamplingPlots(task = task, resamp, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                             repetitions = 2, x.axis.breaks = c(8.5, 14.5),
                                             y.axis.breaks = c(47, 51))
plots.classic[[1]][[3]]
plots.classic[[1]][[1]]
# plotting them all in a grid: 
require("cowplot")
cowplot::plot_grid(plotlist = plots[["Plots"]], ncol = 3, nrow = 1,
                   labels = plots[["Labels"]])

# trying out the support vector machine
# hyperparameter tuning
library("kernlab")
lrn_ksvm = makeLearner("classif.ksvm",
                       predict.type = "prob",
                       kernel = "rbfdot")
perf_level = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 100)
tune_level = makeResampleDesc("SpCV", iters = 4)
# use 50 randomly selected hyperparameters
ctrl = makeTuneControlRandom(maxit = 50)
# define the outer limits of the randomly selected hyperparameters
ps = makeParamSet(
  makeNumericParam("C", lower = -12, upper = 15, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -15, upper = 6, trafo = function(x) 2^x)
)
wrapped_lrn_ksvm = makeTuneWrapper(learner = lrn_ksvm, 
                                   resampling = tune_level,
                                   par.set = ps,
                                   control = ctrl, 
                                   show.info = TRUE,
                                   measures = mlr::auc)

# setting option to keep modeling even if models fail
# and dump failed models
configureMlr(on.learner.error = "warn", on.error.dump = TRUE)
# parallelization under windows
library(parallelMap)
if (Sys.info()["sysname"] == "Windows") {
  parallelStartSocket(level = "mlr.tuneParams", cpus = parallel::detectCores() - 1)
}

if (Sys.info()["sysname"] %in% c("Linux", "Darwin")) {
  parallelStart(mode = "multicore",
                # parallelize the hyperparameter tuning level
                level = "mlr.tuneParams",
                # just use half of the available cores
                cpus = round(parallel::detectCores() - 1),
                # mc.set.seed is only supported on unix systems
                mc.set.seed = TRUE)
}
# this is very time intensive (~16 hours non parallel)
set.seed(12345)
result = mlr::resample(learner = wrapped_lrn_ksvm,
                       task = task,
                       resampling = perf_level,
                       extract = getTuneResult,
                       measures = mlr::auc)
# rerunning tuning with different seed to append tuninglog.txt
set.seed(88888)
result2 = mlr::resample(learner = wrapped_lrn_ksvm,
                       task = task,
                       resampling = perf_level,
                       extract = getTuneResult,
                       measures = mlr::auc)
# threw an error because the last iteration did not run for some reason. 
# Log: 
# Warning in train(learner, task, subset = train.i, weights = weights[train.i]) :
# Could not train learner classif.ksvm.tuned: Error in res.list[[i]] : subscript out of bounds
# Error: $ operator is invalid for atomic vectors
# In addition: Warning messages:
# 1: Quick-TRANSfer stage steps exceeded maximum (= 611100) 
# 2: Quick-TRANSfer stage steps exceeded maximum (= 611100) 
# 3: In mclapply(seq_len(n), do_one, mc.preschedule = mc.preschedule,  :
# scheduled core 4 did not deliver a result, all values of the job will be affected

# it seems my computer ran out of memory in the last cycle. (16GB RAM, 2GB Swap)
# running another 2x3 iterations to do the trick
# the first 4 iterations returned the following parameters:

# [Tune] Result: C=0.0448; sigma=0.00036 : auc.test.mean=0.7292244
# [Tune] Result: C=0.452; sigma=0.00244 : auc.test.mean=0.7711429
# [Tune] Result: C=0.0249; sigma=0.000542 : auc.test.mean=0.7584536
# [Tune] Result: C=0.0135; sigma=0.000909 : auc.test.mean=0.7897935

set.seed(123456)
# [Tune] Result: C=0.0421; sigma=5.32e-05 : auc.test.mean=0.7883878
# [Tune] Result: C=0.293; sigma=3.74e-05 : auc.test.mean=0.7406062
# [Tune] Result: C=0.0802; sigma=8.83e-05 : auc.test.mean=0.7971265
# [Tune] Result: C=0.0478; sigma=0.00147 : auc.test.mean=0.7423675
# [Tune] Result: C=0.00682; sigma=3.1e-05 : auc.test.mean=0.7717502
# [Tune] Result: C=0.0187; sigma=4.16e-05 : auc.test.mean=0.7862915
# reducing the amount of iterations still crashed after 52 steps 
# there seems to be an issue with the way forking works on linux. Some of the
# iterations used up to 16 GB of ram with an additional 16 GB of swap. 
# It crashed on iteration step 52 
# 4: In mclapply(seq_len(n), do_one, mc.preschedule = mc.preschedule,  :
# scheduled core 1 did not deliver a result, all values of the job will be affected
# I scraped the logs of the completed tries for the hyperparameter values
# Trying to create a result object that can be used for peer review:
# (also using the previously computed hyperparameter values as new boundaries
# for the parameter space)

configureMlr(on.learner.error = "warn", on.error.dump = TRUE)
lrn_ksvm = makeLearner("classif.ksvm",
                       predict.type = "prob",
                       kernel = "rbfdot")
perf_level = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 2)
tune_level = makeResampleDesc("SpCV", iters = 2)
ctrl = makeTuneControlRandom(maxit = 2)
ps = makeParamSet(
  makeNumericParam("C", lower = -1, upper = 1, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -1, upper = 1, trafo = function(x) 2^x)
)
wrapped_lrn_ksvm = makeTuneWrapper(learner = lrn_ksvm, 
                                   resampling = tune_level,
                                   par.set = ps,
                                   control = ctrl, 
                                   show.info = TRUE,
                                   measures = mlr::auc)
if (Sys.info()["sysname"] %in% c("Linux", "Darwin")) {
  parallelStart(mode = "multicore", 
                # parallelize the hyperparameter tuning level
                level = "mlr.tuneParams", 
                # just use half of the available cores
                cpus = round(parallel::detectCores() / 2),
                mc.set.seed = TRUE)
}
result1 = mlr::resample(learner = wrapped_lrn_ksvm, 
                       task = task,
                       resampling = perf_level,
                       extract = getTuneResult,
                       measures = mlr::auc)
# iter 8
# [Tune] Result: C=0.0334; sigma=0.0205 : auc.test.mean=0.7986833
# iter 19
# [Tune] Result: C=0.0711; sigma=0.000688 : auc.test.mean=0.8035407
saveRDS(result1, "svm_sp_50it1.rds")
result1 <- readRDS("svm_sp_50it1.rds")
# assigning the last entry in the list the winning hyperparameters from previous
# failed runs
result1$extract[[10]]$x$C <- 0.0711
result1$extract[[10]]$x$sigma <- 0.000688
result1$extract[[10]]$y[[1]] <- 0.8035407
saveRDS(result1, "svm_sp_50it1.rds")
result1 <- readRDS("svm_sp_50it1.rds")
# calling garbage collector to free up ram 
gc()

parallelStop()

task = makeClassifTask(data = data, target = "site",
                       positive = "TRUE", coordinates = coords)
lrn_ksvm = makeLearner("classif.ksvm",
                       predict.type = "prob",
                       kernel = "rbfdot")
# setting hyperparameters to the optimal values I found
lrn_ksvm <- setHyperPars(lrn_ksvm, sigma = result1$extract[[10]]$x$sigma, 
                   par.vals = list(C = result1$extract[[10]]$x$C))
model_svm = train(lrn_ksvm, task)

# # convert raster stack into a data frame
# new_data <- as.data.frame(predictors)
# new_raster <- predictors
# new_matrix <- as.matrix(predictors)
# # apply the model to the data frame
# kern <- as.kernelMatrix(new_matrix)
# pred_svm <- raster::predict(object = new_raster, model = model_svm, type = "response")
# # fails 
# pred_svm <- predict(model_svm, newdata = new_data)
# # put the predicted values into a raster
# x_pred$svm_pred <- pred_svm
# # also fails

test <- as.data.frame(values(predictors))
test[c("x","y")] <- coordinates(predictors)
test_df <- test[complete.cases(test),] 

#### predict to this new data
pred <- predict(model_svm, newdata = test_df, type = "probabilities")
test_df$svm.fit <- pred$data$response

###map the predictions
ggplot(data = test_df, aes(x,y,fill = svm.fit)) + 
  geom_tile() + 
  #scale_fill_gradientn(colours = rev(colorRamps::matlab.like(100))) +
  coord_fixed()

C <- result1$extract[[10]]$x$C
sigma <- result1$extract[[10]]$x$sigma
data$aspect <- as.factor(data$aspect)
# something is wrong with aspect gotta check later
data <- subset(data, select = -c(aspect))
kmodel <- ksvm(site~.,data = data, kernel = "rbfdot", type = "C-svc",
             kpar = list(sigma = sigma), C = C, cross = 5, prob.model = TRUE)
test <- as.data.frame(values(predictors))
test[c("x","y")] <- coordinates(predictors)
test_df <- test[complete.cases(test),]
test_df$aspect <- as.factor(test_df$aspect)
datapred <- predict(kmodel, newdata = test_df, type = "probabilities")
saveRDS(datapred, file = "ksvmpred.rds")
names(datapred) <- c("no", "yes")
test_df$trueprob <- datapred[,2, drop = F]
ggplot(data = test_df, aes(x,y,fill = trueprob)) + 
  geom_tile() + 
  ggtitle("KSVM Predictions") +
  scale_fill_gradientn(colours = rev(colorRamps::matlab.like(100))) +
  coord_fixed()

# merging the predicted data from logistic regression and the svm for later shenanigans
predictions <- df
nam <- names(predictions)
nam[12] <- "lgprd"
names(predictions) <- nam
predictions <- predictions[complete.cases(predictions),]
predictions$svmpd <- test_df$trueprob
# calculating cell differences
predictions$lg_svm_diff <- abs(predictions$lgprd - predictions$svmpd)
# plotting cellwise differences
ggplot(data = predictions, aes(x,y,fill = lg_svm_diff)) + 
  geom_tile() + 
  ggtitle("Absolute Cell Differences: Logit vs SVM") +
  scale_fill_gradientn(colours = rev(colorRamps::matlab.like(100))) +
  coord_fixed()

mean(predictions$svmpd)
mean(predictions$lgprd)
mean(predictions$lg_svm_diff)
# predictive plot for svm
# => convert back to sf data for tmap

# trying random forest
coords = evidence[, c("lon", "lat")]
evidence$aspect <- as.factor(evidence$aspect)
# only keep response and predictors which should be used for the modeling
evd = dplyr::select(evidence, -lon, -lat)
# basic decision tree
library("tree")
tree1 = tree(site ~ distance_water + dem, data = evd)
plot(tree1)
text(tree1, pretty = 1)

# create task
# evd$site <- as.factor(as.logical(evd$site))
task_rf = makeRegrTask(data = evd, target = "site", coordinates = coords)
# learner
lrn_rf = makeLearner(cl = "regr.ranger", predict.type = "response")
# learner
library(randomForest)
library(mlr)
# lrn_rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
# spatial partitioning
perf_level_rf = makeResampleDesc("SpCV", iters = 5)
# specifying random search
ctrl_rf = makeTuneControlRandom(maxit = 50L)
# specifying the search space for hyperparameter tuning

ps = makeParamSet(
  makeIntegerParam("mtry", lower = 1, upper = ncol(evd) - 1),
  makeNumericParam("sample.fraction", lower = 0.2, upper = 0.9),
  makeIntegerParam("min.node.size", lower = 1, upper = 10)
)
# hyperparamter tuning (~20 minutes)
set.seed(123)
tune = tuneParams(learner = lrn_rf, 
                  task = task_rf,
                  resampling = perf_level_rf,
                  par.set = ps,
                  control = ctrl_rf, 
                  measures = mlr::rmse)
saveRDS(tune, file = "rftune1.rds")
tune <- readRDS(file = "rftune1.rds")
# mtry=1; sample.fraction=0.309; min.node.size=10 rmse.test.rmse=0.4660128
# getting optimal sample size 
ssize <- floor(tune$x$sample.fraction * nrow(evd))

# training the model with the best hyperparameter 
  lrn_rf = makeLearner(cl = "regr.ranger", 
                       predict.type = "response", 
                       fix.factors.prediction = TRUE,
                       mtry = tune$x$mtry, 
                       sample.fraction = tune$x$sample.fraction,
                       min.node.size = tune$x$min.node.size,
                       importance  = c("permutation")
                       )
  evd$site <- as.numeric(evd$site)
  task_rf = makeRegrTask(data = evd, target = "site", coordinates = coords)
  model_rf = train(lrn_rf, task_rf)

# convert raster stack into a data frame
new_data = as.data.frame(as.matrix(predictors))
new_data <- new_data[complete.cases(new_data),]
# apply the model to the data frame
pred_rf = predict(model_rf, newdata = new_data)
# currently predicts in interval [1, 2] lets bring it down by one to make it 
# compareable to the other two models 
tmp <- pred_rf$data$response #- 1
predictions$rfpred <- tmp

# calculating cell differences
predictions$lg_rf_diff <- abs(predictions$lgprd - predictions$rfpred)
# plotting cellwise differences
ggplot(data = predictions, aes(x,y,fill = rfpred)) + 
  geom_tile() + 
  ggtitle("Predictive Map: Random Forest") +
  scale_fill_gradientn(colours = rev(colorRamps::matlab.like(100))) +
  coord_fixed()

ggplot(data = predictions, aes(x,y,fill = lg_rf_diff)) + 
  geom_tile() + 
  ggtitle("Absolute Cell Differences: Logit vs. RF") +
  scale_fill_gradientn(colours = rev(colorRamps::matlab.like(100))) +
  coord_fixed()

mean(predictions$svmpd)
mean(predictions$lgprd)
mean(predictions$rfpred)
mean(predictions$lg_svm_diff)
mean(predictions$lg_rf_diff)

impo <- getFeatureImportance(model_rf)
vect <- as.vector(impo$res)
vect <- vect[1,, drop = T]
vect <- unlist(vect)
resc <- scales::rescale(vect, to = c(0,1))

# saving prediction results in Data
write.csv(predictions, file = "Daten/predictions.csv")
saveRDS(predictions, file = "Daten/predictions.rds")

# comparing model performance
# spatially repeated CV to assess predictive performance
coords = evidence[, c("lon", "lat")]
# select response and predictors to use in the modeling
data = dplyr::select(evidence, -lon, -lat)
# create task
data$site <- as.logical(data$site)
data$aspect <- as.factor(data$aspect)
task = makeClassifTask(data = data, target = "site",
                       positive = "TRUE", coordinates = coords)

# specify a learner through
lrn = makeLearner(cl = "classif.binomial",
                  link = "logit",
                  predict.type = "prob",
                  fix.factors.prediction = TRUE)
# specifying a resampling method (Spacial repeated Crossvalidation)
perf_level = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 100)
# running the resampling to assess model performance
set.seed(37)
sp_cv = mlr::resample(learner = lrn, task = task,
                      resampling = perf_level, 
                      measures = mlr::auc)
boxplot(sp_cv$measures.test$auc)

perf_level_std <- makeResampleDesc(method = "RepCV", folds = 5, reps = 100)
std_cv <- mlr::resample(learner = lrn, task = task,
                        resampling = perf_level_std,
                        measures = mlr::auc)
# visualization of resampling methods and results
boxplot(std_cv$measures.test$auc, sp_cv$measures.test$auc)

# doing the same for svm
set.seed(37)
configureMlr(on.learner.error = "warn", on.error.dump = TRUE)
task = makeClassifTask(data = data, target = "site",
                       positive = "TRUE", coordinates = coords)
lrn_ksvm = makeLearner("classif.ksvm",
                       predict.type = "prob",
                       kernel = "rbfdot")
# setting hyperparameters to the optimal values I found
lrn_ksvm <- setHyperPars(lrn_ksvm, sigma = sigma, 
                         par.vals = list(C = C))
sp_cv_svm = mlr::resample(learner = lrn_ksvm, task = task,
                      resampling = perf_level, 
                      measures = mlr::auc)
boxplot(sp_cv$measures.test$auc)

perf_level_std <- makeResampleDesc(method = "RepCV", folds = 5, reps = 100)
std_cv_svm <- mlr::resample(learner = lrn_ksvm, task = task,
                        resampling = perf_level_std,
                        measures = mlr::auc)

boxplot(std_cv_svm$measures.test$auc, sp_cv$measures.test$auc)

# resampling random forest
tune <- readRDS(file = "rftune1.rds")
ssize <- floor(tune$x$sample.fraction * nrow(evd))
# training the model with the best hyperparameter 
lrn_rf = makeLearner(cl = "classif.randomForest", 
                     predict.type = "prob", 
                     #fix.factors.prediction = TRUE,
                     mtry = tune$x$mtry, 
                     sampsize = ssize,
                     nodesize = tune$x$min.node.size,
)
evd$site <- as.factor(evd$site)
task_rf = makeClassifTask(data = evd, target = "site", coordinates = coords)
perf_level_sp_rf = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 100)
sp_cv_rf = mlr::resample(learner = lrn_rf, task = task_rf,
                          resampling = perf_level_sp_rf, 
                          measures = mlr::auc)

perf_level_std_rf <- makeResampleDesc(method = "RepCV", folds = 5, reps = 100)
std_cv_rf <- mlr::resample(learner = lrn, task = task,
                            resampling = perf_level_std_rf,
                            measures = mlr::auc)

resamplist <- list(logitsp = sp_cv, logitstd = std_cv, svmsp = sp_cv_svm,
                   svmstd = std_cv_svm, rfsp = sp_cv_rf, rfstd = std_cv_rf)
boxplot(std_cv_rf$measures.test$auc, sp_cv_rf$measures.test$auc)
saveRDS(resamplist, file = "Daten/resamplelist")

resamplist <- readRDS("Daten/resamplelist")
logitsp <- resamplist$logitsp$measures.test$auc
logitstd <- resamplist$logitstd$measures.test$auc
svmsp <- resamplist$svmsp$measures.test$auc
svmstd <- resamplist$svmstd$measures.test$auc
rfsp <- resamplist$rfsp$measures.test$auc
rfstd <- resamplist$rfstd$measures.test$auc

performanceframe <- data.frame(LogitSP = logitsp, LogitSTD = logitstd, SVMSP = svmsp, 
                               SVMSTD = svmstd, RFSP = rfsp, RFSTD = rfstd)

df.m <- reshape2::melt(performanceframe, value.name = "Label")
ggplot(data = df.m, aes(x=variable, y=Label)) + 
  geom_boxplot(aes(fill=variable)) + 
  ylab("Area under ROC") +
  xlab("Resampling Method") +
  theme(legend.position = "none", axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=14,face="bold")) 

# Adding visualizations for final report
png(filename="predictorstack.png",
    width = 800, height = 550)
plot(predictors)
dev.off()

# logitmap
ggplot(data = df, aes(x,y, fill = pdata)) + 
  geom_tile() + 
  ggtitle("Predictive Map: Logistic Regression") +
  scale_fill_gradientn(colours = rev(colorRamps::matlab.like(100)), name = "") +
  coord_fixed() +
  ylab("Latitude") +
  xlab("Longitude") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
# svmmap
ggplot(data = test_df, aes(x,y,fill = trueprob)) + 
  geom_tile() + 
  ggtitle("Predictive Map: SVM") +
  scale_fill_gradientn(colours = rev(colorRamps::matlab.like(100)), name = "") +
  coord_fixed() +
  ylab("Latitude") +
  xlab("Longitude") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
# rfmap
ggplot(data = predictions, aes(x,y,fill = rfpred)) + 
  geom_tile() + 
  ggtitle("Predictive Map: Random Forest") +
  scale_fill_gradientn(colours = rev(colorRamps::matlab.like(100)), name = "") +
  coord_fixed() +
  ylab("Latitude") +
  xlab("Longitude") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))

#### cell difference maps
# calculating cell differences
predictions$lg_svm_diff <- abs(predictions$lgprd - predictions$svmpd)
predictions$lg_rf_diff <- abs(predictions$lgprd - predictions$rfpred)
predictions$svm_rf_diff <- abs(predictions$svmpd - predictions$rfpred)
# plotting cellwise differences
# logit vs svm
ggplot(data = predictions, aes(x,y,fill = lg_svm_diff)) + 
  geom_tile() + 
  ggtitle("Absolute Cell Differences: Logit vs SVM") +
  scale_fill_gradientn(colours = rev(colorRamps::matlab.like(100)), name = "") +
  coord_fixed() +
  ylab("Latitude") +
  xlab("Longitude") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
# logit vs rf
ggplot(data = predictions, aes(x,y,fill = lg_rf_diff)) + 
  geom_tile() + 
  ggtitle("Absolute Cell Differences: Logit vs. RF") +
  scale_fill_gradientn(colours = rev(colorRamps::matlab.like(100)), name = "") +
  coord_fixed() +
  ylab("Latitude") +
  xlab("Longitude") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
# svm vs rf
ggplot(data = predictions, aes(x,y,fill = svm_rf_diff)) + 
  geom_tile() + 
  ggtitle("Absolute Cell Differences: SVM vs. RF") +
  scale_fill_gradientn(colours = rev(colorRamps::matlab.like(100)), name = "") +
  coord_fixed() +
  ylab("Latitude") +
  xlab("Longitude") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))

