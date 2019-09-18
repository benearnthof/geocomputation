# reproducing chapter 14
# importing data
source(file = "packageloader.R")
data("study_area", "random_points", "comm", "dem", "ndvi", package = "RQGIS")

mapview(ndvi)
head(comm)
mapview(dem)
mapview(random_points)
mapview(study_area)

plot(ndvi)
plot(st_geometry(study_area), add = TRUE)
plot(st_geometry(random_points), add = TRUE)

dem
vignette("functions", package = "raster")
r <- raster(ncol = 10, nrow = 10)
r[] <- 1:100
r
plot(r)
plot(r + 100)

f2 <- function(x, a) {
  v <- raster::getValues(x)
  v <- v + a
  x <- raster::setValues(x, v)
  return(x)
}

s <- f2(r, 5)
s@data@values

raster::getValues(r, 1)
blockSize(r)

#### 2 Geographic Data in R ====
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
new_raster = raster(raster_filepath)
mapview(new_raster)
res(new_raster)
dim(new_raster)
plot(new_raster)
spplot(new_raster)
rasterVis::levelplot(new_raster)

new_raster2 = raster(nrows = 6, ncols = 6, res = 0.5, 
                     xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
                     vals = 1:36)
plot(new_raster2)
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
r_brick = brick(multi_raster_file)
plot(r_brick)
r_brick

# crs shenanigans

crs_data = rgdal::make_EPSG()
View(crs_data)
world
world_df <- st_drop_geometry(world)

test <- world_df[,1, drop = FALSE]
class(test)

#### 3 Attribute Data Operations ====
world_agg1 = aggregate(pop ~ continent, FUN = sum, data = world, na.rm = TRUE)
class(world_agg1)

world_agg2 = aggregate(world["pop"], by = list(world$continent),
                       FUN = sum, na.rm = TRUE)
class(world_agg2)

world_agg3 = world %>%
  group_by(continent) %>%
  summarize(pop = sum(pop, na.rm = TRUE))

# vector attribute joining
# full list of joining operations for two tables
vignette("two-table") 

# left join preserves the first dataset 
world_coffee = left_join(world, coffee_data)
# inner join preserves only countries that have a match in the key variable
world_coffee_inner = inner_join(world, coffee_data)
plot(world_coffee_inner[12])

# joining the other way around
coffee_world = left_join(coffee_data, world)
head(coffee_world)
coffee_world %>% group_by(continent) %>% summarise()
st_as_sf(coffee_world)

# creating attributes and removing spatial information
# classic dplyr shenanigans => mutate transmute unite separate
# unite and separate might prove useful later on 
# they serve to unite two columns into one or do the reverse

# remove geometry of a simple features object with 
head(st_drop_geometry(world_coffee_inner))

# manipulating raster objects
elev <-  raster(nrows = 100, ncols = 100, 
              xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
              vals = 1:10000)
plot(elev)
x <- seq(from = 0, to = 1, length.out = 100)
y <- seq(from = 0, to = 1, length.out = 100)

z <- matrix(nrow = 100, ncol = 100)

for (i in 1:100) {
  for (j in 1:100) {
    z[i,j] <- sqrt((x[i])^2 + (y[j])^2)
  }
}

elev2 <- raster(nrows = 100, ncols = 100, 
                xmn = -1, xmx = 1, ymn = -1, ymx = 1,
                vals = z)
plot(elev2)

# grainraster 
grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = raster(nrows = 6, ncols = 6, res = 0.5, 
               xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
               vals = grain_fact)
plot(grain)
levels(grain)[[1]] = cbind(levels(grain)[[1]], wetness = c("wet", "moist", "dry"))
levels(grain)
plot(grain)
factorValues(grain, grain[c(1, 11, 35)])
plot(grain)

# raster subsetting
# base subsetting operator accepts row-column indexing, cell IDs, coordinates, and another raster object

elev[1]
elev[1,1]
values(elev)
test <- values(elev2)
test <- matrix(test, nrow = 20, ncol = 20)
isSymmetric.matrix(test)

# summarizing raster objects
all.equal(cellStats(elev2, max), sqrt(2))
# cellStats and summary will summarize each layer separately
plot(density(elev2))

##### 4 Spatial Data Operations ====
# spatial subsetting, spatial joining and spatial aggregation 

# 4.2.1 Spatial subsetting
plot(nz[6])
canterbury = nz %>% filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]
plot(nz_height[2])
plot(canterbury)
# x[y,] subsets a target using the contents of a source object y
# this defaults to within but can be changed using the 'op' argument for sf objects
plot(nz_height[canterbury, , op = st_disjoint])
# returns plot of points that do not intersect canterbury

# another method of subsetting is using topological commands
sel_sgbp = st_intersects(x = nz_height, y = canterbury)
sel_logical = lengths(sel_sgbp) > 0
canterbury_height2 = nz_height[sel_logical, ]
plot(canterbury_height2[2])

# 4.2.2 Topological relations
# create a polygon
a_poly = st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1))))
a = st_sfc(a_poly)
# create a line
l_line = st_linestring(x = matrix(c(-1, -1, -0.5, 1), ncol = 2))
l = st_sfc(l_line)
# create points
p_matrix = matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2)
p_multi = st_multipoint(x = p_matrix)
p = st_cast(st_sfc(p_multi), "POINT")
plot(a_poly)
plot(l_line, add = T)
plot(p_multi, add = T)
# do points intersect with polygon
st_intersects(p, a, sparse = F)[, 1]
st_disjoint(p, a, sparse = F)[, 1]
st_within(p, a, sparse = F)[, 1]
st_touches(p, a, sparse = FALSE)[, 1]
# this will come in handy for distance based sampling later on
sel = st_is_within_distance(p, a, dist = 0.9)
lengths(sel) > 0

# 4.2.3 Spatial Joining
set.seed(2018) # set seed for reproducibility
(bb_world = st_bbox(world)) # the world's bounds
#>   xmin   ymin   xmax   ymax
random_df = tibble(
  x = runif(n = 10, min = bb_world[1], max = bb_world[3]),
  y = runif(n = 10, min = bb_world[2], max = bb_world[4])
)
random_points = random_df %>% 
  st_as_sf(coords = c("x", "y")) %>% # set coordinates
  st_set_crs(4326) # set geographic CRS
plot(random_points, col = "green")
world_random = world[random_points, ]
nrow(world_random)
#> [1] 4
random_joined = st_join(random_points, world["name_long"])
# st_join performs a left join by default
# set left = FALSE for inner joins

# 4.2.4 Non-overlapping joins 
plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")
# this will be useful for verifying that points are unique
any(st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE))

cycle_hire_P = st_transform(cycle_hire, 27700)
cycle_hire_osm_P = st_transform(cycle_hire_osm, 27700)
sel = st_is_within_distance(cycle_hire_P, cycle_hire_osm_P, dist = 20)
summary(lengths(sel) > 0)
z = st_join(cycle_hire_P, cycle_hire_osm_P, st_is_within_distance, dist = 20)
nrow(cycle_hire)
nrow(z)
# aggregating values for overlapping points and returning mean
head(z)
z = z %>% 
  group_by(id) %>% 
  summarize(capacity = mean(capacity))
nrow(z) == nrow(cycle_hire)
plot(cycle_hire_osm["capacity"])
plot(z["capacity"])

# 4.2.5 spatial data aggregation
# finding the average height of high points in each region of NZ
nz_avheight = aggregate(x = nz_height, by = nz, FUN = mean)
plot(nz_avheight[2])
# the result is an sf object with the same geometry as the 
# aggregating object (nz)
nz_avheight2 = nz %>%
  st_join(nz_height) %>%
  group_by(Name) %>%
  summarize(elevation = mean(elevation, na.rm = TRUE))
# congruent objects have shared borders, 
# incongruent aggregating objects do not share common borders

# 4.2.6 Distance relations
co = filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)
# st_distance returns distance matrix 

# 4.3 Spatial operations on raster data
id = cellFromXY(elev, xy = c(0.1, 0.1))
elev[id]
# the same as
raster::extract(elev, data.frame(x = 0.1, y = 0.1))
raster::extract(elev, data.frame(x = 0, y = 0))
# rasters can also be subset with another raster object 
clip = raster(xmn = 0.9, xmx = 1.8, ymn = -0.45, ymx = 0.45,
              res = 0.3, vals = rep(1, 9))
extract(elev, extent(clip))

# spatial subsetting 
elev[1:2, drop = FALSE]  
plot(elev[1:2, drop = FALSE])
elev[1, 1:2, drop = FALSE]
plot(elev[1, 1:2, drop = FALSE])
 
# masking is also possible and will be useful for resampling
# create raster mask
rmask = elev 
values(rmask) = sample(c(NA, TRUE), 36, replace = TRUE)

# spatial subsetting
m1 <- elev[rmask, drop = FALSE]           # with [ operator
m2 <- mask(elev, rmask)                   # with mask()
m3 <- overlay(elev, rmask, fun = "max")   # with overlay
all.equal(m1, m2) == all.equal(m1, m3)

# 4.3.2 Map algebra
# makes raster processing really fast
# Local, Focal, Zonal, Global, 
# Local operations comprise all cell-by-cell operations in one or 
# several layers. 
# reclassify is an example of a cell-by-cell operation
rcl = matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
recl = reclassify(elev, rcl = rcl)
plot(recl)
# calc() and overlay() functions are more efficient 
# NDVI normalized difference vegetation index is a local 
# pixel by pixel raster operation. 
# calculated from red and near infrared bands of remotely sensed
# imagery => Landsat or Sentinel
# NVDI = (NIR - RED)/(NIR + RED)
# predictive mapping is another application of local raster operations

# Focal operations take into account a central cell and its neighbours
# => Kernel, Filter, or Moving Window
# focal() for spatial filtering 
# define shape of moving window with a matrix 
# the values of this matrix correspond to weights
# => edge detection kernel, gaussian blur kernel etc
# use the fun parameter to specify the function you wish to apply to 
# this neighborhood

r_focal = focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min)
plot(r_focal)
plot(elev2)

# trying out an edge detection kernel on elev2
edge_kernel <- matrix(c(-1,-1,-1,-1,8,-1,-1,-1,-1), nrow = 3, ncol = 3)
r_focal2 <- focal(elev2, w = edge_kernel, fun = sum)
plot(r_focal2)
# elev2 is too smooth for edge detection
r_smooth <- raster(nrows = 100, ncols = 100, 
                xmn = -1, xmx = 1, ymn = -1, ymx = 1,
                vals = 1)
edges <- c(rep(1,25),rep(2,25),rep(1,25),rep(2,25))
edges <- rep(edges, 100)
m_edges <- matrix(edges, nrow = 100, ncol = 100)
r_edges <- raster(nrows = 100, ncols = 100, 
                  xmn = -1, xmx = 1, ymn = -1, ymx = 1,
                  vals = m_edges)
plot(r_edges)
r_focal_smooth <- focal(r_smooth, w = edge_kernel, fun = sum)
plot(r_focal_smooth)
r_focal_edges <-  focal(r_edges, w = edge_kernel, fun = sum)
plot(r_focal_edges)
# edge detection seems to work at least for a simple example
# box blur looks like this
bb_kernel <- matrix(c(1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9), nrow = 3, ncol = 3)
r_focal_bb <- focal(r_edges, w = bb_kernel, fun = sum)
plot(r_focal_bb)
plot(r_edges)
# for other functions ?focal
# Terrain processing like the calculation of topographic characteristics
# (slope, aspect, and flow directions) rely on focal functions
# terrain() can be used to calculate these metrics
# for other advanced metrics such as curvatures, contributing areas and 
# wetness indices are implemented in GIS software which can be accessed 
# through R => chapter 9

# 4.3.5 Zonal operations
# zonal() function for mean elevation for each grain size class
z = zonal(elev, grain, fun = "mean") %>%
  as.data.frame()
z
# returns the statistics for each category

# 4.3.6 Global operations and distances
# Global operations are a special case of zonal operations with the entire 
# raster dataset representing a single zone. 
# Useful for the computation of distance and weight rasters. 
# => Distance to rivers
# Weight the distance with elevation so that each additional altitudinal meter
# prolongs the euclidian distance. 
# Visibility and viewshed computations are also global operations => viewshed

# 4.3.7 Merging Rasters
ger = getData("alt", country = "DE", mask = TRUE)
aut = getData("alt", country = "AUT", mask = TRUE)
ch <- getData("alt", country = "CH", mask = TRUE)
aut_ch = merge(ger, aut, ch)
plot(aut_ch)
# combines two or more images gdalUtils::mosaic_rasters() is faster

# trying a line detection filter on a different file
file <- raster(system.file("external/rlogo.grd", package = "raster"))
plot(file)
r_file_edges <- focal(file, w = edge_kernel, fun = sum)
plot(r_file_edges)
# focal can also be used to create cellular automata

# Conway's Game of Life 
w <- matrix(c(1,1,1,1,0,1,1,1,1), nr = 3,nc = 3)
gameOfLife <- function(x) {
  f <- focal(x, w = w, pad = TRUE, padValue = 0)
  # cells with less than two or more than three live neighbours die
  x[f < 2 | f > 3] <- 0
  # cells with three live neighbours become alive
  x[f == 3] <- 1
  x
}

# simulation function
sim <- function(x, fun, n = 100, pause = 0.25) {
  for (i in 1:n) {
    x <- fun(x)
    plot(x, legend = FALSE, asp = NA, main = i)
    dev.flush()
    Sys.sleep(pause)
  }
  invisible(x)
}

# Gosper glider gun
m <- matrix(0, ncol = 48, nrow = 34)
m[c(40, 41, 74, 75, 380, 381, 382, 413, 417, 446, 452, 480, 
    486, 517, 549, 553, 584, 585, 586, 619, 718, 719, 720, 752, 
    753, 754, 785, 789, 852, 853, 857, 858, 1194, 1195, 1228, 1229)] <- 1
init <- raster(m)

# run the model
sim(init, gameOfLife, n = 1000, pause = 0.25)

#### 5 Geometry Operations ====
# 5.2 Geometric operations on vector data
# 5.2.1 Simplification 
# Simplification is a process for reducing the amount of memory used
# => simplify complex geometries before publishing them as interactive maps
# simplification may lead to overlapping 
# rmapshaper::ms_simplify() overcomes this issue

# 5.2.2 Centroids
# identify the center of geographic objects, most common measure is the 
# geographic centroid

# 5.2.3 Buffers
# Polygons representing the area within a given distance of a geometric feature
# regardless of whether the input is a point, line or polygon.
# this will come in handy for sampling points and pseudo negative data

seine_buff_5km = st_buffer(seine, dist = 5000)
seine_buff_50km = st_buffer(seine, dist = 50000)

plot(seine_buff_5km)
plot(seine_buff_50km)
plot(random_points)
point_buff_100m <- st_buffer(random_points, dist = 100)
plot(point_buff_100m)

# affine operations: 
# shifting, scaling, rotation (requires rotationmatrix)
# new created geometries can replace old ones with the st_set_geometry() command
# clipping => spatial subsetting that involves changes to the geometry of at least
# some affected features
# geometry unions => silently dissolve the geometries of touching polygons 
# in the same group this is based on the st_union() function
# type transformation is most likely not going to be relevant for me
# can be useful for converting multipoint to linestring to calculate total 
# distance traveled

# 5.3 Geometric operations on raster data
# shift flipping mirroring scaling rotation or warping of images
# => changes in extent, origin or resolution of an image
# 5.3.1 geometric intersections
data("elev", package = "spData")
clip = raster(xmn = 0.9, xmx = 1.8, ymn = -0.45, ymx = 0.45,
              res = 0.3, vals = rep(1, 9))
clipped <- elev[clip, drop = FALSE]
# one can also use the intersect() and crop() command for the same purpose
# for merging or performing map algebra the resolution, projection, origin and extent
# have to match 
elev_2 = extend(elev, c(1, 2), value = 1000)
plot(elev_2)
# Performing an algebraic operation on two objects with differing extents in R, the 
# raster package returns the result for the intersection
elev_3 <- elev_2 + elev
plot(elev_3)
# using extend() with another raster object allows us to skip out on defining the 
# rows and columns to be extended
elev_4 = extend(elev, elev_2)
# the extended cells recieve the value NA
# changing the origin may be helpful to completely overlap two rasters
# or to simply change the look in plots 
# changing the resolution also frequently changes the origin

# 5.3.3 Aggregation and Disaggregation
data("dem", package = "RQGIS")
dem_agg = aggregate(dem, fact = 5, fun = mean)
par(mfrow = c(1,2))
plot(dem)
plot(dem_agg)
par(mfrow = c(1,1))
dem_disagg = disaggregate(dem, fact = 5, method = "bilinear")
plot(dem_disagg)
# resample() allows us to align multiple raster files at once
# rasterprocessing can also be parallelized with beginCluster() and clusteR()

# 5.4 Raster Vector cropping
# cropping and masking are useful to unify the spatial extent of the used data
srtm = raster(system.file("raster/srtm.tif", package = "spDataLarge"))
zion = st_read(system.file("vector/zion.gpkg", package = "spDataLarge"))
zion = st_transform(zion, projection(srtm))
plot(srtm)
plot(st_geometry(zion), add = T)
# crop() reduces the rectangular extent of the object passed to its first argument 
# based on the extent of the object passed to its second argument
srtm_cropped <- crop(srtm, zion)
plot(srtm_cropped)
# mask functions in a similar way
srtm_masked <- mask(srtm, zion)
plot(srtm_masked)
# the inverse option for mask might prove useful later on 
data("zion_points", package = "spDataLarge")
plot(zion_points, add = T)
zion_points$elevation = raster::extract(srtm, zion_points)
# extraction is also possible with a buffer zone, this proves a lot faster than individually
# extracting points and filtering based on distance measures
zion_points$buffer <- raster::extract(srtm, zion_points, buffer = 50)
# the buffer column in zion_points is a list of all values inside the buffer zone

# extracting values along a line is also possible => bike route planning
zion_transect <- cbind(c(-113.2, -112.9), c(37.45, 37.2)) %>%
  st_linestring() %>% 
  st_sfc(crs = projection(srtm)) %>% 
  st_sf()
transect = raster::extract(srtm, zion_transect, 
                           along = TRUE, cellnumbers = TRUE)
# this object contains a column with the cell ids and another with the corresponding dem values
# it is a matrix wrapped in a list so not easy to process
# converting to data.frame
transect_df = purrr::map_dfr(transect, as_data_frame, .id = "ID")
# extracting coordinate values at the corresponding cell IDs
transect_coords = xyFromCell(srtm, transect_df$cell)
# calculating distances along path
pair_dist = geosphere::distGeo(transect_coords)[-nrow(transect_coords)]
# adding everything together to get an elevation profile along the path
transect_df$dist = c(0, cumsum(pair_dist)) 
plot(srtm~dist, data = transect_df, type = "l", xlab = "Distance (m)", ylab = "Elevation (m)")
# extracting along polygons is also possible and might be useful for summary stats
# per polygon 
zion_srtm_values = raster::extract(x = srtm, y = zion, df = TRUE)
# generating summary statistics for the extracted polygon
group_by(zion_srtm_values, ID) %>% 
  summarize_at(vars(srtm), list(~min(.), ~mean(.), ~max(.)))
# this is very useful for counting occurences of categorial data within polygons
data(nlcd, package = "spDataLarge")
zion_nlcd = raster::extract(nlcd, zion, df = TRUE, factors = TRUE) 
dplyr::select(zion_nlcd, ID, levels) %>% 
  tidyr::gather(key, value, -ID) %>%
  group_by(ID, key, value) %>%
  tally() %>% 
  tidyr::spread(value, n, fill = 0)
# in case raster::extract is too slow one might consider 
# parallelization => splitting the points into multiple lists or polygons into 
# multiple parts, use of the velox package or use of GIS software

# the following will be used to calculate distances to rivers and water
# proof of concept (pretty long runtime)
line_raster <- mask(srtm, zion_transect)
distance_line <- distance(line_raster)
plot(distance_line)

# 5.4.3 Rasterization
cycle_hire_osm_projected = st_transform(cycle_hire_osm, 27700)
raster_template = raster(extent(cycle_hire_osm_projected), resolution = 1000,
                         crs = st_crs(cycle_hire_osm_projected)$proj4string)
# presence absence raster
ch_raster1 = rasterize(cycle_hire_osm_projected, raster_template, field = 1)
# counting the number of occurances in the presence absence raster
ch_raster2 = rasterize(cycle_hire_osm_projected, raster_template, 
                       field = 1, fun = "count")
plot(ch_raster2)
# very useful for a heatmap
# heatmap for aggregated capacity in every cell
ch_raster3 = rasterize(cycle_hire_osm_projected, raster_template, 
                       field = "capacity", fun = sum)
plot(ch_raster3)

# rasterization of polygons => cast polygon to multilinestring and rasterize
california = dplyr::filter(us_states, NAME == "California")
# another method of plotting only the outline later on
california_borders = st_cast(california, "MULTILINESTRING")
# creating a template to be filled with the rasterized polygon
raster_template2 = raster(extent(california), resolution = 0.5,
                          crs = st_crs(california)$proj4string)
# rasterization of the multilinestring
california_raster1 = rasterize(california_borders, raster_template2)
# rasterization of the polygon
california_raster2 = rasterize(california, raster_template2)
par(mfrow = c(1,2))
plot(california_raster1)
plot(california_raster2)
par(mfrow = c(1,1))
# polygon rasterization selects all cells whose centroids are inside the polygon
# fasterize::fasterize() is 100 times faster but limited to polygons
# gdalUtils::gdal_rasterize() is also faster but requires a vectorfile and 
# rasterization parameters

# 5.4.4 Spatial Vectorization
# => converting spatially continuous raster data into distinct point data
elev_point <- rasterToPoints(elev, spatial = TRUE) %>% st_as_sf()
plot(elev_point)
# can be very useful for creating contour lines for dem plots
data(dem, package = "RQGIS")
cl = rasterToContour(dem)
plot(dem, axes = FALSE)
plot(cl, add = TRUE)

hs = hillShade(slope = terrain(dem, "slope"), aspect = terrain(dem, "aspect"))
plot(hs, col = gray(0:100 / 100), legend = FALSE)
# overlay with DEM
plot(dem, col = terrain.colors(25), alpha = 0.5, legend = FALSE, add = TRUE)
# add contour lines
contour(dem, col = "white", add = TRUE)
# hillshade might be an interesting variable to consider for the bavaria dataset
# the last vectorization method converts rasters to polygons 
grain_poly = rasterToPolygons(grain) %>% 
  st_as_sf()
grain_poly2 = grain_poly %>% 
  group_by(layer) %>%
  summarize()
plot(grain_poly)
plot(grain_poly2)

#### 6 Reprojecting Geographic Data ====
geosphere::distGeo(c(0, 0), c(1, 0))
# for operations involving distances such as buffering, the only way to ensure a 
# good result is to create a projected copy of the data and run the operation on that
london_proj = data.frame(x = 530000, y = 180000) %>% 
  st_as_sf(coords = 1:2, crs = 27700)
plot(london_proj)
# if the units of a crs are meters rather than degrees, then it has been reprojected
london_proj_buff = st_buffer(london_proj, 111320)
plot(london_proj_buff)

# 6.2 When to Reproject
# for using geometric functions such as st_buffer() reprojecting is essential
# buffer zone around sites => reprojecting to reduce calculation time
# will be vital for resampling later on 

# 6.3 Which CRS to use
# WGS84 is the most common CRS in the world => EPSG code: 4326
# WGS84 is default most of the time
# All CRSs are either equal-area, equidistant, conformal (with shapes remaining
# unchanged) thus a one has to make the correct choice based on the application.
# UTM is another alternative that is widely used
# UTM should be restricted to at most 6 degrees off the central point of the study area

# 6.4 Reprojecting Vector Geometries
# reprojecting vectors consists of transforming the coordinates of the points
# the following can be used to find out wich location a epsg code corresponds to
crs_codes = rgdal::make_EPSG()[1:2]
dplyr::filter(crs_codes, code == 27700)
# 6.5 modidfying map projections
# many custom projections can be accessed and used through the proj4string argument
world_mollweide = st_transform(world, crs = "+proj=moll")
plot(world_mollweide[9])
# sometimes it can be beneficial to have as little distortion as possible for
# area, direction, distance, to achieve this the winkel triple projection comes in handy
world_wintri = lwgeom::st_transform_proj(world, crs = "+proj=wintri")
plot(world_wintri[9])
# equal area lambert azimuth projection
world_laea1 = st_transform(world, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=360 +lat_0=0")
plot(world_laea1[9])
# change the centering of the projection with the lon & lat parameters
world_laea2 = st_transform(world, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-74 +lat_0=40")
plot(world_laea2[9])

world_wintri2 <- lwgeom::st_transform_proj(world, crs = "+proj=moll +x_0=0 +y_0=0 +lon_0=-90 +lat_0=0")
plot(world_wintri2[9])
# create animation of rotating globus (proof of concept)

globe <- function(n=10, pause=0.25) {
  for (i in 1:n) {
    string <- paste("+proj=laea +x_0=0 +y_0=0 +lon_0=",i," +lat_0=0", sep = "")
    world_loop <- st_transform(world, crs = string)
    plot(st_geometry(world_loop[9]))
    dev.flush()
    Sys.sleep(pause)
  }
  invisible(world_loop)
}

globe(n = 360, pause = 0.1)
# seems to work but the rotation breaks the variables once they get too distorted.

# 6.6 Reprojecting Raster Geometries
# Raster reprojection breaks down into two separate tasks: 
# a reprojection of the cell centroids 
# and a recalculation of cell values to fill the new raster 
# raster reprojection can be accomplished with projectRaster()
cat_raster = raster(system.file("raster/nlcd2011.tif", package = "spDataLarge"))
plot(cat_raster)
wgs84 = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
cat_raster_wgs84 = projectRaster(cat_raster, crs = wgs84, method = "ngb")
plot(cat_raster_wgs84)
# reprojecting numeric rasters follows a very similar procedure
con_raster = raster(system.file("raster/srtm.tif", package = "spDataLarge"))
plot(con_raster)
equalarea = "+proj=laea +lat_0=37.32 +lon_0=-113.04"
con_raster_ea = projectRaster(con_raster, crs = equalarea, method = "bilinear")
plot(con_raster_ea)
# raster reprojection almost always leads to differences to the original data
# in number of cells, resolution and extent

#### 7 Geographic data I/O ====
# 7.2 Retrieving open data
download.file(url = "http://nrdata.nps.gov/programs/lands/nps_boundary.zip",
              destfile = "nps_boundary.zip")
unzip(zipfile = "nps_boundary.zip")
usa_parks = st_read(dsn = "nps_boundary.shp")
plot(usa_parks)
# packages for importing geospatial data directly into R
# getlandsat 	    Provides access to Landsat 8 data.
# osmdata 	      Download and import of OpenStreetMap data.
# raster 	        getData() imports administrative, elevation, WorldClim data.
# rnaturalearth 	Access to Natural Earth vector and raster data.
# rnoaa 	        Imports National Oceanic and Atmospheric Administration (NOAA) climate data.
# rWBclimate    	Access World Bank climate data.
# hddtools        for a wide array of hydrological data

# API requests can be generated programmatically with the GET() function
# for further information refer to section 7.4 in the book

# st_read() guesses the correct driver based on the file extension
# the dsn has to be a string that directly refers to a file. For some drivers 
# the dsn could also be a path leading to a folder 

# use band to read a single layer from a raster file
# use brick to read them all at once

# st_write and writeRaster to write vector and raster data respectively
# saving either directly to png or through builtin tmap and/or mapview functions

#### 8 Making Maps with R ====
# tmap allows for a wide range of map making with syntax similar to ggplot2
# tmap is based on the concept of grammar for graphics => split the data from 
# used aesthetics to widen the range of possible plots

tm_shape(nz) + tm_fill() + tm_borders()
# slick as fuck 
# start by passing an 'sf' object to tm_shape and add layers to it

# qtm for quick thematic map is another option for maps but its harder to control 
# individual layers 
qtm(world)
# map objects can be saved as usual. One might add other layers later on
map_nz = tm_shape(nz) + tm_polygons()
map_nz1 = map_nz +
  tm_shape(nz_elev) + tm_raster(alpha = 0.75)
# other layers can be added 
nz_water = st_union(nz) %>% st_buffer(22200) %>% 
  st_cast(to = "LINESTRING")
map_nz2 = map_nz1 +
  tm_shape(nz_water) + tm_lines()
map_nz3 = map_nz2 +
  tm_shape(nz_height) + tm_dots()
# use tmap_arrange() to arrange all maps in a single map
tmap_arrange(map_nz1, map_nz2, map_nz3)

# 8.2.3 Aesthetics
# trying to refine the rotating world map function with tmap
gen_maplist <- function(n=10, pause = 0.25) {
  plotlist <- list()
  for (i in 1:n) {
    string <- paste("+proj=laea +x_0=0 +y_0=0 +lon_0=", i ," +lat_0=0", sep = "")
    world_loop <- st_transform(world, crs = string)
    plotlist[[i]] <- tm_shape(world_loop) + tm_borders()
    # dev.flush()
    Sys.sleep(pause)
  }
  print(plotlist)
  invisible(plotlist)
}

gen_maplist(n = 360, pause = 0)
# this is a lot slower than the plot() based version but only plots outlines 
# and is a lot better looking

# map aesthetics do not require an aes() argument, instead they can be passed to 
# tmap directly
ma1 = tm_shape(nz) + tm_fill(col = "red")
ma2 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3)
ma3 = tm_shape(nz) + tm_borders(col = "blue")
ma4 = tm_shape(nz) + tm_borders(lwd = 3)
ma5 = tm_shape(nz) + tm_borders(lty = 2)
ma6 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3) +
  tm_borders(col = "blue", lwd = 3, lty = 2)
tmap_arrange(ma1, ma2, ma3, ma4, ma5, ma6)

# aesthetics can also be variables that change 
tm_shape(nz) + tm_fill(col = "Land_area")
# always requires a character string that refers to the variable that should be plotted
# legend titles can be added through the title argument
# here expression() is used to create superscript text
legend_title = expression("Area (km"^2*")")
map_nza = tm_shape(nz) +
  tm_fill(col = "Land_area", title = legend_title) + tm_borders()
# the colors used can be manipulated as follows: 
tm_shape(nz) + tm_polygons(col = "Median_income")
breaks = c(0, 3, 4, 5) * 10000
tm_shape(nz) + tm_polygons(col = "Median_income", breaks = breaks)
tm_shape(nz) + tm_polygons(col = "Median_income", n = 10)
elevation_map <- tm_shape(nz) + tm_polygons() +
  tm_shape(nz_elev) + tm_raster(alpha = 1, style = "cont")
# the style argument is important to visualize details in the map data correctly
# pretty = round breaks into whole numbers; equal = divides bins into equal ranges
# quantile = same number of observations per break; jenks maximizes differences between categories
# cont = continuous, useful for elevation maps; cat = categorical values
tm_shape(nz) + tm_polygons() +
  tm_shape(nz_elev) + tm_raster(alpha = 1, style = "quantile", palette = "RdBu")
tm_shape(nz) + tm_polygons() +
  tm_shape(nz_elev) + tm_raster(alpha = 1, style = "fisher", palette = "RdBu")
tm_shape(nz) + tm_polygons() +
  tm_shape(nz_elev) + tm_raster(alpha = 1, style = "equal", palette = "RdBu")

# 8.2.5 Layouts 
# adding elements with +
elevation_map + 
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 50, 100), size = 1) + 
  tm_style("col_blind")

# 8.2.6 faceted maps
# faceted maps can be created with the tm_facets function
urb_1970_2030 = urban_agglomerations %>% 
  filter(year %in% c(1970, 1990, 2010, 2030))
tm_shape(world) +
  tm_polygons() +
  tm_shape(urb_1970_2030) +
  tm_symbols(col = "black", border.col = "white", size = "population_millions") +
  tm_facets(by = "year", nrow = 2, free.coords = FALSE)
# 8.2.7 inset maps
# inset maps are useful to provide context or zoom into a specific region 
# might be useful to get a closer look at the settlements near the danube

# 8.3 Animated maps
# rely on digital publication => obvious
# animated maps are created using the tm_facets function with the argument 
# 'along' instead of 'by'
# 8.4 interactive maps 
# interactive maps can be created by tmap, mapview and mapdeck => 2.5D maps 

set_token(Sys.getenv("MAPBOX"))
crash_data = read.csv("https://git.io/geocompr-mapdeck")
crash_data = na.omit(crash_data)
ms = mapdeck_style("dark")
mapdeck(style = ms, pitch = 45, location = c(0, 52), zoom = 4) %>%
  add_grid(data = crash_data, lat = "lat", lon = "lng", cell_size = 1000,
           elevation_scale = 50, layer_id = "grid_layer",
           colour_range = viridisLite::plasma(6))

# leaflet is the most common mapping package 
pal = colorNumeric("RdYlBu", domain = cycle_hire$nbikes)
leaflet(data = cycle_hire) %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addCircles(col = ~pal(nbikes), opacity = 0.9) %>% 
  addPolygons(data = lnd, fill = FALSE) %>% 
  addLegend(pal = pal, values = ~nbikes) %>% 
  setView(lng = -0.1, 51.5, zoom = 12) %>% 
  addMiniMap()

# maps can also be hosted on a shiny server, => easy

#### 9 Bridges to GIS Software ====
# this requires the installation of grass, saga and qgis
library(RQGIS)
# rqgis works only with qgis 2.18 make sure to install the correct 64 bit version 
# else python is going to act up and cause a lot of pain
# searching the disk for the qgis install
set_env("C:/QGIS218")
# hosting a custom interface to qgis
open_app()
# cant get it to work, skipping this part of the chapter 
# Error in initialize_python(required_module, use_environment) : 
# Your current architecture is 64bit however this version of Python is compiled for 32bit.
# even after multiple reinstalls 

# 9.3 RSAGA
library(RSAGA)
library(link2GI)
saga = linkSAGA()
rsaga.env(path = "C:/Users/benea/Downloads/saga-7.2.0_x64")
data(landslides)
write.sgrd(data = dem, file = file.path(tempdir(), "dem"), header = dem$header)
rsaga.get.libraries()
rsaga.get.modules(libs = "ta_hydrology")
rsaga.get.usage(lib = "ta_hydrology", module = "SAGA Wetness Index")
# calculating wetness index
params = list(DEM = file.path(tempdir(), "dem.sgrd"),
              TWI = file.path(tempdir(), "twi.sdat"))
rsaga.geoprocessor(lib = "ta_hydrology", module = "SAGA Wetness Index", 
                   param = params)
twi = raster::raster(file.path(tempdir(), "twi.sdat"))
# shown is a version using tmap
plot(twi, col = RColorBrewer::brewer.pal(n = 9, name = "Blues"))

# 9.4 RGRASS through rgrass7
# trying to solve the traveling salesman problem with rgrass7
# cant get grass to work on my computer either, skipping the code for this part

#### 10 Scripts, Algorithms, and Functions ====
# 10.2 Scripts
# the reprex package is useful for reproducible examples
poly_mat1 = cbind(
  x = c(0, 0, 9, 9, 0),
  y = c(0, 9, 9, 0, 0)
)
x_coords = c(10, 0, 0, 12, 20, 10)
y_coords = c(0, 0, 10, 20, 15, 0)
poly_mat = cbind(x_coords, y_coords)
Origin = poly_mat[1, ]
# create 'triangle matrix':
T1 = rbind(Origin, poly_mat[2:3, ], Origin) 
# find centroid (drop = FALSE preserves classes, resulting in a matrix):
C1 = (T1[1, , drop = FALSE] + T1[2, , drop = FALSE] + T1[3, , drop = FALSE]) / 3
plot(poly_mat)
# low level functions can be written and modified, 
# but for my purposes it is best to stick to already existing functions

#### 11 Statistical learning ====
