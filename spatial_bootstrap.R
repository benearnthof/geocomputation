#### Comparing different Bootstrapping procedures for spatially correlated data. 
# This is very interesting for estimation of predictor performance through resampling. 
# Classical resampling procedures do not respect the inherent spatial correlation 
# present in spatial data. Thus predictor performance tends to be overestimated. 

#### Running this script uses about 13.5 GB of ram on my computer, (Ubuntu 18.04 LTS)
# so running it on machines that have less than 16 GB of ram available, or are using
# operating systems that require more ram in idle might lead to errors. 
# Running the bagged bootstrap also took around 2300 seconds on my CPU (i5-4690K)

# Load packages 

rm(list = ls()) # clean the workspace
packagelist <- c("rgdal", "foreign","RColorBrewer","plyr","raster","ncdf","rgeos",
                 "maptools","rgeos","rasterVis","gstat","caret")
newpackages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
if (length(newpackages)) install.packages(newpackages)
lapply(packagelist, function(i) require(i, character.only = TRUE))
remove(packagelist, newpackages)
# to install the package rgeos on linux you need to run 
# sudo apt-get install libgeos-dev
# first

# sudo apt-get install libgdal-dev libproj-dev
# sudo apt-get install r-cran-spdep 
# check installation of spdep with
# sudo dpkg-query -l | grep r-cran-spdep *

# macro parameter settings 
size_lon <- 200
size_lat <- 200

# general error terms with spatial correlation - one sample
# a raster without spatial correlation
errors <- matrix(rnorm(size_lon*size_lat, mean = 0, sd = 1),size_lon,size_lat)
# rasterize error values
errors_raster <- raster(errors)
extent(errors_raster) <- c(0,1,0,1)
plot(errors_raster)

# a raster with spatial correlation
positions <- expand.grid(1:200, 1:200)
names(positions) <- c('X','Y')
# define the gstat object (spatial model)
variomodel <- gstat(formula = z~1, locations = ~X+Y, dummy = T, 
                 beta = 1, model = vgm(psill = 1,model = 'Exp',range = 50), nmax = 20)
# make four simulations based on the gstat object
pred1 <- predict(variomodel, newdata = positions, nsim = 4)
# show one realisation
# converting simulations to gridded (spatial) objects
gridded(pred1) = ~X+Y
spplot(pred1)
# standardizing and rasterizing predictions
pred1rast <- (raster(pred1) - mean(as.matrix(raster(pred1))))/sd(as.matrix(raster(pred1)))
extent(pred1rast) <- c(0,1,0,1)
plot(pred1rast)

# change the resoultion of the raster to size_lon*size_lat
# note:  'bilinear' - values are locally interpolated (using the resample function)
# note: for disaggregate() function, 'fact' has to be an integer
#pred1_new <- disaggregate(pred1rast, fact = c(size_lon/200,size_lat/200), method = 'bilinear')
# compare two different resolutions
#par(mfcol = c(1, 2))
#plot(pred1rast, main = "Resolution - Low")
#plot(pred1_new, main = "Resolution - High")
#par(mfcol = c(1, 1))

# generating simulation population of 300 rasters as shown above
pop_size <- 300
positions <- expand.grid(1:200, 1:200)
names(positions) <- c('X','Y')
# define the gstat object (spatial model)
variomodel <- gstat(formula = z~1, locations = ~X+Y, dummy = T, 
                 beta = 1, model = vgm(psill = 1,model = 'Exp',range = 50), nmax = 20)
# make four simulations based on the gstat object
pred2 <- predict(variomodel, newdata = positions, nsim = pop_size)
# show one realisation
gridded(pred2) = ~X+Y
spplot(pred2[1:4], main = "Resolution - 200 * 200")
# standardize the raster stack
resultbrick <- brick(pred2)
for (i in 1:pop_size) {
  pred2[[i]] <- (pred2[[i]] - colMeans(as.matrix(pred2[[i]])))/sd(as.matrix(pred2[[i]]))
  print(i)
}
extent(resultbrick) <- c(0,1,0,1)
plot(resultbrick[[1:4]], main = "Resolution - 200 * 200")
# change the resoultion of the raster to size_lon*size_lat

# pred2_new <- disaggregate(resultbrick, fact = c(2, 2), method = 'bilinear')
# pred2_agg <- aggregate(pred2_new, fact = c(0.625, 0.625), method = "bilinear")
# plot(pred2_new[[1:4]], main = "")
# save the raster brick
# writeRaster(pred2_new[[1:50]], filename="pred2_new1.tif", format="GTiff", overwrite=TRUE) 
# writeRaster(pred2_new[[51:100]], filename="pred2_new2.tif", format="GTiff", overwrite=TRUE) 

# generate independent variable
x <- matrix(rnorm(size_lon*size_lat, mean = 1, sd = 1),size_lon,size_lat)
x_rast <- raster(x)
extent(x_rast) <- c(0,1,0,1) 
plot(x_rast,main = "X - resolution - 200 * 200")

# generate intercept
intercept <- matrix(1,size_lon,size_lat)
intercept_rast <- raster(intercept)
extent(intercept_rast) <- c(0,1,0,1)
plot(intercept_rast)

# generate dependent variable
# initialize the stack, set values to NA
y_rast <- resultbrick
y_rast <- setValues(y_rast, NA)
for (i in 1:pop_size) {
  y_rast[[i]] <- sum(stack(intercept_rast, x_rast, resultbrick[[i]])) 
  print(i)
}
plot(y_rast[[1:4]], main = "Y - resolution - 200 * 200")

# assmeble data - with spatial correlation
data_rast <- stack(y_rast,x_rast)
names(data_rast[[301]]) <- c("x_rast") # name the independent variable
data <- as.data.frame(data_rast)

# simple linear regression

# data with spatial correlation

ols <- matrix(NA,pop_size,1)
for (i in 1:pop_size) {
  fit <- lm(data[,i]~x_rast, data = data) # Ordinary Least Squares
  ols[i] <- coef(fit)[2] # extract estimate on X coefficient
  print(i)
}

plot(density(ols), col = "blue", lwd = 2, xlim = c(0.995,1.005), main = "Ordinary Least Squares")
abline(v = 1, col = "black", lty = 2) 

# spatial block bootstrapping
# parameter setting for bootstrapping
b <- 2500  # block size for bootstrapping
b_lon <- ceiling(sqrt(b)) # longitude length of block 
b_lat <- ceiling(sqrt(b)) # latitude length of block 
tolerance <- 0.2; # tolerance for percentage of 'NA' values in selected blocks
draws <- 500  # number of draws for block bootstrapping

map = stack(data_rast[[sample(1:pop_size,1,replace = TRUE)]],data_rast[[pop_size + 1]])
nx <- nrow(map)
ny <- ncol(map)

b_boot1 <- matrix(NA,draws,1)

for (i in 1:draws) {
  na_counter <- 1 # a counter for 'NA' values in selected block
  while (na_counter > tolerance)
  {
    sub_x <- sample(1:nx - b_lon,1,replace = TRUE) # determine the starting point of block - X dimension
    sub_y <- sample(1:ny - b_lat,1,replace = TRUE) # determine the starting point of block - Y dimension
    
    multi_block <- matrix(NA,b_lon*b_lat,nlayers(map)) # initialize the outcome matrix
    multi_block <- getValuesBlock(map,sub_x,b_lon,sub_y,b_lat,lyr = 1:nlayers(map))
    na_mat <- as.vector(1:nlayers(map))
    
    for (j in 1:nlayers(map)) {
      na_mat[j] <- sum(is.na(multi_block[,j]))/length(multi_block[,j])
    }
    na_counter <- min(na_mat)
    if (na_counter <= tolerance) { break
      cat('\n','Percentage of NULL cell values:',na_counter*100,',no block selected!')
    }
  }
  data_block <- as.data.frame(multi_block)
  names(data_block) <- c("y_rast","x_rast")
  fit <- lm(y_rast~x_rast,data = data_block) # Ordinary Least Squares
  b_boot1[i] <- coef(fit)[2] # extract estimate on X coefficient
  print(i)
}

plot(density(b_boot1),col = "blue",lwd = 2,xlim = c(0.99,1.01),ylim = c(0,800),main = "Ordinary Least Squares")
lines(density(ols),col = "red",lwd = 2)
legend("topright", inset = .05, title = "Spatial Block Bootstrapping", 
       c("Estimated Distribution","True Distribution"),col = c("blue","red"),lwd = 2,cex = 0.8)
abline(v = 1, col = "black",lty = 2) # true beta

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Step 6: spatial bootstrapping - little bag of bootstraps
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# parameter setting for bootstrapping
# note: total population size = size_lon*size_lat
b_num <- 4  # number of bags
draws <- 50  # number of draws within each bag

map = stack(data_rast[[sample(1:pop_size,1,replace = TRUE)]],data_rast[[pop_size + 1]])
mapdata <- as.data.frame(map)
names(mapdata) <- c("y","x")
# split the data into 'b_num' random bags
bags <- createFolds(mapdata$y, k = b_num, list = TRUE, returnTrain = FALSE)

b_boot2 <- matrix(NA,draws,b_num)


# library(parallel)
# no_cores <- detectCores()
# clust <- makeCluster(4)
# 
# clusterEvalQ(clust,
#   for (i in 1:b_num) {
#     bag <- mapdata[bags[[i]],c("y","x")]
#     for (j in 1:draws) {
#       # resample the entire population from the i-th bag
#       bag_data <- bag[sample(nrow(bag), nrow(mapdata), replace = TRUE),]
#       fit <- lm(y~x,data = bag_data) # Ordinary Least Squares
#       b_boot2[j,i] <- coef(fit)[2] # extract estimate on X coefficient
#       print(c(i,j)) # tracking both loops
#   }
# } )


for (i in 1:b_num) {
  bag <- mapdata[bags[[i]],c("y","x")]
  for (j in 1:draws) {
    # resample the entire population from the i-th bag
    bag_data <- bag[sample(nrow(bag), nrow(mapdata), replace = TRUE),]
    fit <- lm(y~x,data = bag_data) # Ordinary Least Squares
    b_boot2[j,i] <- coef(fit)[2] # extract estimate on X coefficient
    print(c(i,j)) # tracking both loops
  }
}


  # plotting for all bags
par(mfrow = c(2,2))
for (p in 1:b_num) {
  plot(density(b_boot2[,p]),col = "blue",lwd = 2,xlim = c(0.975,1.025),ylim = c(0,750),
       main  =  "Little Bag Bootstrap")
  lines(density(ols),col = "red",lwd = 2)
  legend("topright", inset = .05, title = "Spatial Block Bootstrapping", 
         c("Estimated Distribution","True Distribution"),col = c("blue","red"),lwd = 2,cex  =  0.2)
  abline(v  =  1, col  =  "black",lty = 2) # true beta
}
par(mfrow = c(1,1))

avg_bboot <- rowSums(b_boot2)/ncol(b_boot2)
plot(density(avg_bboot),col = "blue",lwd = 2,xlim = c(0.975,1.025),ylim = c(0,750),
     main  =  "Little Bag Bootstrap")
lines(density(ols),col = "red",lwd = 2)
legend("topright", inset = .05, title = "Spatial Block Bootstrapping", 
       c("Estimated Distribution","True Distribution"),col = c("blue","red"),lwd = 2,cex  =  0.2)
abline(v  =  1, col  =  "black",lty = 2)

