#' Creeping Line  pattern
#' ===

#' Loading libraries
library(TurtleGraphics)
library(maptools)
library(rgeos)
library(rgdal)
library(raster)
library(rasterVis)
library(plotKML)

#' Set the area of interest. The user has to create a bounding box and save it to
#' the working directory. For this example we created a bounding box in KML. Setting the coordinate system.
#' For this example it is not necessary to set it because we use KML so the projection is WGS'84.Plotting
#' the area of interest.
area=readOGR("Data\bbox.kml",layer="bbox")
proj4string(area) = CRS("+proj=longlat +datum=WGS84 +no_defs")
plot(area)

#' Create functions to open the links. Through first link the user can see the pattern (creepinh line) and
#' understand the characteristics of it. With the second link the user can choose the Digital Elevation Model
#' of the area of interest and save it to the directory in order to use it later.
meet_the_pattern <- function(){
  browseURL("http://www.navdynamic.com.au/p_navigator-search-rescue-parallel.html")
}

get_your_dem <- function(){
  browseURL("http://srtm.csi.cgiar.org/SELECTION/inputCoord.asp")
}

#' Create function for Creeping Line pattern
creeping_line_pattern <- function(area, n=10, initial_step=0.03, toplot=T, toexport=T ){

  #' According to the pattern the start point is somewhere at the edges of bounding box. The user has to set
  #' the coordinates of the point and plot it in the bounding box.
  point_start=SpatialPoints(cbind(26.24,39.24))
  proj4string(point_start) = CRS("+proj=longlat +datum=WGS84 +no_defs")
  plot(point_start)
  plot(area,add=T)
  x_first=26.24
  y_first=39.24

  #' Create an empty list. The list will store the coordinates of nodes of the legs. The user will set the number
  #' of legs and the length of them through a step. To this pattern the parallels legs have the same lenght
  #' (according to the initial step).That means that there are only two different values for this pattern.
  #' All the even-number legs have the same lenght and all the odd legs have the same length too.
  #' For the example the initial step is 0.03 degrees.
  leg_list = list()

  #' For this pattern we need the TurtleGraphics package. We set the dimensions of the bounding box in which
  #' the turtle has to move. Setting the turtle to the starting point(centroid). In case the pattern is bigger
  #' than the bounding box with the mode we can set the "clip"  which automatically "cuts" the pattern to
  #' the bounding box.
  turtle_init(width=0.173,height=0.211, mode = c("clip"))
  turtle_setpos(x_first,y_first)

  #' Set the start point and create a loop for the pattern. All the odd legs will have the same length (0.03degrees for this example).
  #' The other legs will be bigger(with a standard step)(0.15 degrees for this example).Every four legs and for two (legs) the
  #' UAV will turn 90 degrees right.For the other legs the UAV will turn 90 degrees left. To set the turns we need to create a 
  #' vector with the sequence of numbers of legs where the UAV will turn right. For every node of the pattern
  #' the coordinates will be stored in the list.
  right=c((seq(1,n,4)),(seq(1,n,4))+1)

  for (i in 1:n){

    if (i ==1){
      x_start = x_first
      y_start = y_first
      step = initial_step
    }else{
      x_start = x_end
      y_start = y_end
    }

    if(i%%2 == 0){

      step = initial_step + 0.15

    } else{
      step=initial_step
    }
    if (is.element(i,right)){
      turtle_right(angle=90)
      turtle_forward(dist=step)
    }else{
      turtle_left(angle=90)
      turtle_forward(dist=step)
    }
    y_end=turtle_getpos()[[2]]
    x_end=turtle_getpos()[[1]]
    leg_list[[i]]=c(y_start,x_start, y_end,x_end)


  }

  #' According to the range of the coordinates we unlist the list to store our legs/lines.
  cr_range = range(unlist(leg_list))

  for (k in 1:n){
    leg=leg_list[[k]]

  }

  #' Create a data.frame with the coordinates of legs. Setting the labels of columns.
  creepingdf = data.frame(matrix(unlist(leg_list), ncol=4, byrow=T))
  names(creepingdf)=c("y_from", "x_from", "y_to", "x_to")

  #' Cteate lines/legs from the data.frame.
  lines = vector("list", nrow(creepingdf))
  for (i in seq_along(lines)) {
    lines[[i]] <- Lines(list(Line(rbind(c(creepingdf$x_from[i],creepingdf$y_from[i]),  c(creepingdf$x_to[i], creepingdf$y_to[i]) ))), as.character(i))
  }

  #' Create simple SpatialLines from the lines
  linessp = SpatialLines(lines)
  proj4string(linessp) = CRS("+proj=longlat +datum=WGS84 +no_defs")

  #' Create SpatialLinesDataFrame from simple SpatialLines
  #' Plotting the pattern
  creepingSLDF = SpatialLinesDataFrame(linessp,creepingdf)
  proj4string(creepingSLDF) = CRS("+proj=longlat +datum=WGS84 +no_defs")
  if (toplot){
    plot(creepingSLDF)
  }

  #' Loading the DEM and plotting it. DEM is a raster file, so we need the raster package.
  #' See the characteristics of DEM.
  dem=raster("Data\dem_lesvos.tif")
  plot(dem)
  str(dem)

  #' Finding the length of the pattern(lines) in order to decide the number of waypoints.Waypoints
  #' are the points where the UAV will take pictures. The user will decide the number of points according to
  #' the length of the pattern and his personal will. The points might be regular or random and with the sample
  #' every point will keep the coordinates x,y of the line. For this example we created 200 waypoints.
  #' (the more points the better the result). 
  length_pattern=gLength(creepingSLDF)
  waypts=spsample(creepingSLDF,200,type ="regular")

  #' Creation of data.frame with the values of DEM to every waypoint and zero flight height. We used the "extract"
  #' to get the values of DEM at the points.
  data=data.frame((extract(dem,waypts)),z=0)
  names(data) [1] ="dem_values"

  #' Creation of SpatialPointsDataFrame with the coordinates and the values of elevation of every waypoint.
  waypoints=SpatialPointsDataFrame(waypts,data)

  #' Finding the min/max/mean DEM values of our waypoints. Is useful for the user to know those statistics of
  #' altitude of the waypoints. Statistics will help the user to decide the flight height of the UAV.
  min_height=min(data$dem_values,na.rm = TRUE )
  max_height=max(data$dem_values, na.rm = TRUE)
  mean_height=mean(data$dem_values, na.rm = TRUE)

  #' Setting the flight height of UAV. DEM will help the user to decide the flight height of the UAV. The
  #' flight height can be standard to all waypoints or different (according to the analysis and the
  #' user's will).(For this example the flight-height is 40 meters to areas with elevation <=200m,
  #' 60 meters to areas with elevation <=600m and 100 where the elevation >600m).
  waypoints@data$z <- ifelse(waypoints@data$dem_values<=200, waypoints@data$dem_values+40,ifelse(waypoints@data$dem_values>600,waypoints@data$dem_values+100,waypoints@data$dem_values+60))

  #' The goal from every use of UAV is to cover the greatest possible area. To calculate the coverage
  #' of our pattern we need to create a spatial zone (buffer). The buffer will be around our pattern so we need
  #' to know the distance between our legs an the initial step to decide the size of our zone. Our goal is
  #' to cover all the area and not having gaps between our legs. To this example the size of the buffer is half the value
  #' of the initial step (0.015 degrees).
  range_cover=gBuffer(creepingSLDF, width=0.015)
  plot(range_cover)
  plot(creepingSLDF,add=T)

  #' Another important statistic is the coverage of our pattern. Every user's goal is to cover the greatest possible area,
  #' so we need to find the area covered by the use of our UAV. In order to plot our area we have to
  #' rasterize it and plot it in KML through Google Earth. 
  area_coverage=gArea(range_cover)
  r=raster(range_cover)
  cover_raster=rasterize(range_cover,r)
  plotKML(cover_raster)
  #' Plot waypoints in KML through Google Earth. Plotting in KML is an importatnt step because the user
  #' has the opportunity to see the pattern "in real" and in 3 dimensions. It is useful to plot the coverage
  #' with the waypoints to see all the area covered by the pattern. With the use of Google Earth we can see the
  #' variance between flight heights in "real space" . The waypoints are Yellow and the size of them differ 
  #' depending on the flight height(big flight height->big points).For the areas with no DEM values and no
  #' flight heights the waypoints are white (Nan).
  plotKML(waypoints, colour_scale="#FFFF00", "creeping_line")

  if (toexport){
    #' Write the SpatialLinesDataFrame to shapefile.
    writeOGR(creepingSLDF, "path",layer="creeping line", driver="ESRI Shapefile", overwrite_layer = T)
  }

}

# Call the function and set the parameters
creeping_line_pattern(area, n=10, initial_step=0.03, toplot=T, toexport=T )




