#' Circle pattern
#' ===
#' 
#' Author:
#' Date:
#' Organization: 
#' Description: 


#' Loading libraries
library(maptools)
library(rgeos)
library(rgdal)
library(sp)
library(raster)
library(plotKML)

#' Create functions to open links. Through links users can see the characteristics of the pattern and get
#' the Digital Elevation Model. Users must select the appropriate DEM and save it (to directory).
meet_the_pattern<- function(){
  browseURL("http://mathworld.wolfram.com/ConcentricCircles.html")
}

get_your_dem <- function(){
  browseURL("http://srtm.csi.cgiar.org/SELECTION/inputCoord.asp")
}

#' Create function for Circle pattern
circle_pattern<-function(zones=seq(0.006,0.03,0.006),toplot=T, toexport=T){
  #' Create a start point for the pattern . For this pattern the user has to set the coordinate of the starting point.
  #' The starting point will be the center point for all circles. In fact the user will create  concentric circles.
  #' The concentrics circles will be the result of multiple buffers/spatial zones. The user has to set the number and the
  #' size of zones(according to the use and the size of the area of interest). The size of the zones defined by the radius of
  #' circles. The outer zones have bigger radius than the inside zones. For this example we will create 5 concentric
  #' circles with initial radius/step 0.006 degrees. It is important to set the projection (WGS'84).
  point=SpatialPoints(cbind(26.32701,39.14581))
  proj4string(point) = CRS("+proj=longlat +datum=WGS84 +no_defs")

  #' Create an empty list to store later our zones.
  #' Create an index for our loop to automatically change the zone.
  buffers=list()
  index=1

  #' Create buffer zones. Set the number and size of zones according to initial step/radius. Every circle polygon
  #' from  the loop will have the same center point (concentric spatial polygons).
  for (i in zones){
    buffers[index]=gBuffer(point,byid=TRUE, width=i)
    index=index+1
  }

  #' Checking the IDs of our polygons. Every ID must be unique and the lenght of IDs must be the same with the length of
  #' our polygons. The ID is the idendity of every polygon.
  IDs <- sapply(buffers, function(x)slot(slot(x, "polygons")[[1]], "ID"))
  length(unique(IDs)) == length(buffers)

  #' Create SpatialPolygons from our list. Set unique IDs to polygons.
  spols <- SpatialPolygons(lapply(1:length(buffers), function(i) {
    Pols <- slot(buffers[[i]], "polygons")[[1]]
    slot(Pols, "ID") <- as.character(i)
    Pols
  }))

  #' Create function to extract the coordinates of polygons according to the edges.
  getEdges <- function(x) {
    stopifnot(class(x) == "SpatialPolygons")
    lapply(x@polygons, function(y) {
      y@Polygons[[1]]@coords
    })
  }

  #' Create line from the the first polygon. Getting only the edges of the polygon in order to create
  #' a circle.
  initialline = Line(getEdges(buffers[[1]]))
  initiallines = Lines(list(initialline), ID="1")

  #' Create an empty list sto store the lines/circles.
  list_of_lines=list()
  list_of_lines[[1]]=initiallines

  #' Create a loop to get the edges of every spatial polygon. Store the lines/circles to the list.
  for (i in 2:5){
    my_lines = Line(getEdges(buffers[[i]]))
    my_lines_ids = Lines(list(my_lines), ID=as.character(i))
    list_of_lines[[i]]=my_lines_ids
  }

  #' Create SpatialLines from the lines of our list.
  splines = SpatialLines(list_of_lines)

  #' Create data.frame with the coordinates of our lines.
  data=as.data.frame(coordinates(my_lines_ids))
  names(data) [1]="x"
  names(data) [2]="y"

  #' Create SpatialLinesDataFrame with concentric circles.
  spcirdf = SpatialLinesDataFrame(splines,data)
  plot(spcirdf)

  #' Loading the DEM and plotting it. DEM is a raster file, so we need the raster package.
  dem=raster("Data\dem_lesvos.tif")
  plot(dem)

  #' Finding the length of the pattern(lines) in order to decide the number of waypoints.Waypoints
  #' are the points where the UAV will take pictures. The user will decide the number of points according to
  #' the length of the pattern and his personal will. The points might be regular or random and with the sample
  #' every point will keep the coordinates x,y of the line. For this example we created 1000 waypoints. Plotting the waypoints.
  length_pattern=gLength(spcirdf)
  waypts_circle=spsample(spcirdf,1000,type ="regular")
  plot(waypts_circle)

  #' Creation of data.frame with the values of DEM to every waypoint and zero flight height. We used the "extract" 
  #' to get the values of DEM at the points.
  data=data.frame((extract(dem,waypts_circle)),z=0)
  names(data) [1] ="dem_values"

  #' Creation of SpatialPointsDataFrame with the coordinates and the values of elevation of every waypoint.
  waypoints_circle=SpatialPointsDataFrame(waypts_circle,data)

  #' Finding the min/max/mean DEM values of our waypoints. Is useful for the user to know those statistics of
  #' altitude of the waypoints. Statistics will help the user to decide the flight height of the UAV.
  min_height=min(data$dem_values,na.rm = TRUE )
  max_height=max(data$dem_values, na.rm = TRUE)
  mean_height=mean(data$dem_values, na.rm = TRUE)

  #' Setting the flight height of UAV. DEM will help the user to decide the flight height of the UAV. The
  #' flight height can be standard to all waypoints or different (according to the analysis and the
  #' user's will).(For this example the flight-height is 40 meters to areas with elevation  <=200m,
  #' 60 meters to areas with elevation  <=300m and 100 where the elevation  >300m).
  waypoints_circle@data$z <- ifelse(waypoints_circle@data$dem_values<=200, waypoints_circle@data$dem_values+40,ifelse(waypoints_circle@data$dem_values>300,waypoints_circle@data$dem_values+100,waypoints_circle@data$dem_values+60))

  #' The goal from every use of UAV is to cover the greatest possible area. To calculate the coverage
  #' of our pattern we need to create a spatial zone (buffer). The buffer will be around our pattern so we need
  #' to know the distance between our legs an the initial step to decide the size of our zone. Our goal is
  #' to cover all the area and not having gaps between our legs. To this example the size of the buffer is 0.006 degrees.
  range_cover=gBuffer(spcirdf, width=0.006)
  plot(range_cover)
  plot(spcirdf,add=T)

  #' Another important statistic is the coverage of our pattern. Every user's goal is to cover the greatest possible area,
  #' so we need to find the area covered by the use of our UAV. In order to plot our area we have to
  #' rasterize it and plot it in KML through Google Earth. 
  area_coverage=gArea(range_cover)
  r=raster(range_cover)
  cover_raster=rasterize(range_cover,r)
  proj4string(cover_raster) = CRS("+proj=longlat +datum=WGS84 +no_defs")
  plotKML(cover_raster)

  #' Plot waypoints in KML through Google Earth. Plotting in KML is an importatnt step because the user
  #' has the opportunity to see the pattern "in real" and in 3 dimensions. It is useful to plot the coverage
  #' with the waypoints to see all the area covered by the pattern. With the use of Google Earth we can see the
  #' variance between flight heights in "real space" . The waypoints are Yellow and the size of them differ 
  #' depending on the flight height(big flight height->big points).For the areas with no DEM values and no
  #' flight heights the waypoints are white (Nan).
  proj4string(waypoints_circle) = CRS("+proj=longlat +datum=WGS84 +no_defs")
  plotKML(waypoints_circle,colour_scale="#FFFF00", "circle_pattern")

  #' Write the SpatialLinesDataFrame to shapefile.
  writeOGR(spcirdf, "path",layer="concentric", driver="ESRI Shapefile", overwrite_layer = T)

}

#' Call the function and set the parameters
circle_pattern(zones=seq(0.006,0.03,0.006),toplot=T, toexport=T)


