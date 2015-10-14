
#' Expanding Square pattern
#' ===
#'
#' Authors:
#' Bika Konstantina,postgraduate student at University of the Aegean
#' Kavroudakis Dimitris, Assistant Professor at University of the Aegean
#' Date:12/10/2015
#' Organization: University of the Aegean, Department of Geography, Lesvos
#' Description: Flight-path Analysis for Unmanned Aerial Vehicles (UAVs)

#' Loading libraries
library(TurtleGraphics)
library(maptools)
library(rgeos)
library(rgdal)
library(sp)

#' Set the area of interest trhough a bounding box. The user has to create a bounding box and save it
#' at the working directory. For the example the bounding box is in KML.
area=readOGR("Data\bbox.kml",layer="bbox")
plot(area)

#' At first the user has to set the coordinate system of the file(files). For the example it's not necessary
#' because the file is in KML. Nevertheless the coordinate system is very important.
proj4string(area) = CRS("+proj=longlat +datum=WGS84 +no_defs")

#' Create functions to open links. Through links users can see the characteristics of the pattern and get
#' the Digital Elevation Model. Users must select the appropriate DEM and save it (to directory).
pattern_expanding_info <- function(){
  browseURL("http://www.navdynamic.com.au/p_navigator-search-rescue-square.html")
}

get_your_dem <- function(){
  browseURL("http://srtm.csi.cgiar.org/SELECTION/inputCoord.asp")
}


#' Create function for Expanding Square pattern
pattern_expanding <- function(area, n=10, initial_step=0.03, toplot=T, toexport=T ){


  #' For this pattern (as seen from the link), the Unmanned Aerial Vehicle will start the flightpath from
  #' the center of the area, set by the user. The user creates the centroid of the bounding box and sets the
  #' coordinate system.
  pointcenter = gCentroid(area,byid=FALSE)
  proj4string(pointcenter) = CRS("+proj=longlat +datum=WGS84 +no_defs")
  plot(pointcenter)
  plot(area,add=T)

  #' Set the coordinates of the centroid.
  x_first=26.32701
  y_first=39.14581

  #' Creation of an empty list. The list will have the coordinates of nodes of the legs of the pattern. The number and the
  #' length of legs defined by the user. The length increases by two legs according to the initial step. For this example the
  #' UAV will create 10 legs with 0.03 (degrees)step.
  leg_list = list()

  #' For this pattern we need the TurtleGraphics package. We set the dimensions of the bounding box in which
  #' the turtle has to move. Setting the turtle to the starting point(centroid). In case the pattern is bigger
  #' than the bounding box with the mode we can set the "clip"  which automatically "cuts" the pattern according to
  #' the bounding box.
  turtle_init(width=0.173,height=0.211, mode = c("clip"))
  turtle_setpos(x_first,y_first)

  #' For the creation of legs we need to know the posistion of the UAV.For the first leg the start point is
  #' the centroid of the area. The start point of the other legs are the end point of the previous leg . Every two
  #' legs there is checking in order to increase the length according to the initial step. The UAV will fly
  #' straight the length and then will turn 90 degrees. The coordinates of start and end points will
  #' be stored in the list. With the end of the pattern we need to unlist the list in order to get the coordinates
  #' and store the legs.
  for (i in 1:n){

    if (i ==1){
      x_start = x_first
      y_start = y_first
      step = initial_step
    }else{
      x_start = x_end
      y_start = y_end
    }
    if(i%%2 == 1){
      step =   step + 0.03
    }

    turtle_forward(dist=step)
    turtle_right(angle=90)

    y_end=turtle_getpos()[[2]]
    x_end=turtle_getpos()[[1]]
    leg_list[[i]]=c(y_start,x_start, y_end,x_end)
  }



  #' According to the range of the coordinates we unlist the list to store our legs/lines.
  range_exp = range(unlist(leg_list))

  for (k in 1:n){
    leg=leg_list[[k]]

  }


  #' Create a data.frame with the coordinates of legs. Setting the labels of columns.
  linesdf = data.frame(matrix(unlist(leg_list), ncol=4, byrow=T))
  names(linesdf)=c("y_from", "x_from", "y_to", "x_to")

  #' Cteate lines/legs from the data.frame.
  lines = vector("list", nrow(linesdf))
  for (i in seq_along(lines)) {
    lines[[i]] <- Lines(list(Line(rbind(c(linesdf$x_from[i],linesdf$y_from[i]),  c(linesdf$x_to[i], linesdf$y_to[i]) ))), as.character(i))
  }

  #' Create simple SpatialLines from the lines
  splines = SpatialLines(lines)
  proj4string(splines) = CRS("+proj=longlat +datum=WGS84 +no_defs")

  #' Create SpatialLinesDataFrame from simple SpatialLines
  splinesdf = SpatialLinesDataFrame(splines,linesdf)
  proj4string(splinesdf) = CRS("+proj=longlat +datum=WGS84 +no_defs")

  if (toplot){
    plot(splinesdf)
  }

  #' Loading the DEM and plotting it. DEM is a raster file, so we need the raster package.
  dem=raster("Data\dem_lesvos.tif")
  plot(dem)

  #' Waypoints are the points where the UAV will take pictures. The user will decide the number of points according to
  #' the length of the pattern and his personal will. The points might be regular or random and with the sample
  #' every point will keep the coordinates x,y of the line. For this example we created 200 waypoints.
  #' (the more points the better the result)
  waypts_creep=spsample(splinesdf,200,type ="regular")

  #' Creation of data.frame with the values of DEM to every waypoint and zero flight height. We used the "extract"
  #' to get the values of DEM at the points.
  data=data.frame((extract(dem,waypts_creep)),z=0)
  names(data) [1] ="dem_values"

  #' Creation of SpatialPointsDataFrame
  waypoints_creep=SpatialPointsDataFrame(waypts_creep,data)

  #' Setting the flight height of UAV. DEM will help the user to decide the flight height of the UAV. The
  #' flight height can be standard to all waypoints or different (according to the analysis and the
  #' user's will).(For this example the flight-height is 40 meters to areas with elevation <=200m,
  #' 60 meters to areas with elevation <=600m and 100 where the elevation >600m).
  waypoints_creep@data$z <- ifelse(waypoints_creep@data$dem_values<=200, waypoints_creep@data$dem_values+40,ifelse(waypoints_creep@data$dem_values>600,waypoints_creep@data$dem_values+100,waypoints_creep@data$dem_values+60))

  #' The goal from every use of UAV is to cover the greatest possible area. To calculate the coverage
  #' of our pattern we need to create a spatial zone (buffer). The buffer will be around our pattern so we need
  #' to know the distance between our legs an the initial step to decide the size of our zone. Our goal is
  #' to cover all the area and not having gaps between our legs. To this example the size of the buffer is the
  #' initial step (0.03 degrees).
  range_cover_creep=gBuffer(splinesdf, width=0.03)
  plot(range_cover_creep)
  plot(splinesdf,add=T)

  #'  In order to plot our area we have to rasterize it and plot it in KML through Google Earth.
  r=raster(range_cover_creep)
  cover_raster=rasterize(range_cover_creep,r)
  plotKML(cover_raster)

  #' Plot waypoints in KML through Google Earth. Plotting in KML is an importatnt step because the user
  #' has the opportunity to see the pattern "in real" and in 3 dimensions. It is useful to plot the coverage
  #' with the waypoints to see all the area covered by the pattern. With the use of Google Earth we can see the
  #' variance between flight heights in "real space" . The waypoints are Yellow and the size of them differ
  #' depending on the flight height(big flight height->big points).For the areas with no DEM values and no
  #' flight heights the waypoints are white (Nan).
  plotKML(waypoints_creep,colour_scale="#FFFF00", "expanding_square")

  #' Write to shapefile.
  if (toexport){
    writeOGR(splinesdf, "path",layer="expanding square", driver="ESRI Shapefile", overwrite_layer = T)
    writeOGR(waypoints_creep, "path",layer="waypts_expanding", driver="ESRI Shapefile", overwrite_layer = T)

  }

}

# Call the function and set the parameters
pattern_expanding(area, n=10, initial_step=0.03, toplot=T, toexport=T )







