#' Statistics
#' ===
#' Authors:
#' Bika Konstantina,postgraduate student at University of the Aegean
#' Kavroudakis Dimitris, Assistant Professor at University of the Aegean
#' Date:12/10/2015
#' Organization: University of the Aegean, Department of Geography, Lesvos
#' Description: Flight-path Analysis for Unmanned Aerial Vehicles (UAVs)


#' Finding the length of the pattern(lines) in order to decide the number of waypoints.
pattern_expanding= readShapeLines("Data\expanding square.shp",proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs"))
length_pattern_expanding=gLength(pattern_expanding)

pattern_creeping= readShapeLines("Data\creeping line.shp",proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs"))
length_pattern_creeping=gLength(pattern_creeping)

pattern_circle= readShapeLines("Data\circle.shp",proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs"))
length_pattern_circle=gLength(pattern_circle)

#' Finding the min/max/mean DEM values of our waypoints. Is useful for the user to know those statistics of
#' altitude of the waypoints. Statistics will help the user to decide the flight height of the UAV.
#' (where there are no data values are negative)
waypts_expanding=readShapePoints("Data\waypts_expanding.shp",proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs"))
min_height_expanding=min(waypts_expanding@data$dem_values,na.rm = TRUE )
max_height_expanding=max(waypts_expanding@data$dem_values, na.rm = TRUE)
mean_heigh_expandingt=mean(waypts_expanding@data$dem_values, na.rm = TRUE)

waypts_creeping=readShapePoints("Data\waypts_creeping.shp",proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs"))
min_height_creeping=min(waypts_expanding@data$dem_values,na.rm = TRUE )
max_height_creeping=max(waypts_expanding@data$dem_values, na.rm = TRUE)
mean_height_creeping=mean(waypts_expanding@data$dem_values, na.rm = TRUE)

waypts_circle=readShapePoints("Data\waypts_circle.shp",proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs"))
min_height=min(waypts_circle@data$dem_values,na.rm = TRUE )
max_height=max(waypts_circle@data$dem_values, na.rm = TRUE)
mean_height=mean(waypts_circle@data$dem_values, na.rm = TRUE)



