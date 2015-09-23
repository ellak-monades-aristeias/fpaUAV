#' Circle pattern


#' φόρτωση όλων των βιβλιοθηκών που θα χρειασθούν
library(maptools)
library(rgeos)
library(rgdal)
library(sp)

#' Δημιουργία χωρικού σημείου από όπου θα ξεκινησει η πτήση
point=SpatialPoints(cbind(26.24,39.24))

#' Ορισμός εύρους τιμών για τις ζώνες που θα χρησιμοποιηθούν
zones=seq(1,20,1)

#' Δημιουργία κενής λίστας για την μετέπειτα αποθήκευση των στοιχείων του μοτίβου. Ορισμός δείκτη/βήματος
buffers=list()
index=1

#' Δημιουργία χωρικών ζωνών.
for (i in zones){
  buffers[index]=gBuffer(point,byid=TRUE, width=i)
  index=index+1
}

IDs <- sapply(buffers, function(x)slot(slot(x, "polygons")[[1]], "ID"))
length(unique(IDs)) == length(buffers)
spols <- SpatialPolygons(lapply(1:length(buffers), function(i) {
  Pols <- slot(buffers[[i]], "polygons")[[1]]
  slot(Pols, "ID") <- as.character(i)
  Pols
}))

IDsunique <- sapply(buffers, function(x) slot(slot(x, "polygons")[[1]], "ID"))
length(unique(IDsunique)) == length(buffers)

Spols2 <- SpatialPolygons(lapply(buffers,function(x) slot(x, "polygons")[[1]]))



#polygons= lapply(buffers ,function(x) `@`(x ,"polygons"))
#polygons_ind=lapply(unlist(polygons) , function(y) `@`(y,"Polygons"))
#sppolobject=SpatialPolygons(list(Polygons(unlist(polygons_ind) ,ID = 1:20)))

##class(sppolobject)


#splines <- as(sppolobject, "SpatialLines")
#plot(splines)

#' sppolygondf = SpatialPolygonsDataFrame(sppolobject,dt.f)
#' writeOGR(sppolygondf, "path",layer="concentric", driver="ESRI Shapefile", overwrite_layer = T)

#' plot(sppolygondf)
