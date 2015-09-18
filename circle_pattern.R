#' Circle pattern


#' φόρτωση όλων των βιβλιοθηκών που θα χρειασθούν
library(maptools)
library(rgeos)
library(rgdal)
library(sp)

#' Δημιουργία χωρικού σημείου από όπου θα ξεκινησει η πτήση
point=SpatialPoints(cbind(26.24,39.24))

#' Ορισμός εύρους τιμών για τις ζώνες που θα χρησιμοποιηθούν
zones=seq(1,2,0.01)

#' Δημιουργία κενής λίστας για την μετέπειτα αποθήκευση των στοιχείων του μοτίβου. Ορισμός δείκτη/βήματος
buffers=list()
index=1

#' Δημιουργία χωρικών ζωνών.
for (i in zones){
  buffers[index]=gBuffer(point,byid=TRUE, width=i)
  index=index+1
}
polygons= lapply(buffers ,function(x) `@`(x ,"polygons"))
polygons_ind=lapply(unlist(polygons) , function(y) `@`(y,"Polygons"))
sppolobject=SpatialPolygons(list(Polygons(unlist(polygons_ind) ,ID = 1)))

plot(sppolobject)
class(sppolobject)


#' sppolygondf = SpatialPolygonsDataFrame(sppolobject,dt.f)
#' writeOGR(sppolygondf, "path",layer="concentric", driver="ESRI Shapefile", overwrite_layer = T)

#' plot(sppolygondf)
