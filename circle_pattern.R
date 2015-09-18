#' Circle pattern


#' φόρτωση όλων των βιβλιοθηκών που θα χρειασθούν
library(maptools)
library(rgeos)
library(rgdal)
library(sp)

#' Δημιουργία χωρικού σημείου από όπου θα ξεκινησει η πτήση
point=SpatialPoints(cbind(26.24,39.24))

#' Ορισμός εύρους τιμών για τις ζώνες που θα χρησιμοποιηθούν
zones=seq(10,15,5)

#' Δημιουργία κενής λίστας για την μετέπειτα αποθήκευση των στοιχείων του μοτίβου. Ορισμός δείκτη/βήματος
buffers=list()
index=1

#' Δημιουργία χωρικών ζωνών.
for (i in zones){
  buffes[index]=gBuffer(point, width=i)
  index=index+1
}















