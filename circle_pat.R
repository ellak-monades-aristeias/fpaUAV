#' Cirlce pattern
#' ===

#' φόρτωση όλων των βιβλιοθηκών που θα χρειασθούν
library(TurtleGraphics)
library(maptools)
library(rgeos)
library(rgdal)
library(sp)

#' Ορισμός περιοχής μελέτης μέσα από τη δημιουργία bounding box. Το bounding box που δημιουργήθηκε
#' στο συγκεκριμένο παράδειγμα βρίσκεται σε μορφή kml, ώστε να είναι εύχρηστο. Εισαγωγή του bounding box
#' στο RStudio από το σημείο που βρίσκεται αποθηκευμένο (directory)
area=readOGR("bbox.kml",layer="bbox")

#' Αρχικά γίνεται ορισμός του προβολικού συστήματος του αρχείου. Σε περιπτώσεις που τα αρχεία βρίσκονται
#' σε μορφή kml το βήμα αυτό μπορεί να παραλειφθεί όμως γενικά ο εκάστοτε χρήστης θα πρέπει πριν ξεκινήσει
#' την ανάλυση να αποφασίσει και να δηλώσει το προβολικό σύστημα των αρχείων στο οποίο επιθυμεί να εργασθεί.
proj4string(area) = CRS("+proj=longlat +datum=WGS84 +no_defs")

#' Προτού ξεκινήσει η ανάλυση, προτείνεται στον χρήστη η γνωριμία με το μοτίβο που θα αναπτυχθεί και
#' τα χαρκτηριστικά αυτού. Το συγκεκριμένο μοτίβο χαρακτηρίζεται από ομόκεντρους κύκλους. Το πλήθος και
#' το μέγεθος αυτών ορίζεται από τον χρήστη.
#' browseURL("http://mathworld.wolfram.com/ConcentricCircles.html")



#' Εύρεση και ορισμός σημείου έναρξης πτήσης. Για το συγκεκριμένο μοτίβο το σημείο έναρξης πτήσης ορίζεται
#' ως το κεντροειδές της περιοχής μελέτης. Εύρεση του κεντροειδούς του bounding box. Ορισμός συντεταγμένων
#' αυτού και απεικόνιση αυτού στην περιοχή μελέτης. το κεντροειδές για αυτό το μοτίβο, αποτελεί το κοινό
#' σημείο όλων των κύκλων.
pointcenter = gCentroid(area,byid=FALSE)
proj4string(pointcenter) = CRS("+proj=longlat +datum=WGS84 +no_defs")
plot(pointcenter)
plot(area,add=T)
x_first=26.32701
y_first=39.14581

#' Δημιουργία κενής λίστας για τη μετέπειτα αποθήκευση των συντεταγμένων του μοτίβου. Για το συγκεκριμένο
#' μοτίβο σημαντικό βήμα είναι ο ορισμός της ακτίνας του κύκλου. Η ατκίνα ορίζεται από τον χρήστη ανάλογα
#' με την εφαρμογή στην οποία στοχεύει (π.χ εύρεση ναυαγών στο θαλάσσιο χώρο), την περιοχή μελέτης και τα
#' χαρακτηριστικά της μη επανδρωμένης πλατφόρμας(εύρος κάμερας). Στη συγκεκριμένη περίπτωση η ακτίνα ορίζεται
#' στις 0.02 μοίρες. Το πλήθος των ομόκεντρων κύκλων ορίζεται στους 10.
cir_list = list()
initial_radius= 0.5
n=2

#' Ορισμός διαστάσεων περιοχής μελέτης και εγκατάσταση του turtle από το πακέτο TurtleGraphics στο
#' σημείο έναρξης πτήσης όπως έχει ορισθεί. Σε πριτπώσεις όπου το μοτίβο θα βγαίνει εκτός της οριζόμενης
#' περιοχής μελέτης, θα κόβεται στα όρια του bounding box.
turtle_init()
turtle_setpos(x=26.32701,y=39.14581)

for (i in 1:n){
  for(j in 1:90){
    if(i==1){
      step=initial_radius
      x_start=x_first
      y_start=y_first
    }else{
      x_start=x_end
      y_start=y_end
    }
    if(i!=1){
      step=initial_radius+0.5

    }
    turtle_forward(dist=step)
    turtle_right(angle=4)



  }
  y_end=turtle_getpos()[[2]]
  x_end=turtle_getpos()[[1]]
  cir_list[[i]]=c(y_start,x_start, y_end,x_end)

}

range_cir = range(unlist(cir_list))

for (k in 1:n){
  leg=cir_list[[k]]

}
#' Δημιουργία data.frame με τις συντεταγμένες των legs. Ορισμός στηλών και labels αυτών.
cirdf = data.frame(matrix(unlist(cir_list), ncol=4, byrow=T))
names(cirdf)=c("y_from", "x_from", "y_to", "x_to")

#' Δημιουργία γραμμών των legs μέσα από το data.frame που δημιουργήθηκε.
circles = vector("list", nrow(cirdf))
for (i in seq_along(circles)) {
  circles[[i]] <- Lines(list(Line(rbind(c(cirdf$x_from[i],cirdf$y_from[i]),  c(cirdf$x_to[i], cirdf$y_to[i]) ))), as.character(i))
}

#' Μετατροπή των γρμμών που δημιουργήθηκαν σε simple SpatialLines
spcirc = SpatialLines(circles)
proj4string(spcirc) = CRS("+proj=longlat +datum=WGS84 +no_defs")

#' Μετατροπή από simple SpatialLines σε SpatialLinesDataFrame
spcircdf = SpatialLinesDataFrame(spcirc,cirdf)
proj4string(spcircdf) = CRS("+proj=longlat +datum=WGS84 +no_defs")

plot(spcircdf)

writeOGR(spcircdf, "path",layer="concentric", driver="ESRI Shapefile", overwrite_layer = T)


