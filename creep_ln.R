#' Creeping Line  pattern
#' ===

#' Ορισμός χώρου εργασίας (directory)

setwd("C:/cba_drones")

#' Φόρτωση όλων των βιβλιοθηκών που θα χρειασθούν για την ανάπτυξη του μοτίβου

library("TurtleGraphics")
library(maptools)
library(rgeos)
library(rgdal)

#' Εισαγωγή του bounding box από το χώρο που είναι αποθηκευμένο. Το bounding box ορίζει την περιοχή μελέτης. 
#' Στη συγκεκριμένη περίπτωση το αρχείο βρίσκεται σε μορφή kml. Ο ορισμός του προβολικού δεν είναι αναγκαίος σε αυτή 
#' την περίπτωση όμως γίνεται ώς παράδειγμα στον εκάστοτε χρήστη που θα διαχειρίζεται άλλη μορφή αρχείων (π.χ shp.).
#' Προβολή της περιοχής μελέτης (bounding box). 

area=readOGR("bbox.kml",layer="bbox")
proj4string(area) = CRS("+proj=longlat +datum=WGS84 +no_defs")
plot(area)

#' Στο πρώτο στάδιο της ανάλυσης είναι χρήσιμο ο εκάστοτε χρήστης να γνωρίσει το μοτίβο που πρόκειται να αναπτύξει.
#' Για να γίνει πιο εύκολη η κατανόηση του μοτίβου προτείνεται η παρουσίαση ενός παραδείγματος αυτού. 

browseURL("http://www.navdynamic.com.au/p_navigator-search-rescue-parallel.html")


#' Σύμφωνα με το μοτίβο, το σημείο έναρξης πτήσης της μη επανδρωμένης πλατφόρμας βρίσκεται στα όρια της περιοχής
#' μελέτης. Για το λόγο αυτό ο χρήστης ορίζει τις συντεταγμένες του σημείου έναρξης πτήσης καθώς και το προβο-
#' λικό σύστημα. Έπειτα γίνεται απεικόνιση του σημείου στην περιοχή ενδιαφέροντος.

point_start=SpatialPoints(cbind(26.24,39.24))
proj4string(point_start) = CRS("+proj=longlat +datum=WGS84 +no_defs")
plot(point_start)
plot(area,add=T)
x_first=26.24
y_first=39.24

#' Δημιουργία μιας κενής λίστας όπου θα αποθηκεύονται οι συντεταγμένες των ευθύγραμμων τμημάτων (legs) που 
#' θα δημιουργηθούν από την εφαρμογή του μοτίβου. Τόσο το πλήθος των legs όσο όσο και μήκος αυτών ορίζεται από
#' τον εκάστοτε χρήστη. Χαρακτηριστικά αυτού του μοτίβου είναι πως το κάθε leg΄έχει το ίδιο μήκος με τα παράλληλα
#' σε ευτο ευθύγραμμα τμήματα. Αυτό σημαίνει οι τιμές του μήκους των ευθύγραμμων τμημάτων είναι συνήθως δύο διαφορετικές
#' και εναλλάσσονται νά ευθύγραμμο τμήμα. Σε αυτό το παράδειγμα το πλήθος των ευθύγραμμων τμημάτων ορίσθηκε στα 8
#' και το αρχικό μήκος (βήμα), στις 0.03 μοίρες(η μονάδα εξαρτάρται από το προβολικό σύστημα)

leg_list = list()
n=8
initial_step=0.03

#' Ορισμός χαρακτηριστικών του bounding box. Δηλώνονται το μήκος και το πλάτος αυτού, ενώ σε περίπτωση που το μοτίβο 
#' βγαίνει εκτός ορίων περιοχής μελέτης (με το mode) θα γίνεται αυτόματα η κοπή αυτού στα όρια του bounding box.
#' Ορισμός θέσης του turtle στο σημείο έναρξης πτήσης.

turtle_init(width=0.173,height=0.211, mode = c("clip"))
turtle_setpos(x_first,y_first)

#' 
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
  if(i==1||i==2||i==5||i==6||i==9||i==10){
    turtle_right(angle=90)
    turtle_forward(dist=step)
  }else{
    turtle_left(angle=90)
    turtle_forward(dist=step)
  }
  
  
  
  
  y_end=turtle_getpos()[[2]]
  x_end=turtle_getpos()[[1]]
  leglist[[i]]=c(y_start,x_start, y_end,x_end)
  
  
}


cr_range = range(unlist(leglist))
plot(0, 0, type = "n", xlab="x", ylab="y", ylim=cr_range, xlim=cr_range)
for (k in 1:n){
  leg=leglist[[k]]
  points(leg[1],leg[2], type="p", pch=16)
  segments(leg[1],leg[2],leg[3],leg[4], col=sample(rainbow(40)), lwd=2  )
}
points(leg[3],leg[4], type="p", pch=23, col="red")
#
creepingdf = data.frame(matrix(unlist(leglist), ncol=4, byrow=T))
names(creepingdf)=c("y_from", "x_from", "y_to", "x_to")

#
lines = vector("list", nrow(creepingdf))
library(sp)
for (i in seq_along(lines)) {
  lines[[i]] <- Lines(list(Line(rbind(c(creepingdf$x_from[i],creepingdf$y_from[i]),  c(creepingdf$x_to[i], creepingdf$y_to[i]) ))), as.character(i))
}

# 
linessp = SpatialLines(lines)
proj4string(linessp) = CRS("+proj=longlat +datum=WGS84 +no_defs")

#
creepingSLDF = SpatialLinesDataFrame(linessp,creepingdf)
proj4string(creepingSLDF) = CRS("+proj=longlat +datum=WGS84 +no_defs")