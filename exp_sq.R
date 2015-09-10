
#' Expanding Square pattern
#' ===


#' Ορισμός χώρου εργασίας (directory)
#setwd("C:/cba_drones")

#' Φόρτωση όλων των βιβλιοθηκών που θα χρειασθούν στον κώδικα
library(TurtleGraphics)
library(maptools)
library(rgeos)
library(rgdal)
library(sp)

#' Ορισμός περιοχής μελέτης μέσα από τη δημιουργία bounding box. Αφού δημιουργηθεί το bounding box 
#' γίνεται αποθήκευση στο directory που ορίσθηκε αρχικά και έπειτα εισάγεται στο Rstudio. Το bounding box είναι σε 
#' μορφή kml ώστε να είναι εύκολη η χρήση του. 
area=readOGR("Data/bbox.kml",layer="bbox")
#plot(area)

#' πρώτο βήμα είναι ο ορισμός του προβολικού συστήματος. Στη συγκεκριμένη περίπτωση χρησιμοποιείται
#' το WGS84. Αφού ορισθεί το προβολικό γίνεται απεικονίζεται η περιοχή μελέτης. Ο ορισμός του προβολικού συστήματος 
#' δεν είναι αναγκαίος σε περιπτώσεις που τα αρχεία είναι της μορφής kml, όμως ο εκάστοτε χρήστης θα πρέπει 
#' θα πρέπει ανάλογα με τη μορφή των αρχείων του να ορίζει το σύστημα στο οποίο θα πραγματοποιηθεί η ανάλυσή του. 
proj4string(area) = CRS("+proj=longlat +datum=WGS84 +no_defs")


pattern_expanding_info <- function(){
  #' Ένα σημαντικό βήμα προτού ξεκινήσει η ανάλυση είναι η γνωρισμία με το εκάστοτε μοτίβο και η κατανόηση των 
  #' χαρακτηριστικών του. Για αυτό το λόγο, γίνεται η χρήση urls μέσα από την R. 
  browseURL("http://www.navdynamic.com.au/p_navigator-search-rescue-square.html")
  
}


pattern_expanding <- function(area, n=10, initial_step=0.03, toplot=T, toexport=T ){
  
  #' Για την εκτέλεση του συγκεκριμένου μοτίβου (όπως φαίνεται και από το σχήμα του),η εκάστοτε μη επανδρωμένη
  #' πλατφόρμα θα ξεκινάει την πτήση από το κέντρο της περιοχής μελέτης που έχει ορίσει ο χρήστης. Για αυτό 
  #' το λόγο δημιουργείται το κεντροειδές του bounding box της περιοχής μελέτης. Και για αυτό το σημείο 
  #' ορίζεται το ίδιο προβολικό σύστημα ενώ γίνεται απεικόνιση αυτού σε συνδυασμό με το bounding box.
  pointcenter = gCentroid(area,byid=FALSE)
  proj4string(pointcenter) = CRS("+proj=longlat +datum=WGS84 +no_defs")
  plot(pointcenter)
  plot(area,add=T)
  
  #' Oρισμός των συντεταγμένων του κεντροειδούς (του σημείου έναρξης)
  x_first=26.3
  y_first=39.1
  
  #' δημιουργία κενής λίστας όπου σε επόμενο στάδιο θα αποθηκευθούν τα ευθύγραμμα τμήματα που θα ακολουθήσει
  #' το uav (τα legs). Σε αυτή την περίπτωση ο αριθμός των legs που επιλέχθηκαν να δημιουργηθούν είναι n=10.
  #' Χαρακτηριστικό του μοτίβου είναι πως το μήκος των legs αυξάνει ανά δύο. Το αρχικό μήκος ορίζεται στις 
  #' 0.03 μοίρες.
  leg_list = list()
  #n=10
  #initial_step=0.03
  
  #' Χρήση του πακέτου TurtleGraphics για την εκτέλεση του μοτίβου.Ορισμός των διαστάσεων του bounding box
  #' όπου θα κινηθεί το turtle. σε περιπτώσεις όπου το μοτίβο ξεπερνά τα όρια του bounding box,θα περιορίζεται 
  #' (θα κόβεται) αυτόματα στα όρια αυτού.Εγκατάσταση του turtle στο σημείο έναρξης βάση συντεταγμένων. 
  turtle_init(width=0.173,height=0.211, mode = c("clip"))
  turtle_setpos(x_first,y_first)
  
  #' Για τη δημιουργία των legs αρχικά γίνεται έλεγχος για να βρεθεί το leg στο οποίο βρίσκεται το UAV. 
  #' Εάν βρίσκεται στο πρώτο τότε ως αρχή ορίζεται το σημείο με συντεταγμένες αυτές του κεντροειδούς και 
  #' μήκος το αρχικό step(0.03 μοίρες). Σε κάθε άλλη περίπτωση ως αρχικό σημείο του leg ορίζεται το τελικό 
  #' σημείο του προηγούμενου. Επίσης γίνεται έλεγχος ώστε ανά δύο legs να γίνεται αύξηση του βήματος βάση 
  #' του αρχικού. Ουσιαστικά το βήμα ανά δύο legs θα διπλασιάζεται. Η πορεία που θα ακολουθεί 
  #' το turtle στη συγκεκριμένη περίπτωση είναι σταθερή. Θα προχωρά ευθεία ανάλογα με το μήκος των legs και 
  #' όλες οι στροφές που θα πραγματοποιεί θα είναι δεξιόστροφες των 90 μοιρών. Όλες οι συντεταγμένες των 
  #' αρχικών και τελικών σημείων των legs θα κρατώνται και θα αποθηκεύονται στη λίστα που έχει δημιουργηθεί
  #' σε προηγούμενο στάδιο. Με τη δημιουργία των legs, γίνεται unlist της λίστας που αποθηκεύθηκαν οι 
  #' συντεταγμένες και βάση του εύρους γίνεται η απεικόνιση του μοτίβου. Ορίζονται τα στοιχεία των αξόνων 
  #' καθώς και των legs(μέγεθος,χρώματα).
  for (i in 1:n){

    if (i ==1){
      x_start = x_first
      y_start = y_first
      step = initial_step
    }else{
      x_start = x_end 
      y_start = y_end 
    }
    
    #' Έλεγχος για να δούμε σε ποιο leg βρίσκεται η επανδρωμένη πλατφόρμα. Ο έλεγχος γίνεται ώστε να αυξάνει το μήκος
    #' των legs κάθε δύο. η αύξηση αυτή δεν είναι αυθαίρετη αλλά βασίζεται στη μορφή του συγκεκριμένου μοτίβου. Η αύξηση
    #' του μήκους ανά δύο legs σχετίζεται με το αρχικό βήμα που ορίζει ο χρήστης. Ουσιατικά ανά δύο ευθύγραμμα τμήματα το 
    #' μήκος αυξάνει κατά το αρχικό βήμα. 
    if(i%%2 == 1){
      step =   step + 5
    } 
    
    turtle_forward(dist=step)
    turtle_right(angle=90)
    
    y_end=turtle_getpos()[[2]]
    x_end=turtle_getpos()[[1]]
    leg_list[[i]]=c(y_start,x_start, y_end,x_end)
  }
  
  
  #' Σύμφωνα με το εύρος των τιμών των συντεταγμένων των legs, γίνεται unlist της λίσταςμε σκοπό τη μετέπειτα 
  #' δημιουργία γραμμών του μοτίβου. Ορισμός χαρακτηριστικών των αξόνων x-y καθώς και των σημείων γραμμών που θα 
  #' αναπαρασταθούν. 
  range_exp = range(unlist(leg_list))
  #plot(0, 0, type = "n", xlab="x", ylab="y", ylim=range_exp, xlim=range_exp)
  for (k in 1:n){
    leg=leg_list[[k]]
    #points(leg[1],leg[2], type="p", pch=16)
    #segments(leg[1],leg[2],leg[3],leg[4], col=sample(rainbow(40)), lwd=2  )
  }
  #points(leg[3],leg[4], type="p", pch=23, col="red")
  
  #' Δημιουργία data.frame με τις συντεταγμένες των legs. Ορισμός στηλών και labels αυτών. 
  linesdf = data.frame(matrix(unlist(leg_list), ncol=4, byrow=T))
  names(linesdf)=c("y_from", "x_from", "y_to", "x_to")
  
  #' Δημιουργία γραμμών των legs μέσα από το data.frame που δημιουργήθηκε.
  lines = vector("list", nrow(linesdf))
  library(sp)
  for (i in seq_along(lines)) {
    lines[[i]] <- Lines(list(Line(rbind(c(linesdf$x_from[i],linesdf$y_from[i]),  c(linesdf$x_to[i], linesdf$y_to[i]) ))), as.character(i))
  }
  
  #' Μετατροπή των γρμμών που δημιουργήθηκαν σε simple SpatialLines
  splines = SpatialLines(lines)
  proj4string(splines) = CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  #' Μετατροπή από simple SpatialLines σε SpatialLinesDataFrame
  splinesdf = SpatialLinesDataFrame(splines,linesdf)
  proj4string(splinesdf) = CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  if (toplot){ 
    plot(splinesdf)
  }
  
  #' Αποθήκευση του SpatialLinesDataFrame σε shapefile.
  if (toexport){ 
    writeOGR(splinesdf, "path",layer="expanding square", driver="ESRI Shapefile", overwrite_layer = T)
  }

  
  
} # End of function "pattern_expanding"





# Testing
pattern_expanding(area, n=25, initial_step=0.03, toplot=T, toexport=T )







