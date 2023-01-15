mapgen <- function(NN){
  N1 <- NN[1]
  N2 <- NN[2]
  N3 <- NN[3]
  N4 <- NN[4]
  quadrants <- list(
    list(c(0, 0.5), c(0, 0.5)),
    list(c(0, 0.5), c(-0.5, 0)), 
    list(c(-0.5, 0), c(-.5, 0)),
    list(c(-0.5, 0), c(0, 0.5))
    
    
  )
  coord<-c()
  for(i in 1:4){
    xx1 <- runif(NN[i], quadrants[[i]][[1]][1], quadrants[[i]][[1]][2])
    xx2 <- runif(NN[i], quadrants[[i]][[2]][1], quadrants[[i]][[2]][2])
    coord <- rbind(coord, cbind(xx1, xx2))
  }
  k1 <- knn2nb(knearneigh(coord))
  all.linked <- max(unlist(nbdists(k1, coord)))
  nb<-spdep::dnearneigh(coord,0,all.linked, longlat=FALSE)
  w<-spdep::nb2mat(nb)
  return(list(coord=coord, w=w, NN = NN, nb_df = nb2df(nb, coord)))
}