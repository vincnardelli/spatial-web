samplegen <- function(point, ps, wmat="d", matrix=NULL, lag=T){
  nn <- ps$nn
  data <- point$data
  NN <- point$NN
  index <- list(1:NN[1], NN[1]+1:NN[2], NN[1]+NN[2]+1:NN[3], NN[1]+NN[2]+NN[3]+1:NN[4])
  
  index_sample<- list()
  for(i in 1:4){
    index_sample[[i]] <- sample(index[[i]], nn[i])
  }
  data_sample <- data[unlist(index_sample),]
  coord  <- cbind(data_sample$lat, data_sample$lon)
  
  
  if(wmat=="d"){   # same weight matrix
    k1 <- knn2nb(knearneigh(coord))
    all.linked <- max(unlist(nbdists(k1, coord)))+0.001
    nb<-spdep::dnearneigh(coord,0,all.linked, longlat=FALSE)
    w_list<-nb2listw(nb)
  }else if(wmat=="k"){  # different weight matrix
    k4 <- knn2nb(knearneigh(coord, 4))
    w_list<-nb2listw(k4)
  }else if(wmat == "i"){ # inverse distance
    mat <- 1/as.matrix(dist(coord))
    diag(mat) <- 0
    w_list <- mat2listw(mat, style="W")
  }
  
  if(lag){
    model <- lagsarlm('y~x1-1', data_sample, w_list)
  }else{
    model <- errorsarlm('y~x1-1', data_sample, w_list)
  }
  r <- list(genrho = point$genrho, NN=NN, ps=ps,
            model = model)
  return(r)
}