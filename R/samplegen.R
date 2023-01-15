samplegen <- function(point, ps, wmat="d", matrix=NULL){
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
    all.linked <- max(unlist(nbdists(k1, coord)))
    nb<-spdep::dnearneigh(coord,0,all.linked, longlat=FALSE)
    w_list<-nb2listw(nb)
  }else if(wmat=="k"){  # different weight matrix
    k4 <- knn2nb(knearneigh(coord, 4))
    w_list<-nb2listw(k4)
  }else if(wmat == "i"){ # inverse distance
    k1 <- knn2nb(knearneigh(coord))
    max <- max(unlist(nbdists(k1,coord)))
    w_list <- mat2listw(mat, style="W")
  }

  nb.dist.band <- dnearneigh(coord, 0, Inf)
  distances <- nbdists(nb.dist.band,coord)
  invd1 <- lapply(distances, function(x) (1/x))
  
  
  # r <- list(data=data_sample, w=w_list, genrho = point$genrho, NN=NN, ps=ps, 
  #           model = lagsarlm('y~x1-1', data_sample, w_list), ols = as.numeric(lm('y~x1', data_sample)$coefficients[2]))
  r <- list(genrho = point$genrho, NN=NN, ps=ps,
            model = lagsarlm('y~x1-1', data_sample, w_list))#, ols = as.numeric(lm('y~x1', data_sample)$coefficients[2]))
  return(r)
}