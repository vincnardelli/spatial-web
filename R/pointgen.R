pointgen <- function(map, rhos, beta = c(5, 5, 5, 5)){
  pointlist <- list()
  for(i in 1:length(rhos)){
    rho <- rhos[i]
    coord <- map$coord
    w <- map$w
    NN <- map$NN
    N <- sum(NN)
    
    I<-diag(N)
    
    
    
    b1 <- rep(beta, NN)
    
    mu<-10
    x1<-rnorm(N, mu,0.5*mu)
    
    eps<-rnorm(N,0,1)
    
    y<-solve(I-rho*w)%*%(b1*x1+eps)
    w_list<-mat2listw(w)
    
    truemu <- sum(beta*NN)/sum(NN)
    mu <- sum(beta*nn)/sum(nn)
    
    data = data.frame(y, x1, lat=coord[,1], lon=coord[,2])
    pointlist[[i]] <- list(data=data, 
                           w=w_list,
                           NN=NN, 
                           genrho = rho, 
                           nb_df=map$nb_df, 
                           truemu = truemu, 
                           mu = mu)
  }
  
  if(length(rhos) == 1) pointlist <- pointlist[[1]]
  return(pointlist)
}