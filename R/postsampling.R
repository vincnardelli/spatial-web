postsampling <- function(NN, nn, zita=1){
  r <- nn/NN
  pi <- mean(r)/r


  pslist <- list()
  for(i in 1:length(zita)){
    l <- ifelse( zita[i] < max(pi), zita[i], max(pi))
    
    pistar <- pi/max(pi)
    pistar[pistar < 1-zita[i]] <- 1-zita[i]
    nstar = round(nn*pistar)
    
    pslist[[i]] <- list(nn=nstar, 
                        pi = pi,
                        loss = 1 - sum(nstar)/sum(nn),
                        representativeness = cor(NN, nstar),      
                        zita=zita[i])
  }
  if(length(zita) == 1) pslist <-  pslist[[1]]
  
  
  return(pslist)
}
