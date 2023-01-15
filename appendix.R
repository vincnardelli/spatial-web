source('R/functions.R')

#####################
## REAL DATA ----
#####################

# Load data 
data <- read.csv('data/data_clean.csv')
nil <- read.csv('data/nil.csv')

# Transform data to generate NN and nn
ps_df <- data %>% 
  group_by(id_nil) %>% 
  summarize(observations=n()) %>% 
  merge(nil, by='id_nil') %>% 
  dplyr::select(-X)

# Gereate nstar for each zita
ps <- postsampling(NN=ps_df$families, 
             nn=ps_df$observations, 
             zita=0.8)
 

  postsampled <- data.frame()
  ids <- sort(unique(data$id_nil))
  for(i in 1:length(ids)){
    postsampled <- rbind(postsampled, 
                         data[data$id_nil == ids[i],][sample(1:sum(data$id_nil == ids[i]), ps$nn[i]),])
  }
  
  coord  <- cbind(postsampled$lat, postsampled$lon)
  k1 <- knn2nb(knearneigh(coord))
  all.linked <- max(unlist(nbdists(k1, coord)))
  nb <- spdep::dnearneigh(coord,0,all.linked, longlat=FALSE)
  listw <- nb2listw(nb)
  # Lag model
  model <- lagsarlm('price~mtq', postsampled, listw)



X <- postsampled$mtq
n <- length(X)
dim(X) <- c(n, 1)
s2 <- model$s2
W <- listw2mat(listw)
I<-diag(n)
rho <- model$rho
beta <- model$coefficients[2]
WiA <- W%*%solve(I-rho*W)

Ibb <- s2^-2*t(X)%*%X
Ibr <- s2^-2*t(X)%*%WiA%*%X*beta
Ibs <- 0
Irr <- sum(diag((WiA)))^2+
  s2^-1*t(sum(diag((WiA))))%*%sum(diag((WiA)))+
  s2^-2*t(WiA%*%X*beta)%*%(WiA%*%X*beta)
Irs <- (970/s2)*sum(diag(WiA))
Iss <- 1/s2^2

solve(matrix(c(Ibb, Ibr, Ibs, Ibr, Irr, Irs, Ibs, Irs, Iss), nrow=3))




#### lee
X <- postsampled$mtq
n <- length(X)
dim(X) <- c(n, 1)
s2 <- model$s2
W <- listw2mat(listw)
I<-diag(n)
rho <- model$rho
beta <- model$coefficients[2]



s <- I - rho * W
g <- W * solve(s)


s2^-2*t(X)%*%X
s2^-2*t(X)%*%g%*%X%*%beta
0
s2^-2*t(X)%*%g%*%X%*%beta


