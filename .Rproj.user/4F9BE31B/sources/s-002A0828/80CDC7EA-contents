source('R/functions.R')

#####################
## REAL DATA ----
#####################

# Load data 
data <- read.csv('data/data_clean.csv')
nil <- read.csv('data/nil.csv')
set.seed(1234)

data <- data[sample(1:nrow(data), 1000), ]

# Transform data to generate NN and nn
ps_df <- data %>% 
  group_by(id_nil) %>% 
  summarize(observations=n()) %>% 
  merge(nil, by='id_nil') %>% 
  dplyr::select(-X)



# Gereate nstar for each zita
ps <- postsampling(NN=ps_df$families, 
             nn=ps_df$observations, 
             zita=seq(0, 1, 0.2))
 

for(k in 1:length(ps)){
  cat('Computing model for zita =', ps[[k]]$zita, '\n')
  # Create postsampled dataset
  postsampled <- data.frame()
  ids <- sort(unique(data$id_nil))
  for(i in 1:length(ids)){
    postsampled <- rbind(postsampled, 
                         data[data$id_nil == ids[i],][sample(1:sum(data$id_nil == ids[i]), ps[[k]]$nn[i]),])
  }
  # Generate spatial matrix
  coord  <- cbind(postsampled$lat, postsampled$lon)
  k1 <- knn2nb(knearneigh(coord))
  all.linked <- max(unlist(nbdists(k1, coord)))+0.001
  nb <- spdep::dnearneigh(coord,0,all.linked, longlat=FALSE)
  listw <- nb2listw(nb)
  # Lag model
  ps[[k]]$model <- spatialreg::lagsarlm('price~mtq', postsampled, listw)
  ps[[k]]$impacts <- impacts(ps[[k]]$model, listw = listw)
}

# Create a summary of the results
pstr <- transpose(ps)
results <- data.frame(zeta = unlist(pstr$zita), 
                      loss = unlist(pstr$loss), 
                      repr = unlist(pstr$representativeness), 
                      rho.hat = sapply(pstr$model, function(s) s$rho), 
                      beta.hat = sapply(pstr$model, function(s) s$coefficients[2]),
                      beta.hat.se = sapply(pstr$model, function(s) s$rest.se[2]), 
                      impact_direct = sapply(pstr$impact, function(s) s$direct),
                      impact_indirect = sapply(pstr$impact, function(s) s$indirect), 
                      impact_total = sapply(pstr$impact, function(s) s$total))

#results$beta.hat.sd = results$beta.hat.se*sqrt(nrow(data))

results$mse = (tail(results$beta.hat, 1)-results$beta.hat)^2+results$beta.hat.se^2
results$EL.zeta = (tail(results$beta.hat.se, 1)^2)/results$beta.hat.se^2
results$relative_bias = (tail(results$beta.hat, 1)-results$beta.hat)/tail(results$beta.hat, 1)*100

results
write.table(results, "results.txt")

round(results, 2)
# Visualization
ggplot(data=results) +
 # geom_line(aes(x=zeta, y=loss, color='red')) +
  geom_line(aes(x=zeta, y=mse, color='blue')) +
theme_minimal() +
  theme(
    axis.title.y = element_blank()
  ) +
  scale_color_discrete(labels = c("EL.zeta", "loss"))



# impacts


spatialreg::impacts(ps[[6]]$model, listw = listw)

ps[[6]]


#graphs

library(rgdal)
library(geojsonio)
library(sf)
library(ggmap)
library(maptools)

data_url <- "http://dati.comune.milano.it/dataset/e5a0d956-2eff-454d-b0ea-659cb7b55c0b/resource/af78bd3f-ea45-403a-8882-91cca05087f0/download/nilzone.geojson"
data_file <- "data/nil.geojson"

download.file(data_url, data_file)
data_json <- geojson_read(data_file, what = "sp")

data_json <- fortify(data_json)


ggplot() +
  geom_point(data=data, aes(lon, lat, color = price),  alpha = 1, size=2) +
  geom_path(data = data_json, 
            aes(x = long, y = lat, group = group),
            size = 0.3, color='gray30') +
  scale_colour_gradient(high = "#2e2e2e", low = "#adadad") +
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

ggsave("output/map_milano.png", width=6, height=4.5)
ggsave("output/map_milano.pdf", width=6, height=4.5)


# select one model of the simulation
model <- ps[[4]]$model


# APPENDIX
X <- postsampled$mtq
dim(X) <- c(nrow(postsampled), 1)
s2 <- model$s2
W <- listw2mat(listw)
I<-diag(nrow(postsampled))
rho <- model$rho
beta <- model$coefficients[2]
WiA <- W%*%solve(I-rho*W)
n <- nrow(postsampled)

Ibb <- s2^-2*t(X)%*%X
Ibr <- s2^-2*t(X)%*%WiA%*%X*beta
Ibs <- 0
Irr <- sum(diag((WiA)))^2 +
  n*t(sum(diag((WiA))))%*%sum(diag((WiA)))+
  s2^-2*t(WiA%*%X*beta)%*%(WiA%*%X*beta)
Irs <- sum(diag((n/s2)*((WiA))))
Iss <- n/2*s2^4


I <- matrix(c(Ibb, Ibr, Ibs, Ibr, Irr, Irs, Ibs, Irs, Iss), nrow=3)
solve(I) # :(
