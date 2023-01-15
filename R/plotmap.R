plotmap <- function(point){
  
  data <- point[[1]]$data
  w <- point$w
  
  
  coord <- cbind(data$lat, data$lon)
  k1 <- knn2nb(knearneigh(coord))
  all.linked <- max(unlist(nbdists(k1, coord)))
  nb<-spdep::dnearneigh(coord,0,0.5, longlat=FALSE)
  w<-spdep::nb2mat(nb)
  
  nb_df <- nb2df(nb, coord)
  
  ggplot() +
    geom_segment(aes(x=x, xend=xend, y=y, yend=yend),
                 colour = "gray80", 
                 size=0.05,
                 data=nb_df) +
    geom_point(data=data, aes(lat, lon, color="colour")) +
    theme_minimal() +
    scale_colour_manual(values="#56B1F7") +
    ggtitle(paste0("Simulated map")) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text=element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 16))
}
