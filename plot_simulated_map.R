library(ggplot2)
library(dplyr)
library(spdep)

point <- readRDS("simulations/map.rds")

data <- point[[1]]$data
w <- point$w



coord <- cbind(data$lat, data$lon)
k4 <- knn2nb(knearneigh(coord, 4))
k1 <- knn2nb(knearneigh(coord, 1))
all.linked <- max(unlist(nbdists(k1, coord)))
nb<-spdep::dnearneigh(coord,0,all.linked, longlat=FALSE)

w<-spdep::nb2mat(k4)

nb_df <- nb2df(k4, coord)

ggplot() +
  geom_segment(aes(x=x, xend=xend, y=y, yend=yend),
               colour = "gray20", 
               size=0.1,
               data=nb_df) +
  geom_point(data=data, aes(lat, lon, color="colour")) +
  theme_minimal() +
  scale_colour_manual(values="gray60") +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text=element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 16), 
        legend.position = "none") +
annotate(geom="text", x=0.45, y=0.45, label="Q1", size=8)+
  annotate(geom="text", x=-0.45, y=0.45, label="Q2", size=8)+
  annotate(geom="text", x=-0.45, y=-0.45, label="Q3", size=8)+
  annotate(geom="text", x=0.45, y=-0.45, label="Q4", size=8) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)


ggsave("output/map_simulated.pdf", width = 6, height = 6)
ggsave("output/map_simulated.png", width = 6, height = 6)

