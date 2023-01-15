# Load packages and functions
source('R/functions.R')


set.seed(1234)
#####################
## SIMULATION ----
#####################

# # Input data
NN=c(2000, 2400, 1000, 200)
nn=c(70, 30, 150, 20)
beta = c(-2, 2, -3, 3)
rhos = seq(0, 0.8, 0.2)
zita = seq(0, 1, 0.2)


point <- readRDS('simulations/map.rds')
matrix <- readRDS('simulations/mat.rds')

# Compute the postsampling - return a list with ps ratio for each zita 
# and generate the G matrix
ps <- postsampling(NN, nn, zita = zita)

# Compute in parallel the simulation - return a list with all the objects generated
parallel <- psamplegen(500, point, ps, wmat="i", matrix=matrix)

# the results of this function are saved in the /simulation subfolder
# so it is possible to load all the results without running the simulation
saveRDS(parallel, 'simulations/2_i.rds')