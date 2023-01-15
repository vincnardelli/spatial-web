# Load packages and functions
source('R/functions.R')


set.seed(1234)
#####################
## SIMULATION ----
#####################

# Input data
NN=c(2000, 2400, 1000, 200)*2
nn=c(70, 30, 150, 20)
beta = c(-1, 2, 1, -2)
rhos = seq(0, 0.8, 0.2)
zita = seq(0, 1, 0.2)


true <- sum(beta*NN)/sum(NN)
nnmu <- sum(beta*nn)/sum(nn)
# # 
# # # Generate the map 
map <- mapgen(NN)
# # 
# # # Generate y in map - return a list with points for each rho  
point <- pointgen(map, beta = beta, rhos=rhos, lag=F)


#-----------
# Compute the postsampling - return a list with ps ratio for each zita 
# and generate the G matrix
ps <- postsampling(NN, nn, zita = zita)
ps

# Compute in parallel the simulation - return a list with all the objects generated
parallel <- psamplegen(500, point, ps, wmat="i", lag=F)

parallel
# the results of this function are saved in the /simulation subfolder
# so it is possible to load all the results without running the simulation
saveRDS(parallel, '1_i_error.rds')

#parallel <- readRDS('simulations/1.rds')

# Extract information from parallel object
paralleltr <- transpose(parallel)
results <- data.frame(rho = sapply(paralleltr$model, function(s) s$lambda), 
                      genrho = unlist(paralleltr$genrho), 
                      # b0 = sapply(paralleltr$model, function(s) s$coefficients[1]), 
                      b1 = sapply(paralleltr$model, function(s) s$coefficients[1]),
                      #  b0se = sapply(paralleltr$model, function(s) s$rest.se[1]), 
                      b1se = sapply(paralleltr$model, function(s) s$rest.se[1]), 
                      #ols = unlist(paralleltr$ols),
                      zita = sapply(paralleltr$ps, function(s) s$zita),
                      n = sapply(transpose(paralleltr$ps)$nn, sum), 
                      N = sum(paralleltr$NN[[1]]),
                      loss = sapply(paralleltr$ps, function(s) s$loss),
                      repres = sapply(paralleltr$ps, function(s) s$representativeness),
                      AIC = sapply(paralleltr$model, function(s) AIC(s)), 
                      LL = sapply(paralleltr$model, function(s) s$LL))
#####################
## ANALYSIS ----
#####################

results %>% 
  group_by(zita, n) %>% 
  filter(row_number(zita) == 1) %>% 
  dplyr::select(zita, n)

results %>% 
  mutate(zita = as.factor(zita), 
         genrho = as.factor(genrho)) %>% 
  ggplot() +
  geom_density(aes(x=b1, color=zita)) +
  geom_vline(xintercept=true,
             linetype="dashed", color = '#cacaca') +
  theme_minimal() +
  facet_grid(. ~ genrho) + 
  xlim(c(true-1, true+1)) +
  ggtitle(paste0("Simulation with different rho"))

results %>% 
  mutate(zita = as.factor(zita)) %>% 
  group_by(genrho, zita) %>% 
  summarize(min=min(b1), max=max(b1), mu = mean(b1)) %>% 
  ggplot() +
  geom_crossbar(aes(x=genrho, y= mu, ymin=min, ymax = max, color=zita)) +
  theme_minimal() +
  facet_grid(. ~ zita)


agg <- results %>% 
  select(-rho) %>% 
  select('rho'=genrho,  everything()) %>% 
  group_by(rho, zita) %>% 
  summarise(bias = round((mean(b1)-true)^2, 1), 
            var=round(mean(b1se)^2, 3), 
            mse = bias+var)

ggplot(agg) +
  geom_line(aes(zita, bias, color='red')) +
  geom_line(aes(zita, var, color='blue')) +
  geom_line(aes(zita, mse, color='black')) +
  theme_bw()+
 # coord_cartesian(ylim=c(0, 1)) +
  theme(
    legend.position="bottom",
    axis.title.y = element_blank(), 
    axis.text.x = element_text(size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
  scale_color_discrete("", labels = c("MSE", "Variance", "Bias^2"))+
  facet_grid(. ~ rho, labeller = labeller(.rows = label_both, .cols = label_both))+
  ggtitle("Simulation") 

print(agg)

