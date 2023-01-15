psamplegen <- function(t, point, ps, wmat = "d", matrix=NULL){
  
  
  indexes <- expand.grid(point_index=1:length(point), 
                         ps_index=1:length(ps))
  indexes <- indexes[rep(1:nrow(indexes), each=t), ]
  rownames(indexes) <- 1:nrow(indexes)
  
  cat('\nRunning ', nrow(indexes), 'simulations\n')
  cores <- parallel::detectCores()
  cl <- makeSOCKcluster(cores)
  registerDoSNOW(cl)
  
  pb <- txtProgressBar(min=1, max=nrow(indexes), style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  result <- 
    foreach(i=1:nrow(indexes), .packages="spdep", 
            .export="samplegen", .options.snow=opts) %dopar% {
              samplegen(point[indexes$point_index[i]][[1]], ps=ps[indexes$ps_index[i]][[1]], wmat = wmat, matrix=matrix)
            }
  
  close(pb)
  stopCluster(cl)
  
  saveRDS(result, paste0("simulations/parallel_", as.integer(Sys.time()), ".rds"))
  
  return(result)
}