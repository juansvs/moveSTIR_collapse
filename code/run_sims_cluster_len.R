
# Register a parallel backend
library(doParallel)
# Detect the number of available cores and create cluster
cl <- makeCluster(length(future::availableWorkers()))
# Activate cluster for foreach library
registerDoParallel(cl)

# Load functions
source("code/functions.R")
# Load functions in each node
clusterEvalQ(cl, source("code/functions.R"))


social <- c(0,0.7, 0.95,1)
nus <- 1/2
gridres <- 5
iterations <- 10

trajs <- (replicate(iterations, lapply(social, \(x) simulate_tracks(tau = 5, steps = 5000, dp = 2, social = x))))

# split each trajectory into different lengths
trajs_split <- list()
for (i in seq_along(trajs)) {
  for (r in c(500, 1000, 2000, 5000)) {
      nm <- paste0(i,"-",r)
      trajs_split[[nm]] <- lapply(trajs[[i]], "[", i = 1:r, j = 1:2)
  }
}

UDout <- foreach(i = seq_along(trajs_split), .packages = c("move","ctmm")) %dopar% {
  list(SIM = gsub("-[[:digit:]]+","",  names(trajs_split))[i],
       STEPS = gsub("[[:digit:]]+-","",  names(trajs_split))[i],
       TRAJS = trajs_split[[i]],
       UDS = getUDs(trajs_split[[i]],dr = gridres))
}

#UDout<-unlist(UDout,recursive=FALSE)

out <- foreach(i = seq_along(UDout), .packages = c("move", "ctmm"), .combine = 'rbind', .inorder = FALSE) %dopar% {
    A <- UDout[[i]]$TRAJS
    UDS <- UDout[[i]]$UDS
    SIM <- UDout[[i]]$SIM
    STEPS <- UDout[[i]]$STEPS
    prods <- getUDprod(UDS)
    # cors <- getCorrs(A, uds[[i]][[2]])
    fois <- getFOI(A, UDS, nu = nus)
    cellarea <- prod(res(fois[[1]][[1]]))
    totareai <- summary(UDS[[2]][[1]])$CI[2]
    totareaj <- summary(UDS[[2]][[2]])$CI[2]
    # ratio of FOI at cells visited by both
    cellsindex <- getValues(fois[[1]]$FAB!=fois[[1]]$FUD)
    ncells <- sum(cellsindex)
    ratio1 <- mean((fois[[1]]$FAB[cellsindex]-fois[[1]]$FUD[cellsindex])/fois[[1]]$FUD[cellsindex])
    ratio2 <- mean((fois[[1]]$FBA[cellsindex]-fois[[1]]$FUD[cellsindex])/fois[[1]]$FUD[cellsindex])
    ratio1sd <- sd((fois[[1]]$FAB[cellsindex]-fois[[1]]$FUD[cellsindex])/fois[[1]]$FUD[cellsindex])
    ratio2sd <- sd((fois[[1]]$FBA[cellsindex]-fois[[1]]$FUD[cellsindex])/fois[[1]]$FUD[cellsindex])
    
    c(SIM,
      STEPS,
      nus, 
      cellarea,
      totareai,
      totareaj,
      cellStats(fois[[1]][[3]], sum), 
      cellStats(fois[[1]][[1]], sum), 
      cellStats(fois[[1]][[2]], sum), 
      overlap(UDS[[2]])$CI[,,2][1,2],
      ncells,
      ratio1,
      ratio2,
      ratio1sd,
      ratio2sd
      )
    
  }
stopCluster(cl)
outdf <- as.data.frame(out)
names(outdf) <- c("sim", "steps", "nu", "Ax","Atoti","Atotj", 
                  "foi_ud", "foi_full1", "foi_full2", "overlap",
                  "ncells", "cellratio1","cellratio2","cellratio1sd", "cellratio2sd")
outdf$social <- rep(rep(social, each=4),iterations)
outname <- paste0("outputs/sim_res_trklen_", format(Sys.time(), "%y%m%d"), ".csv")
write.csv(outdf,outname, quote = F, row.names = F)