#### FUNCTIONS ####
getProds <- function(x,y) {
  # Check whether the grids overlap/extend to a common grid for both, or all
  r1 <- extend(x, extent(merge(x,y)), value=0)
  r2 <- extend(y, extent(merge(x,y)), value=0)
  # UD and SD products
  udprod <- r1*r2
  sdprod <- sqrt(r1*(1-r1))*sqrt(r2*(1-r2))
  list(UD = udprod, SD = sdprod)
}
# function to get vector of positions (cell index) for two individuals at a
# time. The grid is common to both, obtained from the getProds function
getPositions <- function(X,R) {
  grd <- R$UD
  pos1 <- cellFromXY(grd, xy = as.matrix(X[[1]][,c("x","y")], ncol = 2))
  pos2 <- cellFromXY(grd, xy = as.matrix(X[[2]][,c("x","y")], ncol = 2))
  list(pos1,pos2)
}
# function to get the correlation matrices for a single pair. Outputs are the
# correlation from a->b, from b->a, and the proportion of "significant"
# correlations within the cell, assessed by confidence intervals. There is an
# option to prewhiten, or to estimate CI using bootstrapping
pairCorrs <- function(X, prewt = TRUE, fltr = NULL, export = F) {
  pos1 <- X[[1]]
  pos2 <- X[[2]]
  ovlpcells <- unique(pos1)[unique(pos1) %in% unique(pos2)]
  if(length(ovlpcells)==0) {
    cat("\nThere are no overlap cells")
    NA
  } else {
    nvisits <- matrix(nrow = length(ovlpcells), ncol = 2)
    nsiglags <- numeric(length(ovlpcells))
    nsteps <- length(pos1)
    maxlag <- ceiling(nsteps/2)
    cormat_ab <- cormat_ba <- matrix(0, nrow = maxlag+1, ncol = length(ovlpcells))
    for (j in seq_along(ovlpcells)) {
      cell <- ovlpcells[j]
      a <- b <- numeric(nsteps)
      a[cell==pos1] <- 1
      b[cell==pos2] <- 1
      nvisits[j,] <- c(sum(a),sum(b))
      xcorr <- if(prewt) TSA::prewhiten(a,b,lag.max = maxlag, plot = F)$ccf else ccf(a, b, lag.max = maxlag, plot = F)
      xcorr_vals <- as.numeric(xcorr$acf)
      FILT <- fltr
      if(FILT == "reg") {
        xcorr_vals <- xcorr_vals*(abs(xcorr_vals)>1.96/sqrt(xcorr$n.used))
        nsiglags[j] <- sum(abs(xcorr_vals)>1.96/sqrt(xcorr$n.used))
      } else if(FILT == "bs") {
        BSC <- funtimes::ccf_boot(a,b,lag.max = maxlag)
        xcorr_vals <- BSC$r_P
        siglags <- as.numeric(xcorr_vals<BSC$lower_P | xcorr_vals>BSC$upper_P)
        nsiglags[j] <- sum(siglags)
        xcorr_vals <- xcorr_vals*siglags
      }

      # sigcells[j] <- switch(fltr[1], 
      #                       reg = mean(abs(xcorr_vals)>(1.96/sqrt(xcorr$n.used))),
      #                       bs = corboot(AB = cbind(a,b), cors = xcorr,n = niter))
      cormat_ab[,j] <- xcorr_vals[(maxlag+1):1]
      cormat_ba[,j] <- xcorr_vals[(maxlag+1):length(xcorr_vals)]
    }
    dimnames(cormat_ab) <- dimnames(cormat_ba) <- list(lag = 0:maxlag, cell = ovlpcells)
    dimnames(nvisits) <- list(cell = ovlpcells,NULL)
    # dimnames(nsiglags) <- list(cell=ovlpcells)
    # Export
    if(export) {
      write.csv(cormat_ab, paste0("outputs/correlations_10min_",ids[ind1],"-",ids[ind2],"_",format(Sys.Date(), "%m%d"), ".csv"))
      write.csv(cormat_ba, paste0("outputs/correlations_10min_",ids[ind2],"-",ids[ind1],"_",format(Sys.Date(), "%m%d"), ".csv"))
    }
    return(list(CAB = cormat_ab, CBA = cormat_ba, nvisits = nvisits, nsiglags = nsiglags))
  }
}

#' Interpolate trajectories
#' 
#' @description
#' Interpolate trajectories using a constant time interval.
#' 
#' @param x,y telemetries objects describing the two trajectories
#' @param lag character of the requested time interval. Default is 10 min. Should follow a format that can be interpreted by `seq`
#' 
#' @returns a list with two objects, the two interpolated trajectories (telemtries objects)
interpTrajs <- function(x,y, lag = "10 min") {
  tr1 <- range(x$timestamp)
  tr2 <- range(y$timestamp)
  # Check if there is temporal overlap
  if(max(tr1[1],tr2[1])>min(min(tr1[2],tr2[2]))) {
    cat("There is no temporal overlap \n")
    # foi_ab <- foi_ba <- beta/Area*lam*(1/nu*udprod)
  } else {
    tseq <- seq(max(tr1[1],tr2[1]),min(tr1[2],tr2[2]), lag)
    lags <- as.numeric(tseq-min(tseq))
    nsteps <- length(tseq)
    # Interpolate trajectories
    interp_traj_1 <- predict(telemetries[[ind1]], FITS[[ind1]], t = tseq)
    interp_traj_2 <- predict(telemetries[[ind2]], FITS[[ind2]], t = tseq)
    list(interp_traj_1,interp_traj_2)
  }
}

#' Calculate R_0
#' 
#' @description
#' Calculate R_0 from the FOI pairwise matrix.
#' 
#' @param X numeric matrix. pairwise FOI matrix
#' @param g numeric. gamma parameter representing the recovery rate (units should match the units in FOI, usually 1/s)
#' 
#' @returns a list with the R matrix, the R0 value, the FOI matrix and the gamma diagonal matrix.
calcR0 <- function(X, g) {
  n <- nrow(X)
  U <- diag(-g, nrow = n)
  invU <- solve(-U)
  R <- X%*%invU
  R0 <- max(abs(eigen(R)$values))
  return(list(R = R, R0 = R0, Fmat = X, U = U))
}

# corboot <- function(AB, cors, n = 1000) {
#   maxlag <- (length(cors$acf)-1)/2
#   M <- replicate(n, {
#     a2 <- sample(AB[,1])
#     b2 <- sample(AB[,2])
#     ccf(a2,b2,lag.max = maxlag, plot = F)$acf
#   })
#   Q <- apply(M, 1, quantile,probs = c(0.025, 0.975))
#   mean(cors$acf<Q[1,] | cors$acf>Q[2,])
# }
# calcFOI <- function() {
#   
# }