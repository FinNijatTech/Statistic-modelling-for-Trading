# inspired by:
# http://www.econ.uiuc.edu/~econ508/R/e-ta7_R.html


grangerTest <- function(d, lags) { 

  require(xts)
  
  # d is a bivariate time-series: 
  #   one needs to regress d[,k] on lags of d[,1] and d[,2]
  names.d <- dimnames(d)[[2]] 
  D <- d 
  for(i in 1:lags) { 
    D <- merge(D, lag.xts(d, i))
  } 
  
  dimnames(D)[[2]] <- paste0(rep(names.d, lags + 1), "_", 
                            rep(0:lags, times = rep(2, lags + 1))) 
  
  x1 <- D[,  -c(1:2)] 
  
  result <- list()
  
  for (k in 1:2) {
    y  <- D[, k] 
    n  <- length(y) 
    x0 <- x1[, ((1:lags) * 2) - (k %% 2)] 
    z1 <- lm(y ~ x1) 
    z0 <- lm(y ~ x0) 
    S1 <- sum(z1$resid^2) 
    S0 <- sum(z0$resid^2) 
    ftest <- ((S0 - S1)/lags)/(S1/(n - 2 * lags - 1)) 
    result[[k]] <- data.frame(direction = paste0(names(d)[k], " <- ", names(d)[3-k]),
                              ftest = ftest, 
                              p.val = 1 - pf(ftest, lags, n - 2 * lags - 1), 
                              R2 = summary(z1)$r.squared) 
  }
  
  return(do.call(rbind.data.frame, result))
} 

