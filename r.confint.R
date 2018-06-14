r.confint <- function(r, N, conf.int=0.95){

  # Calculate z for conf.int
  z <- round(qnorm(1 - (1 - conf.int)/2), 2)
  # Calculate z' for given r
  z_p <- 0.5 * log((1+r)/(1-r))
  # Calculate SE for given N
  SE <- 1/sqrt(N-3)          

  # Calculate lower/upper CI from z' followed by conversion back to r
  z.CI.L <- z_p - z*SE
  e <- exp(2*z.CI.L)
  r.CI.L <- (e-1)/(e+1)
  z.CI.U <- z_p + z*SE
  e <- exp(2*z.CI.U)
  r.CI.U <- (e-1)/(e+1)

  return(list(r=r, conf.int=conf.int, N=N, CI.L=r.CI.L, CI.U=r.CI.U))
}
