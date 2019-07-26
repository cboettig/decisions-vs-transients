growth <- function(x, p) x * p$r * (1 - x / p$K)
consumption <- function(x, p) p$a * x ^ p$Q / (x^p$Q + p$H^p$Q)

theory <-
  function(p){
    tibble(x= seq(0,2, length.out = 100)) %>%
      mutate(growth = growth(x,p),
             consumption = consumption(x,p)) %>%
      mutate(potential = - cumsum(growth - consumption)) %>%
      gather(curve, y, -x, -potential)
  }




may <- function(x, p) x + growth(x,p) - consumption(x,p)
#x + x * p$r * (1 - x / p$K)  - p$a * x ^ p$Q / (x ^ p$Q + p$H ^ p$Q)

stochastic_sim <- function(f, p){
  x <- numeric(p$N)
  x[1] <- p$x0
  dBt <- rnorm(p$N, 0, p$sigma)
  for(t in 1:(p$N-1)){
    x[t+1] <- max(
      f(x[t], p) + x[t] * dBt[t],
      0)
  }
  data.frame(t = 1:p$N, x)
}

det_sim <- function(f, p){
  x <- numeric(p$N)
  x[1] <- p$x0
  for(t in 1:(p$N-1)){
    x[t+1] <- f(x[t], p)
  }
  data.frame(t = 1:p$N, x)
}

