simulation & inference of transients
================
Carl Boettiger
5/30/2019

``` r
library(tidyverse)
library(furrr)
set.seed(12345)
```

``` r
p <- list(r = .05, K = 2, Q = 5, H = .38, sigma = .02, a=0.023, N = 3e3, x0 = 0.2)
growth <- function(x, p) x * p$r * (1 - x / p$K)
consumption <- function(x, p) p$a * x ^ p$Q / (x^p$Q + p$H^p$Q)
```

``` r
theory <- 
  function(p){
    tibble(x= seq(0,2, length.out = 100)) %>%
    mutate(growth = growth(x,p), 
           consumption = consumption(x,p)) %>%
    mutate(potential = - cumsum(growth - consumption)) %>%
    gather(curve, y, -x, -potential) 
}
```

``` r
theory(p) %>%
  ggplot(aes(x, y, col=curve)) +
  geom_line(lwd=1)
```

![](ghost-sims_files/figure-gfm/noisy_switch-1.png)<!-- -->

``` r
theory(p) %>%
  ggplot(aes(x, potential)) + 
  geom_line(lwd=1)
```

![](ghost-sims_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
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
```

``` r
det <- det_sim(may,p) 
det %>% ggplot(aes(t, x)) + geom_line() 
```

![](ghost-sims_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
a <- c(0.0225, 0.0230, 0.02305, 0.0231)
names(a) <- a

df <- map_dfr(a, function(a){
  p$a <- a
  p$N <- 1e4
  det_sim(may, p)
},
.id = "a")


df %>% ggplot(aes(t,x, col = a)) + geom_line() + coord_cartesian(xlim=c(0,1000))
```

![](ghost-sims_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
theory(p) %>% ggplot(aes(x, y, col=curve)) + geom_line(lwd=1)
```

![](ghost-sims_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
plan("multisession")
system.time({
  df <- future_map_dfr(1:500, function(reps) stochastic_sim(may, p), .id = "reps")
})
```

    ##    user  system elapsed 
    ##   0.252   0.022   6.388

``` r
det <- det_sim(may,p) %>% mutate(reps=1)
mean <- df %>% group_by(t) %>% summarise(x = mean(x)) %>% mutate(reps = 1)
```

## Stochastic ensemble mean vs deterministic mean

``` r
df %>% ggplot(aes(t, x, group=reps)) + 
  geom_line(alpha=0.05) +
  geom_line(data = det, color = "blue") + 
  geom_line(data = mean, color = "red")
```

![](ghost-sims_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
df %>% write_csv("../data/reps.csv.xz")
```

## Deterministic system with initial conditions in neighborhood below ghost

``` r
lambda <- function(x0, p){
                      pars <- p
                      p$x0 <- x0
                      det_sim(may, p)
                    }
mu <- 0.02
init <- mu + rnorm(100, 0, .02 * mu)
df <- future_map_dfr(init, lambda, p = p, .id = "reps")


df %>% ggplot(aes(t, x, group=reps)) + 
  geom_line(alpha=0.1)
```

![](ghost-sims_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Deterministic System with initial condition in neighborhood of ghost

``` r
mu <- 0.55
init <- mu + rnorm(100, 0, .02 * mu)
df <- future_map_dfr(init, lambda, p = p, .id = "reps")



df %>% ggplot(aes(t, x, group=reps)) + 
  geom_line(alpha=0.1)
```

![](ghost-sims_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
