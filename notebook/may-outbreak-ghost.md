
``` r
knitr::opts_chunk$set(fig.width = 7)
```

``` r
library(MDPtoolbox)
library(sarsop) # remotes::install_github("boettiger-lab/sarsop")
library(tidyverse) # for plotting
library(mdplearning)

#if(interactive()) ggplot2::theme_set(ggdark::dark_theme_light())
#ggplot2::theme_set(theme_grey())
#ggdark::invert_geom_defaults()
```

# Outbreak model

  - ‘harvest’ term corresponds to removal of pest, with associated cost
  - also experience damage costs proportional to pest abundance

<!-- end list -->

``` r
damage <- 0.05
control <- 1
reward_fn <- function(x,h) - damage * x ^ 2 - control * h
discount <- 0.98


states <- seq(0,2, length=140)
actions <- states
observations <- states
sigma_g <- 0.05
sigma_m <- 0.0

r <- 0.8
K <- 1.53
q <- 2
b <- .2
eps <- states[2]/10
Tmax <- 100

may <- function(a){  
  function(x, h){ # May
    # x <- pmax(x - h, 0)   # harvest then recruit   
    x + x * r * (1 - x / K)  - a * x ^ q / (x ^ q + b ^ q) + eps - h
  }
}
```

Range of possible a that covers tipping in both directions:

``` r
possible_a <- seq(.25, .34, by = 0.005)
true_a <- 0.27   ## reality has just a transient.  use  0.277 for stronger ghost
believe_a <- 0.295 ## believe there's an attractor
true_i <- which.min(abs(possible_a - true_a))
```

``` r
f <- may(true_a)
tibble(x = states[1:120],
       f = f(x,0) - x) %>%
  ggplot(aes(x, f)) + geom_line() +
  geom_point() +  geom_hline(aes(yintercept = 0))
```

![](may-outbreak-ghost_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
f <- may(believe_a)
tibble(x = states[1:120],
       f = f(x,0) - x) %>%
  ggplot(aes(x, f)) + geom_line() +
  geom_point() +  geom_hline(aes(yintercept = 0))
```

![](may-outbreak-ghost_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
m_true <- fisheries_matrices(states, actions, observations, reward_fn, 
                        may(true_a), sigma_g, sigma_m, noise = "lognormal")


m <- fisheries_matrices(states, actions, observations, reward_fn, 
                        may(believe_a), sigma_g, sigma_m, noise = "lognormal")
```

``` r
soln <- mdp_value_iteration(m$transition, m$reward, discount)
opt_soln <- mdp_value_iteration(m_true$transition, m_true$reward, discount)
```

Policy based on the belief (i.e. that system is bi-stable)

``` r
df <- tibble(state = states,
             action = actions[soln$policy],
             value = soln$V)
df %>% ggplot(aes(state, action)) + geom_point() 
```

![](may-outbreak-ghost_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Optimal policy (knowing it is really just a transient)

``` r
tibble(state = states,
       action = actions[opt_soln$policy]) %>% 
  ggplot(aes(state, action)) + geom_point() 
```

![](may-outbreak-ghost_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
x0 <- which.min(abs(states - 0.1))
```

``` r
no_policy <- numeric(length(states)) + 1

df <- mdp_planning(m_true$transition, m_true$reward, discount, model_prior = c(1), 
                   policy = no_policy, x0 = x0, Tmax = 100)

df %>% mutate(state = states[state], action = actions[action]) %>% 
  ggplot(aes(time, state)) + geom_point()+geom_path() + 
  geom_line(aes(time, action), col="blue")
```

![](may-outbreak-ghost_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Result experienced by incorrect belief: initial in-action followed by
need for continued maintenance to prevent high-level
outbreak:

``` r
df <- mdp_planning(m_true$transition, m_true$reward, discount, model_prior = c(1), 
                   policy = soln$policy, x0 = x0, Tmax = 100)

df %>% mutate(state = states[state], action = actions[action]) %>% 
  ggplot(aes(time, state)) + geom_point()+geom_path() + 
  geom_line(aes(time, action), col="blue")
```

![](may-outbreak-ghost_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Expected result based on the belief: stable low level is acceptable, so
no action is
required:

``` r
df <- mdp_planning(m$transition, m$reward, discount, model_prior = c(1), 
                   policy = soln$policy, x0 = x0, Tmax = 100)

df %>% mutate(state = states[state], action = actions[action]) %>% 
  ggplot(aes(time, state)) + geom_point()+geom_path() + 
  geom_line(aes(time, action), col="blue")
```

![](may-outbreak-ghost_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Optimal strategy knowing this is just a transient (will depend on
discount
rate):

``` r
df <- mdp_planning(m_true$transition, m_true$reward, discount, model_prior = c(1), 
                   policy = opt_soln$policy, x0 = x0, Tmax = 100)

df %>% mutate(state = states[state], action = actions[action]) %>% 
  ggplot(aes(time, state)) + geom_point()+geom_path() + 
  geom_line(aes(time, action), col="blue")
```

![](may-outbreak-ghost_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
models <- map(possible_a, function(a){
  fisheries_matrices(states, actions, observations, reward_fn, 
                     may(a), sigma_g, sigma_m, noise = "lognormal")
})
```

``` r
transition <- lapply(models, `[[`, "transition")
reward <- models[[1]][["reward"]]
```

## a near ghost

``` r
prior <- dnorm(possible_a, believe_a, 0.005)
prior <- prior / sum(prior)
```

``` r
data.frame(a = possible_a, probability = prior) %>%
  ggplot(aes(a,prior)) + geom_bar(stat="identity") +
  geom_vline(aes(xintercept = true_a), col="red", lwd=1, lty=2) + 
  geom_vline(aes(xintercept = believe_a), col="blue", lwd=1, lty=2) 
```

![](may-outbreak-ghost_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
set.seed(12345)
sim <- mdp_learning(transition, reward, discount, 
                    x0 = x0, 
                    Tmax = Tmax, 
                    true_transition = transition[[true_i]],
                    model_prior = prior,
                    type = "value iteration", 
                    epsilon = 1e-2)
```

``` r
 sim$df %>% 
  select(-value) %>% 
  gather(series, state, -time) %>% 
  ggplot(aes(time, states[state], color = series)) + geom_line()
```

![](may-outbreak-ghost_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
 sim$posterior %>% 
  data.frame(time = 1:Tmax) %>%
  filter(time %in% seq(1,Tmax, by = 5)) %>%
  gather(param, probability, -time, factor_key =TRUE) %>% 
  mutate(param = as.numeric(param)) %>% 
  ggplot(aes(param, probability, group = time, alpha = time)) + 
  geom_line()
```

![](may-outbreak-ghost_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
saveRDS(sim, "sim-ghost-learning.Rds")
```
