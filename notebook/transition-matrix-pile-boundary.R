
transition_matrix <- function(states, actions, f, sigma){
  n_s <- length(states)
  n_a <- length(actions)
  transition <- array(0, dim = c(n_s, n_s, n_a))
  for(i in 1:n_a){
    for (k in 1:n_s) {
      nextpop <- f(states[k], actions[i])
      if (nextpop <= 0) {
        #if(which.min(abs(states - nextpop)) > 1) {
        x  <- c(1, rep(0, n_s - 1))
      } else {
        n_s <- length(states)
        mu <- nextpop
        sig <- sigma * nextpop
        x <- dnorm(states, mu, sig)
        if(sum(x) <= 0){
          x  <- c(1, rep(0, n_s - 1))
        } else {
          ## Pile negative density onto boundary
          negs <- pnorm(states[1], mu, sig)
          x[1] <- x[1] + negs
          ## Pile density beyond last state onto other boundary
          N <- pnorm(states[n_s], mu, sig)
          x[n_s] <- x[n_s] + N
          x <- x/sum(x)
        }
      }
      transition[k, , i] <- x
    }
  }
  if(any(is.nan(transition))) stop("error creating transition matrix")
  transition
}