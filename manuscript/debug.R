type <- "policy iteration"
n_models <- length(transition)
n_states <- dim(transition[[1]])[1]
n_actions <- dim(transition[[1]])[3]
next_value <- numeric(n_states)
next_policy <- numeric(n_states)
V_model <- array(dim = c(n_states, n_models))
converged <- FALSE
t <- 1
if (is.null(model_prior)) {
  model_prior <- rep(1, n_models)/n_models
}
if (type == "finite time")
  max_iter <- Tmax
while (t < max_iter && converged == FALSE) {
  Q <- array(0, dim = c(n_states, n_actions))
  for (i in 1:n_actions) {
    for (j in 1:n_models) {
      V_model[, j] <- transition[[j]][, , i] %*% next_value
    }
    Q[, i] <- reward[, i] + discount * V_model %*% model_prior
    if(any(is.nan(Q))){
      stop(paste(V_model[, j]))
      #stop(sprintf("t=%i, i=%i,j=%i", t, i, j))
      }
    if(any(is.na(Q))) stop("NAs found")

  }
  value <- apply(Q, 1, max)
  policy <- apply(Q, 1, which.max)
  if (type == "value iteration") {
    if (sum(abs(value - next_value)) < epsilon) {
      converged <- TRUE
    }
  }
  else if (type == "policy iteration") {
    if (sum(abs(policy - next_policy)) < epsilon) {
      converged <- TRUE
    }
  }
  next_value <- value
  next_policy <- policy
  t <- t + 1
  if (t == max_iter)
    message("Note: max number of iterations reached")
}
