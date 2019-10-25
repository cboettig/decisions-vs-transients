N <- 2^14
system.time(
  replicate(10,{
  matrix(rnorm(N*N), nrow=N, ncol=N) %*%
  matrix(rnorm(N*N), nrow=N, ncol=N)})
  )

  rx <- callr::r(
  function(){
    N <- 2^14
    M <- matrix(rnorm(N*N), nrow=N, ncol=N)
    M %*% M
  },
  env=c(LD_PRELOAD="libnvblas.so"))