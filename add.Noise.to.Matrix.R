addNoise <- function(mtx) {
  if (!is.matrix(mtx)) mtx <- matrix(mtx, byrow = TRUE, nrow = 1)
  random.stuff <- matrix(runif(prod(dim(mtx)), min = -0.00001, max = 0.0001), nrow = dim(mtx)[1])
  random.stuff + mtx
}

new.data <- matrix(1:100, nrow = 10)
addNoise(mtx = new.data)

new.data <- matrix(1:10, nrow = 1)
addNoise(mtx = new.data)

library(rbenchmark)
addNoise <- function(mtx) {
  if (!is.matrix(mtx)) mtx <- matrix(mtx, byrow = TRUE, nrow = 1)
  random.stuff <- matrix(runif(prod(dim(mtx)), min = -0.00001, max = 0.0001), nrow = dim(mtx)[1])
  out <- random.stuff + mtx
  out
}

addNoiseReturn <- function(mtx) {
  if (!is.matrix(mtx)) mtx <- matrix(mtx, byrow = TRUE, nrow = 1)
  random.stuff <- matrix(runif(prod(dim(mtx)), min = -0.00001, max = 0.0001), nrow = dim(mtx)[1])
  out <- random.stuff + mtx
  return(out)
}

new.data <- matrix(1:100, nrow = 10)
benchmark(replications = rep(10000, 1),
          noReturn = addNoise(new.data),
          withReturn = addNoiseReturn(new.data))
test replications elapsed relative user.self sys.self user.child sys.child
