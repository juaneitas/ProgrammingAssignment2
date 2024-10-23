makeCacheMatrix <- function(x = matrix()) {
  cache <- new.env()
  set <- function(matrix) {
    x <<- matrix
    cache$inverse <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    cache$inverse <<- inverse
  }
  getInverse <- function() {
    cache$inverse
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

sampleMatrix <- matrix(c(2, 3, 1, 4), nrow = 2, ncol = 2)
cachedMatrix <- makeCacheMatrix(sampleMatrix)
inverseFirst <- cacheSolve(cachedMatrix)
print(inverseFirst)
inverseSecond <- cacheSolve(cachedMatrix)
print(inverseSecond)
