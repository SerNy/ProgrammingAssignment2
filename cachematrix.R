## A pair of functions that cache the inverse of a matrix.

##  This function creates a special "matrix" object that can caches its inverse.

makeCacheMatrix <- function(xx = matrix(data_xx, nrow = n, ncol = n)) {
  xx_1 <- NULL
  set <- function(yy) {
    xx <<- yy
    xx_1 <<- NULL
  }
  get <- function() xx
  setsolve <- function(solve) xx_1 <<- solve
  getsolve <- function() xx_1
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function computes the inverse of a special "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(xx, ...) { ## Return a matrix that is the inverse of 'xx'
  xx_1 <- xx$getsolve()
  if(!is.null(xx_1)) {
    message("getting cached data")
    return(xx_1)
  }
  data <- xx$get()
  xx_1 <- solve(data, ...)
  xx$setsolve(xx_1)
  xx_1
}