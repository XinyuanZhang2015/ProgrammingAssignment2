## These functions compute the inverse of the special "matrix" returned by function
## makeCacheMatrix. If the inverse has already been calculated (and the matrix has not 
## changed), then the function cacheSolve should retrieve the inverse from the cache.

## This function contains a list of functions, to achieve setting matrix or getting
## matrix from cached data.

makeCacheMatrix <- function(x = matrix()) {
      mInverse <- NULL
      set <- function(y) {
            x <<- y
            mInverse <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) mInverse <<- inverse
      getInverse <- function() mInverse
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function is to whether the inverse matrix is cached. If so, then returns
## the cached inverse matrix; if not, then calculates the inverse matrix and returns.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if (!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m
}
