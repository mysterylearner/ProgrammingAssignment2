## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## This program caches the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	z <- NULL
      set <- function(z) {
                x <<- y
                z <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) z <<- inverse
      getinverse <- function() z
      list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	y <- x$getinverse()
      if(!is.null(y)) {
      	message("getting cached data")
      	return(y)
      }
      data <- x$get()
      y <- solve(data, ...)
      x$setinverse(y)
      y
      ## Return a matrix that is the inverse of 'x'
	
}
