## Caching the Inverse of a Matrix
## Matrix inversion in some cases is computational expensive.
## This program cache the result of a Matrix inversion to avoid
## the repeative computation. For any cache miss, it will conduct
## the calculation, otherwise it will read the cached value.


## This function creates a special "matrix" object that caches the inverse

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(InputMatrix) invMatrix <<- InputMatrix
  getInverse <- function() invMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function caches the inverse of the matrix object 'x' generated
## 'makeCacheMatrix' function. If 'x' has the inverse calculated already,
## it will read the cached value. If not, this function will do the matrix 
## inversion operation for 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
