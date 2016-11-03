## Code results in functions mackeCacheMatrix and cacheSolve which cache the inverse of a matrix


## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolv <- function(solve) m <<- solve
  getsolv<- function() m
  list(set = set, get = get,
       setsolv = setsolv,
       getsolv = getsolv)
}


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolv(m)
  m
}