## There are two functions written. The first one creates the matrix 
## and calculates the inverse of the matrix
## The follow up one also calculates inverse but will consider if 
## the inversion has already happened in prior function

## This function creates a "matrix" object. Solve function is embedded inside to invert the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  matrix(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function computes the inverse of the "matrix" returned by makeCacheMatrix above. 
## If the inverse is already calculated in makeCacheMatrix, 
## then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
