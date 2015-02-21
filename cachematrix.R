## These functions will allow a user to create a matrix, solve for the inverse of the matrix, and cache
## the inverse solution for future use to avoid redundant, time-consuming calculations


## Function that creates a list of functions to: set value of matrix, get value of matrix
## , set value of the matrix inverse, get value of the matrix inverse. This is to be used
## with the function 'cacheSolve' below

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Function to compute the inverse of a matrix and cache it for future use. 
## If the inverse has already been computed, this function will retrieve the cached solution.
## This is to be used with the function 'makeCacheMatrix'.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}