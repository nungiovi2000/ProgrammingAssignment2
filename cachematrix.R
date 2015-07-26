## These function are used to cache (or compute) the inverse of a matrix

## makeCacheMatrix is used to

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the inverse of a matrix
  setinv <- function(solve) inv <<- solve
  ## get the inverse of a matrix
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  ## if the inverse is already in cache, return its value
  if(!is.null(inv)) {
    message("getting matrix inverse")
    return(inv)
  }
  ## otherwise calculate it
  data <- x$get()
  inv <- inv(data, ...)
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
