## These function are used to cache (or compute) the inverse of a matrix

## makeCacheMatrix is used to get and set the inverse value

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
  setinvmatrix <- function(solve) inv <<- solve
  ## get the inverse of a matrix
  getinvmatrix <- function() m
  list(set = set, get = get, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}


## cacheSolve is used to retreive value from cache (if any) or calculate the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinvmatrix()
  ## if the inverse is already in cache, return its value
  if(!is.null(inv)) {
    message("getting matrix inverse")
    return(inv)
  }
  ## otherwise calculate it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinvmatrix(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
