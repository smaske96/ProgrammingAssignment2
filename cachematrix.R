## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a special matrix that is actually a list to set
# and get the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## cacheSolve returns inverse of special matrix created by function above
## if the inverse is already calculated, the cached result is return 
## with a message, skipping the inverse calculation

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  m <- x$get()
  inv <- solve(m,...)
  x$setinv(inv)
  inv
}
