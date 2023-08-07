## These functions take a matrix and finds and caches its inverse.
## If the inverse is requested and has already been computed, the
## cached inverse is returned. Otherwise, the inverse is computed

## Takes a matrix as input, outputs a list of 4 functions to get and 
## set the matrix and its inverse. Note: does NOT compute inverse,
## simply assigns inverse variable and does so using the <<-
## operator so that the variable is accessible outside of the 
## function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## First checks if the inverse of the matrix specified in 
## makeCacheMatrix has already been computed. If so, returned cached
## value; if not, computes inverse and assigns it to inverse variable
## (and returns inverse)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
