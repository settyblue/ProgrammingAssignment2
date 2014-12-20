## These functions will cache the value of a matrix and its inverse
## and will help in avoiding repeated computations of inverse.

## makeCacheMatrix will create a new matrix with ability to store
## its inverse value in a cache, so that it need not be computed 
## everytime.

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


## cacheSolve will compute the inverse of an invertible matrix,
## if it has been already computed before, then it will just 
## retrive this value from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
