## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # stores the cached value
  # initialize to NULL
  inv <- NULL
  
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
}
  # get the value of the matrix
  get <- function() x
  # invert the matrix and store in cache
  setinverse <- function(inverse) cache <<- inverse
  # get the inverted matrix from cache
  getInverse <- function() cache
  
  # return the created functions to the working environment
  list(set = set, get = get,
       setinverse = setinverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
##cacheSolve retrieves the inverse of a matrix returned 
##by makeCacheMatrix above from the cache that has already been calculated.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data.")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
##################
##test run
x = rbind(c(3, -4), c(-8, 2))
m = makeCacheMatrix(x)
m$get()
