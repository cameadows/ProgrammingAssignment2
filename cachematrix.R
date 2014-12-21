## Put comments here that give an overall description of what your
## functions do

## this function returns a list of functions of a matrix: 
## "set" sets the value of the matrix
## "get" gets the value of the matrix
## "getinv" gets the value of the inverse
## "setinv" sets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## This function returns the inverse of a matrix
## If the inverse has already been computed, it returns it from the cache
## Otherwise, it computes the inverse
## and stores it in the cache

cacheSolve <- function(x, ...) {
 inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinv(inverse)
  inverse
  
}
