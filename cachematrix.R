## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly.
## Following two functions used to catch the inverse of a matrix.

##  First fuction makeCacheMatrix creates a special "matrix" object that can cache 
##  its inverse which is a list containing a function to
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of inverse of the matrix
##    4. get the value of inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The seconde function cacheSolve computes the inverse of the special "matrix" 
## returned by above makeCacheMatrix fuction.
## If the inverse has already been computed it gets the result and skips the
## computation. 

## This function assumes that the matrix is always invertible.



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
