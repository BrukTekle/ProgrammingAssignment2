
## This functions calalculate the inverse of a matrix and store that inverse, So that if the inverse 
##has already been calculated it will return that inverse with out calculating it again.

## makeCacheMatrix is a function that accepts a matrix as its argument, it sets and gets 
## the matrix accepted and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## accepts an object of makeCacheMatrix function. check whether the inverse of the matrix 
## in that object has ben computed, if yes it return the inverse else calculate and return
## the inverse

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}

