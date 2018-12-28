## A function that takes as an argument an invertible matrix and
## creates a list of functions which can store and retrieve the matrix,
## and calculate, store and retrieve its inverse matrix. 
## A second function which calls these functions to calculate the inverse 
## of the matrix set in the first function or retrieve it if it has
## already been calculated

## returns a list of functions to set and get a matrix and its inverse
## and sets the argument as the matrix initially 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## calls elements of the list returned by makeCacheMatrix to calculate
## the inverse of the matrix set by makeCacheMatrix or makeCacheMatrix$set,
## or retrieve it if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
      data <- x$get()
      inv <- solve(data,...)
      x$setinverse(inv)
      inv
      
}
