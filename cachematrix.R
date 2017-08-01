## This function create a special object that store a matrix and cache its inverse 

## This function "makeCacheMatrix" creates a special vector matrix.
## set the value of the matrix,get the value of the matrix
##set the inverse matrix and get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## the function "cachesolve" calculate the inverse of the matrix from the output
## of the above function i.e makeCacheMatrix.
## It first checks if the inverse of the matrix has been already calculated.
## If it is, then it get's the inverse of the matrix from the cache and skip the 
## computation
## else it calculate the inverse of the matrix and set the value of the inverse
## in the cache via the set inverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message ("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve (data,...)
  x$setinverse(inv)
  inv
}

