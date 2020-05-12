# R Programming Week3 
# Programming Assignment 2: lexical scoping assignment
# The functions allow to create an S3 Object that cointain a matrix and caches its inverse. 
# The first function implements the getters and settters methods: get(), set(), setInverse(), getInverse().
# The Second function receive as parameter the object create by the first function and cache (or retrive if already cached) che inverse of the input matrix

## makeCacheMatrix: creates an R object that stores a matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(invMatrix) inverseMatrix <<- invMatrix
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve: pulate and/or retrieve the Inverse Matrix from an object of type makeCacheMatrix()
## The absumption is that the input matrix is invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}


