# R Programming Assignment 2: Lexical Scoping
# Matrix inversion is usually a costly computation and their may be some 
# benefit to caching the inverse of a matrix rather than compute it 
# repeatedly (there are also alternatives to matrix inversion that we will 
# not discuss here). Your assignment is to write a pair of functions 
# that cache the inverse of a matrix.

# makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse. Specifically, it is a function that returns a 
# list of functions. It contains the following functions:
# setMatrix     store a matrix
# getMatrix     return the stored matrix
# setInverse    store the inverse of the matrix
# getInverse    return the cached inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
  
  # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  matrix <- NULL
  
  # store a matrix
  setMatrix <- function(y) {
    x <<- y
    # since the matrix is assigned a new value, flush the cache
    matrix <<- NULL
  }
  
  # return the stored matrix
  getMatrix <- function() {
    x
  }
  
  # store the inverse of the matrix 
  setInverse <- function(solve) {
    matrix <<- solve
  }
  
  # return the cached inverse of the matrix
  getInverse <- function() {
    matrix
  }
  
  # return a list of named functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  # get the cached value
  matrix <- x$getInverse()
  # if a cached value exists return it
  if(!is.null(matrix)) {
    message("getting cached data")
    return(matrix)
  }
  # otherwise get the matrix, caclulate the inverse and store it in the cache
  data <- x$getMatrix()
  matrix <- solve(data)
  x$setInverse(matrix)
  
  # return the cached inverse of the matrix
  matrix
}
