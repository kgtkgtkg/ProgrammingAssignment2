## Coursera. John Hopkins University. R Programming
## Programming Assignment 2

## Assignment: Caching the Inverse of a Matrix

## Write the following functions:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()){
  m.inv <- NULL
  set <- function(y) {
    x <<- y
    m.inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m.inv <<- inverse
  getinverse <- function() m.inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x,...){
  m.inv <- x$getinverse()
  if(!is.null(m.inv)) {
    message("getting cached data")
    return(m.inv)
  }
  matrix <- x$get()
  m.inv <- solve(matrix, ...)
  x$setinverse(m.inv)
  m.inv
}