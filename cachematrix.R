## Coursera. John Hopkins University. R Programming
## Programming Assignment 2

## Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## This function allows for the functions within it to operate in its own environment.
## The makeCacheMatrix function is called with a constructed matrix, or the desired matrix to be analyzed.
## If saved to an object, it will be saved as data on the global environment


makeCacheMatrix <- function(x = matrix()) {
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


## This function checks if the inverse of the matrix saved above has already been calculated and cached.
## If it has been calculated before and cached, it will retrieve the cached answer (aka the inverse of the matrix above)
## If not, it will calculate the answer and cache it.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
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
