## makeCacheMatrix() is used as an input to cacheSolve(), which calculates an
## inverse of a matrix
## 

## input: square matrix
## return: lest of get and set methods for matrix and its invers
## makeCacheMatrix keeps track of a matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL 
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## input: makeCacheMatrix return: inverse of a matrix The method calculates
## inverse of 'x' and stores it in 'x', if inverse does not exist, otherwise if
## returns cached value of the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$getMatrix()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
}
