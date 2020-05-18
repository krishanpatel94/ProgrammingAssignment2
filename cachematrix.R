## makeCacheMatrix  creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- matrix()
  setMatrix <- function(y = matrix()) {
    x <<- y
    inverse <<- matrix()
  } 
  getMatrix <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
  inverse = x$getInverse()
  if(!is.na(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  matrix = x$getMatrix()
  inverse = solve(matrix)
  x$setInverse(inverse)
  inverse
}
