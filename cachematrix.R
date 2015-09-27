## Functions to create a matrix which caches its inverse and calculate the matrix inverse
## 

## makeCacheMatrix is to create a matrix which caches its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <-function() x
  setinverse <- function(matrix_inverse) inverse <<- matrix_inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculate the matrix inverse if matrix inverse was not calculated.
## It returns the cached value if matrix inverse was calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}