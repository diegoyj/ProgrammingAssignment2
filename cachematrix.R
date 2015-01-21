
# This function creates a special "matrix" object that can cache its inverse
# It has four nested function: set(), get(), setinverse(), getinverse() 
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL  
  set <- function(y) {
    if(!identical(x, y)) {
      x <<- y
      inverse <<- NULL
    }
  }
  get <- function() x
  setinverse <- function(data) inverse <<- data
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
# The function solve() return the inverse of a matrix.
cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse() 
  if(!is.null(inverse)){
    message("Getting cached inverse")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setinverse(inverse)
  inverse
}
