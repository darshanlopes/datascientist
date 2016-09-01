##  Following is a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
  
  ## mtx is a square invertible matrix
  ## returns a list containing functions to
  ##    a. set the matrix
  ##    b. get the matrix
  ##    c. set the inverse
  ##    d. get the inverse

  inverse_m <- NULL
  set <- function(y) {
    mtx <<- y
    inverse_m <<- NULL
  }
  get <- function() mtx
  setinverse<- function(inverse) inverse_m <<-inverse
  getinverse <- function() inverse_m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## This function computes the inverse of the matrix returned by above function
## If the inverse has already been calculated then it would retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
        ## Return a matrix that is the inverse of 'mtx'
  
  inverse_m <- mtx$getinverse()
  if(!is.null(inverse_m)) {
    message("Getting cached data...")
    return(inverse_m)
  }
  data <- mtx$get()
  invserse_m <- solve(data, ...)
  mtx$setinverse(inverse_m)
  return(inverse_m)
 
}
