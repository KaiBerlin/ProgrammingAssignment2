## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix - Prepares a matrix for caching of the inverse operation
## Usage: diag(x)
## - x is a matrix
## return the matrix with cache

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve - inverts a matrix, using the cached inverted matrix
## if this exists
## Usage: cacheSolve(x, ...)
## - x   is a matrix converted into a cacheMatrix by makeCacheMatrix
## - ... further arguments to the solve function
## returns the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iv <- x$getinverse()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setinverse(iv)
  iv
}
