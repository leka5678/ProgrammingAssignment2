## Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse.

## This function creates a special "matrix" object 
## that can cache its inverse. 
## setmat - sets matrix
## getmat - gets matrix
## setinv - sets inverse matrix
## getinv - gets inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmat <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmat <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieve 
## the inverse from the cache. 
##The function itself doesn't check whether the matrix was changed or not.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmat()
  m <- solve(data)
  x$setinv(m)
  m
}
