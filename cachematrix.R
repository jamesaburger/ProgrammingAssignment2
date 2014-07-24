## Summary:
## Set of matrix functions capable of caching the
## inverse of a matrix and potentially improving
## performance for clients that utilize matrix
## inverses

## Function takes a matrix argument and creates a 
## data structure that can retain the inverse of the 
## matrix until the matrix changes

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    if(!isTRUE(all.equal(x, y)))
    {
      x <<- y
      m <<- NULL
    }
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}

## Function takes a makeCacheMatrix argument and computes the 
## inverse if the matrix property is new/changed or returns the 
## cached inverse otherwise

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x$get'
  m <- x$getsolve()
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
