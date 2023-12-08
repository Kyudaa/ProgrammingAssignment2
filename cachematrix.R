


## This first function creates a matrix, while caching its inverse, given by the "solve"-Function.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(b) {
    x <<- b
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This second function looks for a cached inverse of the matrix and if not available calculates it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
  ## Return a matrix that is the inverse (i) of 'x'
}
