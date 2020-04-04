## The original solve(a, ...) function, when given a single argument, a,
## is a generic function solves the equation a %*% x = b for x, 
## where b is the identity matrix.  The result returned is an inverse of a.

## In the case of makeCacheMatrix, we are looking to create local overrides
## to the basic internal functions within the solve() function.  By doing so,
## the overridden functions will support our new cached version of solve(), 
## thus allowing us to avoid the added compute needed to invert matrices, 
## if we had already computed such an invertion.
## This approach has been modelled after the makeVector and cacheMean samples.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This new function, cacheSolve, leverages the workings of the solve() function,
## but before executing the matrix inversion associated with a single argument
## call (to solve internally), instead the function will first check in cache,
## to see if we've already performed the inversion of such argument.
## When found in cache, the cacheSolve function will instead use the calculated
## values stored in the cache, and return that value instead.  However, if not
## found in cache, the function will call solve(), and then store the calculated
## matrix value in cache (to use another time) before returning the inverted
## matrix to the caller.

cacheSolve <- function(x, ...) {
  ## cacheSolve will return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
