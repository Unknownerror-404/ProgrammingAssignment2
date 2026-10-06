makeCacheMatrix <- function(x = matrix()) {
  ## Stores the inverse of x.
  ## Initially, no inverse has been calculated.
  m <- NULL
  ## Replaces the stored matrix with y.
  ## The cached inverse must be cleared because it is no longer valid.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Returns the currently stored matrix.
  get <- function() x
  ## Returns the cached inverse, or NULL if none exists.
  setinv <- function(inv) m <<- inv
  ## Returns the cached inverse, or NULL if none exists.
  getinv <- function() m
  ## Returns the accessor functions as a list.
  ## This allows the user to interact with x and m without
  ## directly modifying them.
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {
  ## Retrieve the previously cached inverse.
  m <- x$getinv()
  ## If an inverse already exists, return it without recalculating.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Retrieve the original matrix.
  data <- x$get()
  ## Calculate its inverse.
  ## Additional arguments can be passed to solve() through ...
  m <- solve(data, ...)
  ## Store the newly calculated inverse in the cache.
  x$setinv(m)
  ## Return the inverse.
  m
}
