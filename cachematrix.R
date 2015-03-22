## These functions provide the ability to cache the result of a solve operation on a matrix

## Returns a special matrix with the ability to cache the result of a solve operation
## on the underlying data.
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the matrix inverse
  inv <- NULL
  
  ## Flushes the cache and sets the matrix
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Returns the underlying matrix
  get = function() x
  
  ## Caches the provided inverse in the parent scope
  cacheinverse = function(inv) inv <<- inv
  
  ## Returns the cached inverse of the matrix
  getcachedinverse = function() inv
  
  ## The operations for our special matrix
  list (get = get,
        set = set, 
        cacheinverse = cacheinverse, 
        getcachedinverse = getcachedinverse)
}


## Returns the cached result if available If no value is in the cache
## the function will compute the value and cache the result.
cacheSolve <- function(x, ...) {
  
  ## See if the result is in the cache
  inv <- x$getcachedinverse()
  if (!is.null(inv)) {
    #print ("found result in cache")
    return(inv)
  }
  
  ## The inverse is not in the cache. Get the matrix
  data <- x$get()
  ## Compute the inverse
  inv <- solve(data, ...)
  ## Cache the result
  x$cacheinverse(inv)
  ## Return the result
  inv
}
