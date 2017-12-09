## The first of two functions used that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.#

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The second function used to get the inverse of the matrix.
## Uses the ouput of the first function as parameter.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  temp <- x$get()
  inv <- solve(temp, ...)
  x$setInverse(inv)
  inv
}
