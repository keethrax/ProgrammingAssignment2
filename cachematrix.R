## These funcitons allow you to create a matrix-like 
## object, which caches inverses to prevent having to
## perform xpensive recalculation

## This converts a nomral matrix and returns a list of functions
## which allow you to manipulate both the matrix and the cached
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function uses solve to calculate the inverse, getting
## it from the cache if it's already been done, and calculating it 
## otherwise

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
