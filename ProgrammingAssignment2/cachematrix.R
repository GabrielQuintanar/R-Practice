## makeCacheMatrix & cacheSolve prepare and cache a matrix trying
## not to compute the inverse very single time

## This function prepare a matrix as an object, with its own variables

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function (y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setInverse <- function (inv) inverse <- inv
      getInverse <- function () inverse
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Try to compute the inverse, but if the inverse is already calculated
## then is returned.

cacheSolve <- function(x, ...) {
            inverse <- x$getInverse()
            if(!is.null(inverse)){
                message("getting cached matrix")
                  return(inverse)
            }
            d <- x$get()
            inverse <- solve(d)
            x$setInverse(inverse)
            inverse
      }