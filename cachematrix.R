## The following two functions creates an object that stores a matrix and cache's its inverse

## The following function generates a list containing functions that get/set a matrix and get/set its inverse, 
## which will subsequently be passed on as the argument of the second function (cacheSolve).

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The following function calculates the inverse of the matrix created in the above function. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


