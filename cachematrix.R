## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## set: set value of matrix
## get: get value of matrix
## setInverse: set inverse of matrix to m
## getInverse: get inverse of matrix m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## takes a list returned by makeCachedMatrix as argument
## returns a cached version of inverse matrix if available
## else calculate the inverse and set it in cached so next time it will return a cached version


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached inverse matrix")
    return (m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}