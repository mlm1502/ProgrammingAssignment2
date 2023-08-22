## Put comments here that give an overall description of what your
## functions do

## Creates a matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  specialMatrix <- NULL
  set <- function(y) {
    x <<- y
    specialMatrix <<- NULL
  }
  get <- function() x
  setmatrix <- function(mat) specialMatrix <<- mat
  getmatrix <- function() specialMatrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Checks the cache to see if the inverse for this matrix
## has already been calculated. If so, use that. If not,
## calculate the inverse then cache it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  specialMatrix <- x$getmatrix()
  if(!is.null(specialMatrix)) {
    message("getting cached data")
    return(specialMatrix)
  }
  data <- x$get()
  specialMatrix <- solve(x, ...)
  x$setmatrix(specialMatrix)
  specialMatrix
}