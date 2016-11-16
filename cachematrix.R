## These functions provide the ability to cache the inversion of a matrix
## to reduce processing time when multiple accesses will be required.

## makeCacheMatrix provides an interface which supports storing a matrix
## and its inverted version.  Variable 'i' is used to store the inverted version
## the 'set' function allows the matrix stored within the makeCacheMatrix to be 
## replaced by a new matrix as well.  The 'set' function also NULLs out the 
## inverted version so that a new inversion of the currently set matrix can be created
## on the next call.  The 'get' function returns the base matrix (not inverted).  The
## "set_inverse" function performs the inversion via 'solve'
## The 'get_inverse' function returns the inverse of the base matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) i <<- solve
  get_inverse <- function() i
  
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)

}


## cacheSolve takes a variable of type makeCacheMatrix as its parameter as well as a list
## of additional parameters.  It obtains the inverted matrix via the 'get_inverse' function.
## If the variable i is not NULL then the cached inverted matrix is returned.  If it is NULL
## then the base matrix is retrieved from the makeCacheMatrix object, the inversion is performed 
## via 'solve' and then the inverted version of the matrix is set back into the makeCacheMatrix
## object via the ' set_inverse' function.  Then 'i' (the inverted matrix) is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$get_inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}
