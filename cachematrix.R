makeCacheMatrix <- function(x = matrix()) {
    # store newM value
    newm <- NULL
    set <- function(y) {
        x <<- y
        newm <<- NULL
    }
    # get the original matrix
    get <- function() x
    # set inverse value
    set_inverse <- function(inv) newm <<- inv
    # get inverse value
    get_inverse <- function() newm
    # Return a list
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  newm <- x$get_inverse()
  if(!is.null(newm)) {
     message("getting cached data")
     return(newm)
  }
  data <- x$get()
  newm <- solve(data, ...)
  x$set_inverse(newm)
  newm
}
