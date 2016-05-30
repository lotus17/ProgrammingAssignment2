## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  xinv = NULL
  set = function(y) {
    x <<- y
    xinv <<- NULL
  }
  get = function() x
  setxinv = function(inverse) xinv <<- inverse 
  getxinv = function() xinv
  list(set=set, get=get, setxinv=setxinv, getxinv=getxinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xinv = x$getxinv()
  if (!is.null(xinv)){
    message("getting cached data")
    return(xinv)
  }
  data = x$get()
  xinv = solve(data, ...)
  x$setxinv(xinv)
  return(xinv)
}
