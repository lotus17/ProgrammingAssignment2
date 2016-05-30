## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  xinv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
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
  
  # if the inverse has already been calculated
  if (!is.null(xinv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(xinv)
  }
  
  # otherwise, calculates the inverse 
  data = x$get()
  xinv = solve(data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setxinv(xinv)
  
  return(xinv)
}
