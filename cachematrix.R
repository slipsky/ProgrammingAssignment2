## Certain procedures in R can be quite expensive in terms of processing time
## The below functions are designed to run these procs as efficiently as possible
## by saving results of previously-run calculations to a cache, and then being
## to recognize when the cache can be used instead of a calculation.

## The below function takes a matrix of the user's creation and returns a list
## of four elements: set, get, setsolve, getsolve


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This is the function which checks for the cached value; if it does not yet
## exist, it runs the solve function and then caches the result.  If it already
## exists, it simply returns the cached result (and says so via a message)


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
       if(!is.null(m)) {
             message("getting cached data")
             return(m)
         }
       data <- x$get()
       m <- solve(data, ...)
       x$setsolve(m)
       m