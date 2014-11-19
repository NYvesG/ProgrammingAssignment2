## Put comments here that give an overall description of what your
## functions do

## create matrix
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y											# set x out of the local scope
    s <<- NULL										# make sure there is no s in the global scope
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse		# set the inverse out of the local scope
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function returns the inverse of matrix 'x'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  s <- x$getinverse()
  if(!is.null(s)) {						## the inverse was cached before
    message("getting cached data")
    return(s)
  }
  data <- x$get()						## inverse has to be calculated
  s <- solve(data)						## calculate it
  x$setinverse(s)						## set the inverse
  s
}
