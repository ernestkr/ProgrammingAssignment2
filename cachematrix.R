## Construct a "Matrix" object to enable efficient computation of its inverse
## The idea is to compute the inverse only once, and then simply access the cache
## whenever the inverse has been computed.

## Construct an object containing the matrix data passed in the argument
## The return value is a list of four elements, which are the functions defined
## within the object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Using the "Matrix" object created by the makeCacheMatrix function which is
## passed as the argument, this function either computes the inverse of the matrix
## data within the object (and store it into a cache), or access the cache when 
## the inverse has been computed earlier

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
