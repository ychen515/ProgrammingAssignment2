## This is a pair of functions that 
## cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## inverse
  set <- function(y) {
    x <<- y  ## set matrix value
    i <<- NULL 
  }
  get <- function() x  ## getter for matrix value
  setinverse <- function(inverse) i <<- inverse ## setter for inverse
  getinverse <- function() i ## getter for inverse
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse) ## for cacheSolve to reference
}


## computes the inverse of 
## the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  ## if the inverse has been computed&cached
  if (!is.null(i)) {
    message("getting cached data")
    return(i) ## retrieve cached value
  }
  ## otherwise calculate inverse
  data <- x$get() ## retrieve matric value
  i <- solve(data, ...)
  x$setinverse(i) ## cache calculated inverse
  i 
  
}
