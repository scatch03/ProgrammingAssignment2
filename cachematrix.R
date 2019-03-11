## Create matrix wrapper implementation for holding  
## cached inverse matrix value which is obtained by relatively
## heavy computing operation

## wrap matrix and lazily-computed inverse matrix value together
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
## where 'x' is obtained by calling  'makeCacheMatrix'
## cached value is returned upon repeated calls
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
