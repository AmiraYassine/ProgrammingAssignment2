## makeCacheMatrix creates a matrix, which is a list containing a function to:
## set and get the elements of the matrix.
## set and get the elements of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)     
}


## cachesolve calculates the inverse of the matrix created using the above function.
## It first checks to see if the inverse has already been calculated: 
## If yes, it gets the inverse from the cache and skips the computation. 
## If no, it calculates the inverse of the matrix and sets it in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}
