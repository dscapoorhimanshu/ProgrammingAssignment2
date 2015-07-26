## makecachematrix creates the cached matrix object
## whose inverse can be caches
##
## cacheSolve function returns the inverse of the matrix




## returns the list of settter, getter for matrix and setter and getter for inverse

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


## returns the inverse of the matrix. In case inverse is not cached
## cached matrix object is created and inverse is saved and if its
## already created inverse is returned from the cached object


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
