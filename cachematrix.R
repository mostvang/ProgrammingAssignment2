## Matrix conversion is a costly computation. These function will cache the inverse of a matrix to reduce this cost. 
## 

## This is a function that will create a "matrix" object than can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                           
  
  set <- function(y) {           
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function (inverse) inv <<- inverse
  getinverse <- function () inv
  
  list (set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}




## This function will compute the inverse of the matrix returned by makeCacheMatrix. 
## First it checks if it has already been computed. If so, this already computed 
## matrix is returned. 

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


