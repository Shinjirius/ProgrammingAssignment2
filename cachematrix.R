## 'makeCacheMatrix' is a function that creates a matrix that can store both is own data and its inverse.
## 'cacheSolve' is a function that checkes a CacheMatrix, return its inverse. It calculates the inverse if needed and stores it in the CacheMatrix.

## This function is used to create a matrix that can store its own inverse.
## It creates a lexical scope that hold two variables: 
## The x which holds  matrix and the i whih can hold its inverse.
## This function returns a list with four functions:
## The 'get' and 'set' functions to get and set the matrix
## The 'getinverse' and 'setinverse' functions to get and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function gets the inverse of the cacheMatrix that is passed as its parameter.
## It calls the getinverse() function of cacheMatrix.
##
## If the result of the this function is not NULL, the inverse was cached in the cacheMatrix.
## This result is then returned using the return() fuction, which end the execution of the cacheSolve function.
## 
## Otherwise, the get function is used on the cacheMatrix to get the matrix.
## Then the solve function is called to get the inverse of this matrix.
## This inverse is stored in the cacheMatrix using the setinverse method.
## the inverse is then returned.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
