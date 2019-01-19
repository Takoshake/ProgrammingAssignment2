## Matrix inversion is a costly computation. The following two functions will create,
## take the inverse of and cache a special matrix object with the makeCacheMatrix.
## To avoid the costly computation of matrix inversion the cacheSolve function will
## check if the inverted matrix exists in the cache before performing the computation.
## If the inverse matrix exists in the cache the cacheSolve function will return the matrix,
## otherwise it will compute the inverse matrix and then return the inverse matrix.

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
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


## This function computes the inverse of the matrix returned by makeCacheMatrix. The function
## checks if the inverse has previously been calculated and if the matrix has not been changed.
## This function then retrieves the inverse of the matrix from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}