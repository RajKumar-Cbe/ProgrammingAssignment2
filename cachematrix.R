## The makeCacheMatrix function returns a list containing functions to
## a: set the value of the matrix
## b: fetch the value of the matrix
## c: set the inverse of the matrix
## d: get the inverse of the matrix

## The function creates a special matrix object that can cache its inverse.
## We are assuming that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    # The `<<-` is used to assign a value to an object in an env 
    # that is different from the current env. 
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invmat <<- inverse 
  getinv <- function() invmat
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## The CacheSolve function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed then the CacheSolve
## should retrieve the inverse from the cache. 
## In this function we are using the solve function to return the inverse of the matrix given.

cacheSolve <- function(x, ...) {  
  invmat <- x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(invmat)){
    # get it from the cache and return. 
    message("Getting cached data.")
    return(invmat)
  }
  
  # otherwise, compute the inverse 
  matdata <- x$get()
  invmat <- solve(matdata, ...)
  
  # sets the value of the inverse in the cache by calling the setinv function.
  x$setinv(invmat)
  
  return(invmat)
}

