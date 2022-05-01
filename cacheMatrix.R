## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix consiting of set, get, setinv, getinv
## using > library(MASS) to calculate the inverse for non-squared and squared matrices 
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## Initiallizing the inverse as Null
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
get <- function()x  ## Set the function get matrix x
setinv <- function(inverse)inv <<- inverse 
getinv <- function() {
    inver <- ginv(x)
    inver%*%x  ## Retrieves the inverse of the matrix
  }
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}

## This function is used to retrieve the cached data
cacheSolve <- function(x, ...) {  ## Retrieves the cached data
  inv <- x$getinv()
  if(!is.null(inv)) {  ## Checks if the inverse is Null
    message("Getting cached data")  
    return(inv)  ## Returns the inverse value
  }
data <- x$get()
inv <- solve(data, ...)
x$setinv(inv)
inv  ## Returns a matrix with the inverse of x
}  
  
