## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function will create a special object that has some attributes and methods
## Attributes:
##  * i : actual value for the inverse of the matrix. It is not accesible directly, like object$i
##  * x : actual value for the matrix object.
## Methods:
##  * set : initialize the object by assigning the parsered parameter, and the initial inverse with NULL value
##  * get : retrieve the value of the initial object x=matrix() that was parsered
##  * setinverse : set 'inverse' as the inverse of the object 
##  * getinverse : retrieve the actual value of the inverse (i) 

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



## Write a short comment describing this function
## This function looks for inverse attribute (i) of the special object 'makeCacheMatrix'. Tries to retrieve that value by firstly making a
## consult with the method 'getinverse'. In case that the inverse was previously calculated, this function woun't have to recalculate it again,
## but if the inverse was not calculated, the function retrieves the matrix from the object, calculates the inverse, sets the inverse into the
## objetct and prints the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
