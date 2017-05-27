## cacheMatrix. 
## The following functions accomplish 2 objects, 1) solve for the matrix 
## inverse of the matrix inputted and 2) stores this value to avoid having
## to recompute this value whenever it is necessary.


## This function creates the cache matrix, storing the original matrix
## and two getter/setter pairs which allow the user to get/set the matrix
## and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL # Necessary to ensure a previously stored inverse is not used.
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function, when called, will first check if the provided cacheMatrix in
## the argument already has had its inverse calculated. If so, it will return
## this value; if not it will solve for the matrix inverse, set the inverse
## for the cacheMatrix and return the matrix inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinverse() # Gets cached value for x's matrix inverse
  
  # If matrix inverse has been calculated, m will not be null
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  } 
  # Runs if m Null, calculate's x's matrix inverse and sets the inverse for x.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
