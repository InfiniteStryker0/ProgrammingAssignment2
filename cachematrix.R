## Programming Assignment week 2
## Eric Mukherjee


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  #Return the list
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  
  #First get the inverse
  inv <- x$getinverse()
  
  #If the inverse is not null, then return the inverse
  if(!(is.null(inv))) {
    return(inv)
  } else {
  
  #If its not null, then calculate the inverse
  #First, get the matrix
  data <- x$get()
  
  #and compute its inverse
  inv <- solve(data)
  x$setinverse(inv)

  
  return(inv)
  }
}
