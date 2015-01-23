## Functions described here caches the value of the inverse matrix of a given matrix

## This function makes cache of a matrix and the Inverse of it
## Matrix must be invertible 
makeCacheMatrix <- function(x = matrix()) {

  inverseValue <- NULL   
  
  ## sets the matrix value. In this time, the inverse
  set <- function(y) 
  {
    x <<- y
    inverseValue <<- NULL
  }
  
  ## gets the matrix value
  get <- function()
  {
    x
  }
  
  ## sets the inverse value
  setInverse <- function(inverse) 
  {
    inverseValue <<- inverse
  }  
  
  ## gets the inverse value
  getInverse <- function() 
  {
    inverseValue
  }  
  
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function uses solve function to make the matrix inverse 
## and sets it on the special matrix created by function makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## gets the cached inverse value from Matrix
  inverseValue <- x$getInverse()  
  
  ## if exists, show a message and return this value else compute inverse value and returns it
  
  if(!is.null(m)) 
  {
    message("getting cached data")
    
    return(m)
  }
  
  ## gets matrix value
  data <- x$get()  
  
  ## computes inverse matrix
  m <- solve(data, ...)
  
  ## makes cache 
  x$setInverse(m)  
  
  ## return
  m
  
}
