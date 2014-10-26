## Put comments here that give an overall description of what your
## functions do

## as per the assignment , this Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  
  ## Initialize the inverse property to a null value
  
  j <- NULL
  
  ## Method to set the matrix
  
  set <- function( matrix ) {
    m <<- matrix
    j <<- NULL
    
  }
  ## simple Method the get the matrix
  get <- function() {
    
    ## Return the matrix as expected 
    m
  }
  
  ## Method to set the inverse of the matrix
  
  setInverse <- function(inverse) {
    j <<- inverse
  }
  ## next step is Method to get the inverse of the matrix
  
  getInverse <- function() {
    ## Return the inverse property
    j
  }
  ## Return a list of the methods as expected
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix" as per function 1 in assingment 
## If the inverse has already been calculated (and the matrix has not ## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  ## if condition
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
