

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      ## Initialize the inverse property to a null value
            set<-function(y){
                  ## Method to set the matrix
            x<<-y
            m<<-NULL
      }
      get<-function() x
      ## simple Method the get the matrix
      
      setmatrix<-function(solve) m<<- solve
      ## Method to set and get the inverse of the matrix
      getmatrix<-function() m
      ## Return a list of the methods as expected
      list(set=set, get=get,
           setmatrix=setmatrix,
           getmatrix=getmatrix)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix" as per function 1 in assingment 
## If the inverse has already been calculated (and the matrix has not ## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
      ## Return a matrix that is the inverse of 'x'
      
      m<-x$getmatrix()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      ## Get the matrix from our object
            matrix<-x$get()
      m<-solve(matrix, ...)
      ##Return the matrix as expected output
      x$setmatrix(m)
      m
}

