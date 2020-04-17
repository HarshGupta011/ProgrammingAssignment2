## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


##The following function This function creates a special "matrix" 
## object that can cache its inverse. 
##Comments for specific functions are provided right before the defination

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    
    ## Set the matrix to hold a value given by user
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ## Fetch the value of the matrix and return to user
    get <- function(){
      ##returns the matrix
      x
    }
    
    ##Sets the value of inverse and stores it for further use
    setinv <- function(inv){
      m <<- inv
    }
    
    ## Fetch the inverse and display the result
    getinv <- function(){
      m
    }
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.


cacheSolve <- function(x, ...) {
        
  m <- x$getinv()
  
  ## Check if its available and computed before and return if found
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  ## Calculate inverse as cache not found
  m <- solve(data) 
  x$setinv(m)
  m
}
