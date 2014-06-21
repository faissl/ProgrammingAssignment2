## FUNCTION: Anonymous function to invert a matrix. Due to the processing-intense
##           nature of a matrix inverse, the results will be cached and reused
##           using a semaphore that preserves the state of whether or not the 
##           matrix has been inverted or not. If it has, the currently cached
##           copy of the matrix will be used. 
##
## ARGUMENTS: 
##    x - a lame name for an argument that doesn't descriptively indicate it's 
##        purpose. It is the matrix to be inverted. 
##
## RETURNS:
##    A matrix that is the inverse of the supplied matrix "x". 
## 
## ASSUMPTIONS: The calling routine will need to ensure that the matrix to be
##              inverted HAS NOT been modified since the last call to the 
##              makeCacheMatrix call. 
##
###############################################################################

## The following function is used to set up an instance of a cached matrix
## object with access routines to get and set the matrix, and get and set 
## the matrix inverse. It initially sets the inverse matrix to NULL, indicating
## that it has not been cached. If it is not NULL, the inverse has been computed
## 


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
               x <<- y
               m <<- NULL
           }
    
    get <- function() x
    setInverse <- function(inverseM) m <<- inverseM
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates and returns the inverse matrix of a supplied
## invertible matrix x. It first checks to see if it has been cached within the 
## makeCacheMatrix object's access functions defined above.  

cacheSolve <- function(x, ...) {
  ## Check to see if there is a cached inverse matrix. 
  matrixInverse <- x$getInverse()
  
  if(is.null(matrixInverse)) {
      ## Inverse matrix not cached. Compute the inverse and cache it. 
      data <- x$get()
      matrixInverse <- solve(data, ...)
      x$setInverse(matrixInverse)
      matrixInverse  
  } else {
      ## Inverse matrix cached. Return cached inverse matrix
      message("getting cached matrix inverse data")     
      data <- x$get()
      matrixInverse
  }
  
}
