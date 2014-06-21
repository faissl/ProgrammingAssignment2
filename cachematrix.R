###############################################################################
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
  mi <- NULL

  set <- function(y = matrix()) {
    x <<- y
    mi <<- matrix()
  }
  
  get <- function() x
            
  setInverse <- function(mInverse){
                   mi <<- mInverse
                }
  getInverse <- function() mi
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
    message("Inverse matrix not cached...computing for matrix:")
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setInverse(matrixInverse)
    return(matrixInverse)
  } else {
    ## Inverse matrix cached. Return cached inverse matrix
    message("Getting cached matrix inverse data")     
    return(matrixInverse)
  }
  
}