## These Function calculates the inverse of a invertible matrix
##This function set the matrix and the inverse and you can get them from it

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x                             
  setInverse <- function(inverse) invMatrix <<- inverse  
  getInverse <- function() invMatrix                     
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

##Tis function takes th output of the previous function and returns the solved matrix but before it checks if th invMatrix is not NULL 

cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()        
  if(!is.null(invMatrix)) {                       
    message("Getting Cached Matrix")   
    return(invMatrix)    
  }
  MatrixData <- x$getMatrix()                      
  invMatrix <- solve(MatrixData, ...)            
  x$setInverse(invMatrix)                     
  return(invMatrix)     ## Return a matrix that is the inverse of 'x'
}
