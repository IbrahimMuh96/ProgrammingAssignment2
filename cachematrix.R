## These Function calculates the inverse of a invertible matrix
##This function set the matrix and the inverse and you can get them from it

makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    a <<- y
  }
  getMatrix <- function() a                             
  setInverse <- function(inverse) inv <<- inverse  
  getInverse <- function() inv                     
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

##Tis function takes th output of the previous function and returns the solved matrix but before it checks if th inv is not NULL 

cacheSolve <- function(a, ...) {
  inv <- a$getInverse()        
  if(!is.null(inv)) {                       
    message("Getting Cached Matrix")   
    return(inv)    
  }
  Data <- a$getMatrix()                      
  inv <- solve(Data, ...)            
  a$setInverse(inv)                     
  return(inv)     ## Return a matrix that is the inverse of 'x'
}
