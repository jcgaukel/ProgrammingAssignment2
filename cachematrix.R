## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Author1:      J.C. Gaukel (jc.github@gaukel.org)
# Date:         January 23rd, 2015
# Course:       R Programming
# 
# Function:     makeCacheMatrix 
# 
# Description:  
#              
# 
# Parameters :  x- a square matrix 
# 
# Return:       inverse of x
# 
# Examples of Usage: 
# 
#    
#    
#   
#   
# 
# 

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL

  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }

  get <- function() x
  
  setinverse <- function(inverseMatrix) invMatrix <<- inverse 
  
  getinverse <- function() invMatrix
  
  list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
    
}


## Write a short comment describing this function

# Author1: J.C. Gaukel (jc.github@gaukel.org)
# Date:    January 23rd, 2015
# Course:  R Programming
# 
# Function   : cacheSolve 
# 
# Description: 
#              
# 
# Parameters : x- a square matrix 
# 
# Return     : inverse of x
# 
# Examples of Usage: 
# 
#    
#    
#   
#   
# 
# 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  invMatrix <- x$getinverse()
  
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setinverse(invMatrix)
  invMatrix
  
}
