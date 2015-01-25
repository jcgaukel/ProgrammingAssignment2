# Author:       J.C. Gaukel (jc.github@gaukel.org)
# Date:         January 24, 2015
# Course:       R Programming
# 
# Function:     makeCacheMatrix 
# 
# Description:  This function stores a square matrix so that its inverse can be calculated and cached for future 
#               retrieval.
#
# Parameters :  x - a square matrix 
# 
# Subfunctions: get - returns the current value for x
#               set - stores a new value for x and clears the cached inverse
#               setinverse - used by cacheSolve to cache the inverse of x
#               getinverse - returns the cached value for the inverse of x
# 
# Examples of Usage: 
# 
#    > x <- makeCacheMatrix(matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE))
#    > cacheSolve(x)
#    [,1] [,2]
#    [1,]  0.0    1
#    [2,]  0.5    0
#    > cacheSolve(x)
#    getting cached data
#    [,1] [,2]
#    [1,]  0.0    1
#    [2,]  0.5    0
#    > x$set(matrix(c(1, 5, 6, 0), nrow = 2, ncol = 2, byrow = TRUE))
#    > cacheSolve(x)
#         [,1]        [,2]
#    [1,]  0.0  0.16666667
#    [2,]  0.2 -0.03333333
#    > cacheSolve(x)
#    getting cached data
#         [,1]        [,2]
#    [1,]  0.0  0.16666667
#    [2,]  0.2 -0.03333333
#

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL

  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }

  get <- function() x
  
  setinverse <- function(inverse) invMatrix <<- inverse 
  
  getinverse <- function() invMatrix
  
  list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
    
}

# Author:      J.C. Gaukel (jc.github@gaukel.org)
# Date:        January 24, 2015
# Course:      R Programming
# 
# Function:    cacheSolve 
# 
# Description: returns an inverse of a square matrix and caches the value              
# 
# Parameters:  x - a cached square matrix (see makeCacheMatrix)
# 
# Return:      inverse of x
# 
# Examples of Usage: 
# 
#    see makeCacheMatrix
#    

cacheSolve <- function(x, ...) {

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
