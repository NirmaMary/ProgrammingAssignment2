## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

#function to set the value of the matrix
#funtion get the value of the matrix
#funtion to set the value of the inverse of the matrix
#funtion to get the value of the inverse of thhe matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # initializes the variables to NULL
  Rinvr <- NULL
  # y <- NULL
  
  setM <- function(y)                # sets x to the matrix to cache
  {
    x <<- y            # caches the Matrix argument in to X variable
    Rinvr <<- NULL     # Resets variabe to NULL in case it has changed.
  }
  
  getM <- function() x               # get the cached matrix as passed
  
  setinverse <- function(inverse) Rinvr <<- inverse  #sets the Inverse of Matrix
  
  getinverse <- function() Rinvr     # retrieves the Inverse of the matrix
  
  list(setM= setM, getM = getM,      # list of all funtions possible from this funtion
       setinverse = setinverse,
       getinverse = getinverse)
  
}

#This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.

cacheSolve <- function (x = matrix(), ...) {
  #(x = matrix(), ...)'
  # currM <- x
  Rinvr <- x$getinverse()    # retrieve the inverse cached before.
  
  if(!is.null(Rinvr))        # check if inverse is recovered from cache.
  {
    # oldM <- x$setM() 
    # currM <- x$getM()
    # what if this matrix is different from the cached matrix
    # If(oldM == currM) 
    message("getting cached data") # print that the cache is recovered.
    return(Rinvr)                  # Inverse matrix is returned.
    
  }
  data <- x$getM()           # Get the matrix assigned to this variable.
  
  Rinvr <- solve(data, ...)# Get the inverse using solve funtion.
  
  x$setinverse (Rinvr)       # resets the inverse to the new.
  Rinvr                      # returns the inverse.
  
}
