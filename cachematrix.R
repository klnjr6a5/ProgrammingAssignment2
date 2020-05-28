## Week 3 assignment 


## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    j <- NULL  
  set <- function(y){      #set the value of the matrix that may overwrite the original matrix
    x <<- y
    j <<- NULL
    
  }
  get <-function() x       #get the value of the original matrix
    setinv <- function(inverse) j <<- inverse
    getinv <- function() j
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)

}


## This function computes the inverse of the matrix returned by 'makeCacheMatrix'
## If the inverse has already been calculated (and the matrix has not changed), 
## the 'cacheSolve' will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        
  j <- x$getinv()   #Return a matrix that is the inverse of 'x'
  
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  data <- x$get()
  j <- solve(data, ...)
  x$setinv(j)
  j
}
