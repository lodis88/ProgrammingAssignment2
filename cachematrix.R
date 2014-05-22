## This is the solution for Coursera Programming Assingment 2 
## Creates a special "matrix" object that can cache its inverse.
## x is a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
          i  <- NULL
          set  <- function(y){
                    x <<- y
                    i <<- NULL 
            }
          get  <- function() x
          setinverse  <- function(inverse) i  <<- inverse
          getinverse  <- function() i
          list(set= set, get = get, 
                            setinverse = setinverse, 
                            getinverse = getinverse)
  
}

##This function computes the inverse of the matrix returned by above function

cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
            i  <- x$getinverse()
          if (!is.null(i)){
                    message("getting cached data")
                    return(i)
            }
          data  <- x$get()        
          ##solve returns its inverse
          i  <- solve(data, ...)
          x$setinverse(i)
          i
}
