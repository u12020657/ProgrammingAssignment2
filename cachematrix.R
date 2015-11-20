## The following two functions, makeCacheMatrix and cacheSolve, are used to keep the 
## calculated inverse of a matrix in the cache to increase function efficiency


#----------------------------------------------------------------------------------------

## makeCacheMatrix stores several functions. Assign it to a variable using the matrix
##    as your argument.
##    NOTE: most of the functions are used in the cacheSolve function below. Do not
##          attempt to use the functions outside of cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      
      set <- function(y){
            x <<- y
            inverse <<- NULL
      }
            #sets the input (ie. 'y') to become the matrix we're working with
      
      get         <- function() x
            #gets x, requires no args
      
      setinverse <- function(inv) inverse <<- inv
            #takes inv and sets inverse equal to inv
      
      getinverse <- function() inverse
            #gets inverse, takes no arguments
      
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
            #lists all the sub-functions
}
##---------------------------------------------------------------------------------------


## cacheSolve either fetches the already solved matrix inverse, or it calulates
##    the inverse and saves it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      inverse <- x$getinverse()
      
      if(!is.null(inverse)){
            print("Getting cached inverse")
            return(inverse)
      }
            #if the inverse has already been calculated, simply get it from makeCacheMatrix
      
      
      data <- x$get()
      inverse <- solve(data)
      x$setinverse(inverse)
            #if not yet solved, solve it
      inverse
}

##---------------------------------------------------------------------------------------