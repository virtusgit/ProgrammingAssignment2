## These functions, when used together, create a special matrix, to store the 
## value of its inverse. 
## Usage: m <- makeCacheMatrix( matrix)
##        inv <- cacheSolve(m)

## This function defines the functions set, get, setinverse, and getinverse 
## used to interact with the special matrix. It returns them as a list.

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL               # clears parameter m 
            set <- function(y) {
                  x <<- y           # sets new matrix value 
                  m <<- NULL        # clears cache
            }
            get <- function() x     # gives value of object
            
            # sets value of inverse
            setinverse <- function(inverse) m <<- inverse 
            
            #gives value of inverse
            getinverse <- function() m
            
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## This function first looks in memory if special matrix build with 
## makeCacheMatrix already exists, if it does not
## it computes, and cashes, the inverse of the passed matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()    # looks in cache for inverse
      if(!is.null(m)) {
            message("getting cached data")
            return(m)        # if cache was not empty return inverse
      }
      data <- x$get()        # otherwise, get matrix
      m <- solve(data, ...)  # find inverse
      x$setinverse(m)        # cache inverse
      m                      # return inverse
}
