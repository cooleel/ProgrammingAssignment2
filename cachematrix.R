## Caching the inverse of a matrix

## Get a list with four defined functions , set, get, setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y){
                  x <<- y
                  m <<- NULL
          }
          get <- function() x
          set_inverse <- function(solve) m <<- solve
          get_inverse <- function() m
          list(set = set, get = get,
               setinverse = set_inverse,
               getinverse = get_inverse)
}


## to test if the inverse of matrix is already been calulated first instead of
## directly calulate it again

cacheSolve <- function(x, ...) {
          m <- x$getinverse()
          if(!is.null(m)) {
            message("getting cached inverse")
            return (m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
          m
        ## Return a matrix that is the inverse of 'x'
}
