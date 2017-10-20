## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  		   inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
        }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
   }		  

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'		                  
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("Cached Value Result")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
   }		  

##> m <- matrix(rnorm(16),4,4)
##> m1 <- makeCacheMatrix(m)
##> cacheSolve(m1)
##            [,1]       [,2]        [,3]       [,4]
##[1,] -0.03511373 -1.7534681   5.8147270   5.630841
##[2,]  1.10813300 10.8447970 -30.4407204 -32.824078
##[3,] -0.54815818  0.5342709  -0.6703988  -1.526275
##[4,]  0.19980761  2.8224662  -8.0828971  -9.452399
