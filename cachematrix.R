## This pair of functions enables us to take a matrix x 
## and enables it to cache its inverse.
## This means we don't need to recompute the inverse if
## we already have, saving computation time by just
## looking up the precomputed inverse if we can.

## The logic of this code closely parallels the scheme used
## in the problem statement, which can be found at
## https://class.coursera.org/rprog-005/human-grading/view
## /courses/972576/assesments/3/submissions.  

## Puts a matrix x in the right form for us to do caching.

makeCacheMatrix <- function(x = matrix()) {
   ## inv stores the inverse of the matrix, or NULL
   ## if it hasn't been computed yet
   inv <- NULL
   ## set lets us update the matrix, and when we do, it
   ## tells us that the inverse of the new matrix has not
   ## been computed yet
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   ## get returns the value of the matrix
   get <- function() x
   ##setinv is how we compute the inverse when we need to.
   setinv <- function(solve) inv <<- solve
   ##getinv gives us the inverse if we already have it
   getinv <- function() inv 
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Actually computes the inverse, using the cache if it can.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv <- x$getinv() ##looks up the inverse
   if(!is.null(inv)) { ##if we have computed it, returns it
      message("getting cached data")
      return(inv)
   }
   else { ##otherwise, we invert the matrix and cache the result.
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
   }
}
