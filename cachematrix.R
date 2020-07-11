##                     Calculate Inverse matrix


##In this Programming Assignment we will take advantage of the scoping rules of
##the R language and how they can be manipulated to preserve state inside
##of an R object.


##Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse of a matrix rather than computing it
##repeatedly


##  Two Function will be created:
##
##  1.  `makeCacheMatrix`: This function creates a special "matrix" object
##  that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
    {
      inv <- NULL
      set <- function(y) 
        {
          x <<- y
          inv <<- NULL
        }
      get <- function() x
      setInvrs <- function(invrs) inv <<- invrs
      getInvrs <- function() inv
      list(set = set, get = get, setInvrs = setInvrs, getInvrs = getInvrs)
    }

##  2.  `cacheSolve`: This function computes the inverse of the special
##  "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then
##  `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
    {
      inv <- x$getInvrs()
      if (!is.null(inv))
        {
          message("getting cached data")
          return(inv)
        }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInvrs(inv)
      inv
    }

##Most of the code was done using an example delivered in the beginning of
##the assignation