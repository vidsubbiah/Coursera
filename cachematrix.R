## makeCacheMatrix function creates a matrix "object" that caches the inverse of
## the matrix and gives access to the matrix and it's inverse via get and set
## functions. 
## Arguments : matrix
## Returns : a list of functions

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL # inverse of the matrix                
      set<-function(y){
            x<<-y  # setting passed value using scoping
            inv<<-NULL # resetting inverse
      }
      get<-function() x
      setInverse<-function(solve) inv<<- solve
      getInverse<-function() inv
      list(set=set, get=get,
           setInverse=setInverse,
           getInverse=getInverse)
}

## cacheSolve function computes the inverse of the special "matrix" returned by
## the makeCacheMatrix function above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv<-x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data<-x$get()
      inv<-solve(data, ...)
      x$setInverse(inv)
      inv
}
