## These functions create a special square matrix object whose
## inverse is cached to prevent time consuming computations.

## makeCacheMatrix creates the matrix object with get() and set()
## functions, as well as setInverse() and getInverse() functions,
##to either set a new inverse value to cache or get the cached value.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setInverse<-function(invx)inv<<-invx
  getInverse<-function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


##cacheSolve calculates the inverse of the special matrix object created by makeCacheMatrix,
##checks if a value for the inverse has already been cached and if so, returns this value

cacheSolve <- function(x, ...) {
        inv<-x$getInverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setInverse(inv)
        inv
}
