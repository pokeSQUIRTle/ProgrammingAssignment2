## R-function that is able to cache potentially time consuming computation of
## calculating inverse of a matrix thus eliminating recomputation of same matrix.

## Below R-function returns a list of functions that can be used to compute the
## inverse of a matrix and Cache it.

makeCacheMatrix <- function(x = matrix()) {
                         inv<-NULL
                         set<-function(y){
                           x<<-y    ## used in case we have to change the input data. 
                           inv<<-NULL
                         }
                         get<-function()x
                         setinv<-function(inverse) inv<<-inverse  ## sets the inverse
                         getinv<-function()  inv       ## gets the inverse 
                         list(set=set,get=get,setinv=setinv,getinv=getinv) ##list of functions created above
}


## This R-function retrieves the cached data if available and if the cached data 
## is not available it will compute the inverse and Caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()      ##obtains the value of inverse from above R-function.
  if(!is.null(inv)){
    print("getting cached data")  ##checks if the inverse is actually in the cache.
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)## Computing the inverse in case where inverse is not in the cache.
  x$setinv(inv)
  inv
}
