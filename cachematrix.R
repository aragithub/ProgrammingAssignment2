## Assesment2 "Caching the inverse of matrix" .Write a pair of functions that cache the inverse of a matrix.

## Next function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
        x<<-y
        m<<-NULL
         }
         
        get<-function() x
        setinversa<-function(inversa) m<<-inversa
        getinversa<-function() m
        list(set=set, get=get,
                setinversa=setinversa,
                getinversa=getinversa)
}


## Assesment2. This function computes the inverse of the special "matrix" returned by "makeCacheMatrix" above. 
##If the inverse has already been calculated (and the matrix has not changed), then "cacheSolve" should retrieve 
##the inverse from the cache.
  
  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinversa()
  
        if(!is.null(m)){
        message("getting cached data")
        return(m)
        }
        
        data<-x$get()
        m<-solve(data,...)
        x$setinversa(m)
        m
        
}
