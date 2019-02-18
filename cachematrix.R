## Below are two functions that cache and compute the 
## inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

    makeCacheMatrix <- function(z = matrix()) {
        inv<-NULL
        set<- function(m){
                z<<-m
                inv<<-NULL
                
        }
        get<-function()z
        setinverse<-function(inverse)inv<<-inverse
        getinverse<-function()inv
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` function created above. 
## In case the inverse has been calculated while the matrix didn't change,
## the 'cacheSolve' function should retrieve the inverse of the matrix from the cache.

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'z'
        inv<-z$getinverse()
        if(!is.null (inv))  {
                message("getting cached data")
                return(inv)
        }
        data<-z$get()
        inv<-solve(data,...)
        z$setinverse(inv)
        inv
}


