## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix function 



makeCacheMatrix <- function(x = matrix()) {
        
        ## Initilize NULL 
        inv<-NULL
        
        ## Set value of matrix
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        
        ## get value of matrix
        get<-function()x
        
        ## set inverse of matrix
        setInv<-function(inverse) inv<-inverse
        
        ## get inverse of matrix
        getInv<-function() inv
        
        list(set=set,
             get=get,
             setInv=setInv,
             getInv=getInv)
}


## CacheSolve function returns inverse of matrix.
## cacheSolve: This function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Checks if inverse is computed
        ## If yes, gets the inverse of matrix 
        ## Else, computes inverse using setinv 
        
        inv<-x$getInv()
        if(!is.null(inv)){
                message("getting cache data")
                return(inv)
        }
        
        data<-x$get()
        inv<-solve(data,...)
        x$setInv(inv)
        inv
}
