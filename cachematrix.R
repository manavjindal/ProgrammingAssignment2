## return: a list containing functions to

## This function return a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

## x: a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        
        v_inv <- NULL
        
        set <- function(z) {
                x <<- z
                v_inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inverse)  v_inv <<- inverse
        
        getinv <- function() v_inv
        
        list(set=set, 
             get=get, 
             setinv=setinv, 
             getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        v_inv <- x$getinv()
        
        # if the inverse has already been calculated
        if(!is.null(v_inv)) {
                
                # get it from the cache and skips the computation.
                
                message("Getting cached data, please wait")
                return(v_inv)
        }
        
        # else calculates the inverse
        
        data <- x$get()
        
        v_inv <- solve(data, ...)
        
        # sets the value of the inverse in the cache via the setinv function
        x$setinv(v_inv)
        
        #return the inversed matrix
        return(v_inv)
}
