## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL                     ## inv will contain inverse of matrix x
        set <- function(y,nr,nc) {      ## setting a matrix with content y and dimension (nr X nc)
                x <<- matrix(y,nrow=nr,ncol=nc)
                inv <<- NULL
        }
        get <- function() x             ## will return the matrix x 
        setInv <- function(inverse) inv <<- inverse       
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInv()
        ## if inv is previously calculated we will return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if it is not calculated, we will calculate it and also set its value  in environment as in line 37
        data <- x$get()
        inv <- solve(data, ...)   ## solve is the function to calculate the inverse of matrix
        x$setInv(inv)
        inv
}
