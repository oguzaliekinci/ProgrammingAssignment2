## makeCacheMatrix function creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the given matrix (setInv)
## get the inverse of the given matrix (getInv)
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function(){x}
        setInv <- function(z) i <<- z 
        getInv <- function() i
        list(set=set, get=get, 
             setInv=setInv, 
             getInv=getInv)
}

## cacheSolve function calculates the inverse of the special "matrix" created with the above (makeCacheMatrix) function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse of the matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse of the matrix in the cache via the setInv function.
cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i)) {
                return(i)
        }
        mtrx <- x$get()
        i <- solve(mtrx, ...)
        x$setInv(i)
        i
}
