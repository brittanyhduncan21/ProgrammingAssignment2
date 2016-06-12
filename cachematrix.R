## The makeCacheMatrix and cacheSolve functions can be used together to 
## find the inverse of a matrix and cache the resulting inverse so that 
## future calls for the inverse of the same matrix will not need to be 
## recomputed. 

## This function takes a matrix as its argument and returns a list of 
## functions among which get and set the inverse of the matrix.  

makeCacheMatrix <- function(A = matrix()) {
        ## Initial value for the inverse of the matrix
        Ainv <- NULL
        setMatrix <- function(B) {
                A <<- B
                Ainv <<- NULL
        }
        ## Retrieve the matrix passed to this function
        getMatrix <- function() A
        ## Set the inverse of this matrix 
        setInv <- function(inv) Ainv <<- inv
        ## Retrieve the inverse of this matrix. 
        getInv <- function() Ainv
        ## return the above list of functions 
        list(setMatrix=setMatrix, getMatrix=getMatrix, setInv=setInv, getInv=getInv)
}


## This function takes a list of functions (from makeCacheMatrix) as its
## argument and either computes the inverse of a matrix or retrieves the 
## cached inverse of the matrix and returns it. 

cacheSolve <- function(list, ...) {
        ## Retrieve the inverse of the matrix (NULL if never computed before)
        Ainv <- list$getInv()
        ## Check to see if inverse has been previously computed. If yes, retrieve
        ## cached inverse. 
        if(!is.null(Ainv)) {
                message("getting cached data")
                return(Ainv)
        }
        ## Otherwise, get the matrix passed to makeCacheMatrix
        data <- list$getMatrix()
        ## Use the solve function to compute its inverse
        Ainv <- solve(data,...)
        ## Set the inverse for this matrix so that in future it won't be recomputed
        list$setInv(Ainv)
        ## Return a matrix that is the inverse of 'x'
        return(Ainv)
}
