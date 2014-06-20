## The objective of this R will accept input as square matrix and return inverse of the matrix 
## from cache if exist else use solve function to return inverse of the matrix. 

## This function helps to save matrix in cache and retrieve it when needed and returns list refering to the input. 

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinvmat <- function(invmat) im <<- invmat
        getinvmat <- function() im
        list(set = set, get = get,
             setinvmat = setinvmat,
             getinvmat = getinvmat)
}


## This function will accept list returned by makeCacheMatrix and checks if inverse matrix of input already present in cache. 
## If yes, inverse matrix will be returned from cache else inverse will be returned using solve function.

cacheSolve <- function(x, ...) {
        m <- x$getinvmat()
        message(m)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        message(data)
        m <- solve(data)
        x$setinvmat(m)
        m
}
