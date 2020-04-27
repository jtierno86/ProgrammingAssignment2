## This two functions are preparing for creating a inverse of a matrix and save it in the cache
## memory to use it when it´s necessary and avoid the recursive calculations.

## This function (makeCacheMatrix) create a special "matrix" object which is 
## really a list containing a fucntion to: 
## a) set the value of the matrix
## b) get the value of the matrix
## c) set the value of the inverse
## d) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created aboved and if 
## the inverse has already created it get the inverse. Otherwise, it calculate the 
## inverse of the data and sets the value of the inverse in the cache via setinverse
## function. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}

## TEST
ma <- makeCacheMatrix(matrix(c(1, 2, 3,  2, 3, 0,  0, 1, 2), nrow=3, byrow=3))

cacheSolve(ma) #inverse after computation

cacheSolve(ma) #inverse from the cache
