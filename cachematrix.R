## Put comments here that give an overall description of what your
## functions do

## cache the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        get <- function() x      #get the input matrix
        setinv <- function(data_solve) inv <<- data_solve    #set inv to data_solve  
        getinv <- function() inv      #get inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)       #return a list
}

## calculate the inverse or the inverse matrix is already calculated

cacheSolve <- function(x, ...){
        inv <-  x$getinv()
        if(!is.null(inv)){
                message("The inverse is calculated, getting cached data!")
                return(inv)   #return inv if the matrix is already calculated
        }
        matrix <- x$get()     #get the input matrix from the return list of function makeCacheMatrix
        inv <- solve(matrix, ...)     #calculate the inverse of matrix
        x$setinv(inv)       
        inv
}



