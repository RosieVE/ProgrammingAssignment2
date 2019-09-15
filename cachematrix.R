## The makeCacheMatrix function takes a given matrix and creates a special
## matrix that has an inverse to be calculated. The cacheSolve function
## will take the special matrix, find its inverse and store the inverse
## into a cache. This will allow the inverse to be quickly found later if
## asked for, avoiding the timely process of calculating the inverse again.


## The first function, makeCacheMatrix, takes a matrix as its argument.
## It first initializes the inverse, 'i', to NULL. Nested in the function is
## the function 'set', which puts the matrix and its inverse into the parent
## environment. The function 'get' then returns the matrix. The function 
## 'setinverse' initiates the inverse to 'i' in the parent environment. The
## function 'getinverse' then prints the inverse matrix 'i'. Lastly, the 
## function creates a list to set all of the nested functions to their 
## names, which allowsbfor the use of the '$' operator if desired.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
                ## '<<-' operator puts x and i in parent environment
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        ## set i to the inverse in the parent environment
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
        ## setting the names allows us to use '$' operator
}

## The second function, cacheSolve, takes a matrix of the type 
## makeCacheMatrix() as its argument. This means the original matrix must
## be the result of the previous makeCacheMatrix function. It first
## initializes 'i' to the inverse. If the inverse is not NULL, it will be
## returned from the cache. If the inverse is NULL (meaning it hasn't been
## calculated previously), the solve function will calculate the inverse of
## the matrix and initialize it to 'i'. 'i' will then be set as the inverse
## for the matrix in the cache. The inverse will then be printed out.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## If the inverse is in the cache, it will be printed out
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
