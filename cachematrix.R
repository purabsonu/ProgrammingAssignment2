## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This part of the code tries to set up the inverse value for the matrix
##This part of the code makes the vector required for the next function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setInverse <- function(inverse) m <<- inverse 
        getInverse <- function() m
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse  )

}


## Write a short comment describing this function
## This code first takes the list output of the previous code and 
## checks whether the inverse is cached or not
## If not, the inverse is calculated and the list is updated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.na(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse <- m
        return(m)
}
