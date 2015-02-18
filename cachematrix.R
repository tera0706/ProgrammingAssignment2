## Those two functions are designed to ofload CPU. 
## To decreas the time required to obtain desired result, in this case inverse of matrix

## This function is creating a special list. The list containe for vaues : 
##get and set (matrix) and get and set(inverse of matrix - in this case solve)
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## This function is calculating the invers of function. But firstly is checking if 
## inverse was solved before. If yes function doesn't need to do calculation again
## just assign the catched value to result.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
