## Those two functions are designed to ofload CPU. 
## To decreas the time required to obtain desired result, in this case inverse of matrix

## This function is creating a special list
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setmean <- function(solve) s <<- solve
        getmean <- function() s
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)

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
