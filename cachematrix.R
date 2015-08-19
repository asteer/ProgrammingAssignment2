## This pair of functions caches the inverse of a matrix and allows a user
## to either retreive the cached value or write the new value if applicable.
## Fuctions are based on example code from Coursera course rprog-031
## https://class.coursera.org/rprog-031/human_grading/view/courses/975105/assessments/3/submissions


## first function (makeCacheMatrix) creates a vector in which 
## to store the inverse value


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(mean) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## second function (cacheSolve) checks for existing value, 
## computes the inverse if not found and tells user which it is doing

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
