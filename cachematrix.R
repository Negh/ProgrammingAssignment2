## This pair of functions enables to cache the inverse of a special "matrix" 
## object if it has been previously calculated


## makeCacheMatrix takes a matrix (by default an empty one) and return a special 
## "matrix" object which is a list of functions to access (get) and to modify 
## (set) the initial matrix (x) and its inverse (i)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes a special "matrix" object, tries to retrieve the inverse if 
## it has already been calculated and if not, calculates it using the solve() 
## function
## We suppose that the matrix supplied is always invertible therefore there is 
## no verification needed before applying the solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
