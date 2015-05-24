## Put comments here that give an overall description of what your
## functions do: 

## These two functions are used to store a matrix and compute its inverse. Since the calculation of 
## of a matrix inverse might require a lot of computation resources, it might be a good idea not to
## do it more often than necessery. To avoid this, the inverse is not computed if it has already been cached 
## by the function object makeCacheMatrix.  

## Write a short comment describing this function

## This is the "cache function", if passed a matrix as argument and assigned to a variable x, the variable
## will contain references to data, functions and a list of those functions. 

## 1. set will set the value of a matrix
## 2. get will return the value of the matrix
## 3. set_inverse will if provided with the matrix inverse store it
## 4. get_inverse will return the value stored in inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL         ## When function is called with new matrix, inverse is "reset".
        set <- function(y) {
            x <<- y             ## The "<<-" operator is used so the value y can be assigned to x in the parrent enviroment (the body of makeCacheMatrix).
            inverse <<- NULL    ## New data requires inverse to be "reset".
        }
        get <- function() x
        set_inverse <- function(inv) inverse <<- inv ## Again, "<<-" is used to manipulate a variable outside "set_inverse".
        get_inverse <- function() inverse
        list(set = set, get = get,                   ## This list of the four sub-functions is returned.
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function

## This function checks if an appropriate inverse for a matrix is cached (not NULL), if not it calculates it using
## the built in R function solve. cacheSolve calls functions defined in makeCacheMatrix to store its output.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse() ## Returns what is stored in the matrix objects inverse variable.
        if(!is.null(inverse)) { 
            messange("Getting cached data")
            return(inverse)    ## This is the exit point of the function if the if statement is true, code bellow will be skipped.
        }
        data <- x$get()        ## Matrix is returned from matrix object.
        inverse <- solve(data, ...) ## "..." argument can be provided.
        x$set_inverse(inverse) ## Result is set in matrix object 
        inverse                ## and returned.
}
