## Coursera R Programming: Assignment 2 - "The Matrix"
## By: Michael Simonsen, 19-03-2015

## Set working dir for my local pc.
setwd("~/Google Drev/R google")                 # google drev hjemme
#setwd("C:/Users/msi/Google Drev/R google")      # google drev fra itu

## The assignment is to write two functions that cache the inverse of a matrix.    
## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## cacheSolve: computes the inverse of the "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## The makeCacheMatrix function takes the argument x, wich is a (invertible) matrix
## The function returns a list with four functions (set, get, setinv, getinv)
## The function works like a "constructor" and any objet passed to makeCacheMatrix
## gets the four functions.
## When the second function, cacheSolve, is called whith a object ("x" below) created  
## with makeCacheMatrix, it uses the list with the four functions as input to cacheSolve()
makeCacheMatrix <- function(x = matrix())     {
    inv = NULL
    set = function(y) {             # 1. set the matrix
        x <<- y                     # `<<-` is used to assign a value to an object
        inv <<- NULL                # outside the local environment of the makeCacheMatrix().  
    }
    get = function() {              # 2. get the matrix
        x 
    }                               
    setinv = function(inverse){
        inv <<- inverse             # 3. set the inverse
    }
    getinv = function() {
        inv                         # 4. get the inverse
    }
    list(set=set,                   # return the list of functions
         get=get, 
         setinv=setinv, 
         getinv=getinv)    
}                                   # end function

cacheSolve <- function(x, ...) {
    ## the argument x is the return/output/objet from makeCacheMatrix()    
    inv = x$getinv()   
    
    # if the inverse is calculated
    if (!is.null(inv)){
        # get it from the cache 
        message("getting cached data")
        return(inv)
    }
    
    # else, calculate the inverse 
    mat.data = x$get()
    inv = solve(mat.data, ...)  # solve() returns its inverse of the matrix 
    
    
    x$setinv(inv)               # setinv() sets the value of the inverse in the cache
    
    return(inv)                 # return: inverse of the original matrix input "x"
                                # to makeCacheMatrix()
}
