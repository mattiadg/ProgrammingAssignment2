## Put comments here that give an overall description of what your
## functions do

## Crea una matrice particolare, in grado di tenere in cache il valore della sua
## inversa. I metodi get e set permettono di dare un valore o prelevare la
## matrice. I metodi setinv e getinv rispettivamente valorizzano e restituiscono
## la matrice inversa.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Resituisce l'inversa di x, calcolandola se non è già stata calcolata, oppure
## prelevandola dalla cache se il valore esiste già.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
      message("Getting cached inverse")
      return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinv(inv)
  inv
}