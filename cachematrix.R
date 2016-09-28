
#makeCacheMatrix will start matrix  
# Set will permit user to Cachset and write again the matrix and it will reset the inverse to null
# get will bring back the cached matrix
# setInverseMatrix allows the user to overwrite the cached inverse matrix
# in get InverseMatrix, the user to retreive inverse matrix
makeCacheMatrix <- function(tmpMatrix = matrix()) {
  inverse_matrix <- NULL
  m_matrix <-tmpMatrix
  
  set <-function(y){
    m_matrix <<- y
    inverse_matrix <<- NULL
  }
  
  get <- function() m_matrix 
  
  setInverseMatrix <- function(matrix) inverse_matrix <<- matrix
  
  # return the cached inverse matrix.
  getInverseMatrix <- function() {
    # return inverse matrix
    return (inverse_matrix)
  }
  
  list (set = set, 
        get= get,
        setInverseMatrix = setInverseMatrix,
        getInverseMatrix = getInverseMatrix)
}


# this function checks if the inverse matrix has been generated (by checking if value = null) 
# If the inverse object is False or NULL. It will create the inverse matrix, cache inverse matrix, and pring back the inverse matrix

cacheSolve <- function(x, ...) {
  if (is.null(x$getInverseMatrix())){
    # generate inverse matrix and store it
    x$setInverseMatrix(solve(x$get(), ...))
  }
  else{
    message ("getting cached data")
  }
  
  return(x$getInverseMatrix())
}
