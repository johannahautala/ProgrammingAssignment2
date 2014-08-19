
## The following two functions make it faster to inverse a matrix. Inversing a 
## huge matrix may be time consuming so if a matrix is inversed once, the 
## result is stored. This result is then used instead of reinversing if the 
## inverse is needed afterwards.

## The first function assigns a list of four simple functions to matrix. The
## matrix is then handled through these functions in order to get or set the 
## value or the inverse of the matrix.

makeCacheMatrix <- function( Matr = matrix() ) {
	
	IM <- NULL  ## variable containing the inverse of the matrix
	
	## function to set the value of the matrix
	setMatrix <- function( Matr2 ) {

		Matr <<- Matr2
		IM <<- NULL ## if matrix is changed, the previous inverse 
				## must be nulled, of course
	}
	
	## function to get the value of the matrix
	getMatrix <- function() Matr
	
	## function to set the inverse of the matrix
	setInverse <- function( InverseM ) IM <<- InverseM

	## function to get the inverse of the matrix
	getInverse <- function() IM

	list( SetMatrix = setMatrix, GetMatrix = getMatrix,
		SetInverse = setInverse, GetInverse = getInverse )
	
}


## This function returns the inverse of the given matrix. If the inverse has
## already been calculated, it is printed. If not, it is calculated and then
## printed.

cacheSolve <- function( Matr, ... ) {
        
	IM <- Matr$GetInverse()

	if( is.null( IM ) ) { ## i.e. the inverse has not ever been calculated
		
		data <- Matr$GetMatrix()

		IM <- solve( data, ... )

		Matr$SetInverse( IM )
	} else {

		message( "cached inverse" )
	}
		
	IM

}

