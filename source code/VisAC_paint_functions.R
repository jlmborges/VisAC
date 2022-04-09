

######################################################
######################################################
## Function that identifies descendents of the repeated ancestors
## if t$to_color == 2 it means that we have to paint it

## The same as paint_simple but using global vectors dst_* instead of the dataframe dstree



##' Method to indentify descendants of a common ancestor. Recursive function.
##'
##' @param to_paint a vector with IDs of ancestors to deal with
paint_simple_dst <- function( to_paint ){

  if( is.vector(to_paint) && length(to_paint) == 0 ) return()  ## checks if it is the end of the recursivity
    
  for( i in 1:length(to_paint)){ ## For each individual in the input parameter
    
    pkg.globals$dst_to_color [ which(pkg.globals$dst_id == to_paint[i])  ] <- 2    
    local_sex <- pkg.globals$dst_sex [ which(pkg.globals$dst_id == to_paint[i])  ]
	
	if ( local_sex == "M" ) {	
		paint_simple_dst( pkg.globals$dst_id[ which( pkg.globals$dst_father == to_paint[i] & pkg.globals$dst_to_color != 2 )] )
    } else {
		paint_simple_dst( pkg.globals$dst_id[ which( pkg.globals$dst_mother == to_paint[i]  & pkg.globals$dst_to_color != 2 ) ] ) 		
	}
  }
  
}


		
##' Method to indentify descendants of a common ancestor that is used by the desdendants highlight feature.
##'
##' @param to_paint a vector with IDs of ancestors to deal with
##' @param init a parameter that will be used to encode the thicknees of the arcs acording to the number of paths that go through the individual

paint_descendants_highlight_dst <- function( to_paint , init ){

	if ( length (pkg.globals$dst_id[ which ( pkg.globals$dst_id == to_paint & (pkg.globals$dst_to_highlight) == 0 ) ]  ) > 0    ){ 
		pkg.globals$dst_to_highlight[ which(  pkg.globals$dst_id == to_paint & (pkg.globals$dst_to_highlight) == 0 )  ] <-  init
	}

	if ( length(pkg.globals$dst_id[ which ( pkg.globals$dst_id == to_paint ) ] )  > 0    ){ 
		pkg.globals$dst_highlight_count[ which( pkg.globals$dst_id == to_paint) ] <- pkg.globals$dst_highlight_count[ which( pkg.globals$dst_id == to_paint) ] + 1
	}	
	new_cases <- NULL


	if (   unique( na.omit( pkg.globals$dst_sex[ which( pkg.globals$dst_id == to_paint) ] ) ) == "M"  ) {
		new_cases <-  ( na.omit( pkg.globals$dst_id[which(pkg.globals$dst_father == to_paint)] ) )	
	} else {
		new_cases <-  ( na.omit (pkg.globals$dst_id[ which( pkg.globals$dst_mother == to_paint ) ] )	 )
	}	
	new_cases <- unique ( new_cases )  ## remove repetitions
	if (  length ( new_cases) > 0  ){
		for ( i in 1:length( new_cases) ) {
		  
			paint_descendants_highlight_dst( as.character( new_cases[i] ) , init + 1 )	
		}
	}
	return ()
}



# the previous version had a problem when one root was a direct ancestor of the other
# now we call this method with an initial vector that may include a root, but the recursive part
# of it never calls for a root.

################################################################## 
#' Indentifies the common ancestors that are common to both roots
#'
#' @param to_paint IDs of all individuals in the tree
shared_common_ancestors <- function(to_paint){

	root_reached <- 0
	root1_reached <- FALSE
	root2_reached <- FALSE
	vec_local_var <- vector( length = length (to_paint) )

	for( i in 1:length(to_paint)){ ## For each individual in the family tree
		pkg.globals$dst_to_color [ which(pkg.globals$dst_id == to_paint[i])  ] <- 2
		local_var <- 0
		root1_reached <- FALSE
		root2_reached <- FALSE    

		if ( any( na.omit(unique( pkg.globals$dst_id[ pkg.globals$dst_father == to_paint[i]  ] ) ) != "I0000" ) ) {

			if (  pkg.globals$ID_inicial[1] %in% pkg.globals$dst_id [ which(pkg.globals$dst_father == to_paint[i] )] ) {
				root1_reached <- TRUE
				#print("root 1 reached")
			}
			if ( pkg.globals$ID_inicial[2] %in% pkg.globals$dst_id [ which(pkg.globals$dst_father == to_paint[i] )] ) {
				root2_reached <- TRUE			
				#print("root 2 reached")
			}

			local_var <- shared_common_ancestors( pkg.globals$dst_id [ which(pkg.globals$dst_father == to_paint[i] & ! pkg.globals$dst_id %in% pkg.globals$ID_inicial ) ] )	

			if (!is.null(local_var) ) {

			if ( local_var == 2 & root1_reached == TRUE ) {
				local_var <- 3 
			} else if ( local_var == 1 & root2_reached == TRUE ){ 
				local_var <- 3
			} else if ( root1_reached == TRUE & root2_reached == TRUE ) {
				local_var <- 3
			} else if ( root1_reached == TRUE ) {
				local_var <- 1		
			} else if ( root2_reached == TRUE ) {
				local_var <- 2	
			} 

			if ( local_var == 3  ){
				pkg.globals$dst_path_to_both[pkg.globals$dst_id == to_paint[i]] <- TRUE
			}


			vec_local_var[i] <- local_var
			}

		}else  if ( any(na.omit(unique( pkg.globals$dst_id[ pkg.globals$dst_mother == to_paint[i] ] ) ) != "I0000" )){

			if (  pkg.globals$ID_inicial[1] %in% pkg.globals$dst_id [ which(pkg.globals$dst_mother == to_paint[i] )] ) {
				root1_reached <- TRUE
			}
			
			if ( pkg.globals$ID_inicial[2] %in% pkg.globals$dst_id [ which(pkg.globals$dst_mother == to_paint[i] )] ) {
				root2_reached <- TRUE			
			}

			local_var <- shared_common_ancestors( pkg.globals$dst_id[ which( pkg.globals$dst_mother == to_paint[i] & ! pkg.globals$dst_id %in% pkg.globals$ID_inicial  ) ] ) 	

			if (!is.null(local_var) ) {
				if ( local_var == 2 & root1_reached == TRUE ) {
					local_var <- 3 
				} else if ( local_var == 1 & root2_reached == TRUE ){ 
					local_var <- 3
				} else if ( root1_reached == TRUE & root2_reached == TRUE ) {
					local_var <- 3
				} else if ( root1_reached == TRUE ) {
					local_var <- 1

				} else if ( root2_reached == TRUE ) {
					local_var <- 2

				} 
				if ( local_var == 3 ){	
					pkg.globals$dst_path_to_both[pkg.globals$dst_id == to_paint[i]] <- TRUE							
				}
				vec_local_var[i] <- local_var
			}	
		}


	}

	if ( 3 %in% vec_local_var ) {
		root_reached <- 3
	} else if ( 1 %in% vec_local_var & 2 %in% vec_local_var) {
		root_reached <- 3		
	} else if ( 1 %in% vec_local_var ) {
		root_reached <- 1	
	} else if ( 2 %in% vec_local_var) {
		root_reached <- 2
	}
	return( root_reached)
}  ### ...  shared_common_ancestors <- function(to_paint){




### gets the paths from the common ancestors identified by the shared_common_ancestors() function
### to the roots
################################################################## 
#' To be called after the shared_common_ancestors method. Set the dataframe parameter to highlight for all individuals in the paths to both roots
#'
#' @param to_paint IDs of common ancestors that are shared by the two roots

common_paint_dst <- function( to_paint ){
  
	if(is.vector(to_paint) && length(to_paint) == 0 ) return()  ## verifica se chegou ao fim da recursividade

	for( i in 1:length(to_paint)){ ## Para cada uma das pessoas identificadas
		pkg.globals$dst_common_highlight[ which(pkg.globals$dst_id == to_paint[i])  ] <- TRUE   
		common_paint_dst( pkg.globals$dst_id[which(pkg.globals$dst_father == to_paint[i] & pkg.globals$dst_common_highlight == FALSE )] )
		common_paint_dst( pkg.globals$dst_id [ which( pkg.globals$dst_mother == to_paint[i] & pkg.globals$dst_common_highlight == FALSE )] ) 		
	}
}  ### ... common_paint_dst () 

