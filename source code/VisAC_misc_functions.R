#'
#' Function to shorten a name. Returns a name with the specified number of words
#'
#' Given a name and a number of words, returns a name composed by the last word in the name plus
#' a number o words from the beginning until the specified number of words is met.
#' Only words in the name that have more than two letters are considered
#'
#' @param input_name the name to shorten
#' @param num_names	the number of names to include 
#'
#' @examples
#' short_name("jose luis moura borges", 1)  # returns 'borges'
#' short_name("jose luis moura borges", 2)  # returns 'jose borges'
#' short_name("jose luis moura borges", 3)  # returns 'jose luis borges'
#'
#' @export

short_name <- function(input_name, num_names){

	new_name <- ""
	name_words <- unlist(strsplit( input_name, " ") )
	
	if(length(name_words) > num_names){  ## verifies if the name has more than one word
		counter <- 1  ## counts the number of processed words
		counter_palavras <- 1  ## counts the number of words having more than two words 
		
		while(counter_palavras <= num_names-1 & counter <= length(name_words)){
			new_name <- paste(new_name, name_words[counter])		
			
			if (nchar(name_words[counter]) > 2) {
				counter_palavras <- counter_palavras + 1
			}		
			counter <- counter +1	
		}
		new_name <- paste(new_name, name_words[length(name_words)])  ## inserts last names
	
	}else {
	
		new_name <- input_name	## if the original name has the intended number of words, returns the original name 
	}

	return(new_name)
}		


#' This function is able to deal with dates with more than one symbol
#' dates such as "FEB 1671/73" were being problematic
#' Function to extract a year from a string parsed from a GEDCOM file.
#'
#' A GEDCOM has dates having, for example, the format 'DATE 12 MAR 2018', 'DATE FEB 1977' or 'DATE BET 1991 AND 1997'.
#' This function gets the year from such a string, and returns a year depending on the what parameter:
#'
#' The function parses substrings with 4 letters, which in this type of strings correspond to years' references.
#' The what parameter determines what is returned in case the string contains two years.
#'
#' 1: returns the first year; 2: returns the second year; 3: returns the integer below the average of the two years
#'
#' get_year("FEB 1992", 1 ) -> 1992
#'
#' get_year("BET 1992 AND 1997", 1 ) -> 1992
#'
#' get_year("BET 1992 AND 1997", 2 ) -> 1997
#'
#' get_year("BET 1992 AND 1997", 3 ) -> int( (1992+1997) / 2 ) = 1994
#'
#' @param date_string a date string as parsed by the gedcom_parser() function
#' @param what defines the year to be returned (1: the first; 2: the second; 3: the lowest nearest integer of the average of the two years )
#'
#' @examples
#' get_year("FEB 1992", 1 ) # returns 1992
#' get_year("BET 1992 AND 1997", 1 ) # returns 1992
#' get_year("BET 1992 AND 1997", 2 ) # returns 1997
#' get_year("BET 1992 AND 1997", 3 ) # returns 1994
#' @export
get_year <- function( date_string, what){

	aux_v <- unlist(strsplit(date_string,"[/ ,-]+"))
	aux_year <- ""	
	if(what == 1){
		aux_year <- aux_v[nchar(aux_v)==4][1]
		
	} else if (what == 2) {
	
		## A gedcom file has "BET 1677 AND 79".... and I had to discard the second year
		if ( length ( aux_v[nchar(aux_v)==4] ) == 1 ) {
			aux_year <- aux_v[nchar(aux_v)==4][1]
		} else {
			aux_year <- aux_v[nchar(aux_v)==4][2]
		}
	} else if (what == 3) {
		if ( length ( aux_v[nchar(aux_v)==4] ) == 1 ) {
			aux_year <- aux_v[nchar(aux_v)==4][1]
		} else {
			aux_year <- ( as.integer(aux_v[nchar(aux_v)==4][2])  + as.integer(aux_v[nchar(aux_v)==4][1])  ) / 2
		}
	}
	
	the_year <- 0
	if ( is.na(aux_year) ){
		the_year <- NA
	} else if (aux_year == "") {
		the_year <- NA
	} else {
		the_year <- suppressWarnings( as.integer(aux_year) )
	}
	
	return( the_year )
}

#' Function to extract a year from a string parsed from a GEDCOM file.
#'
#' A GEDCOM has dates having, for example, the format 'DATE 12 MAR 2018', 'DATE FEB 1977' or 'DATE BET 1991 AND 1997'.
#' This function gets the year from such a string.
#'
#' The function parses substrings with 4 letters, which in this type of strings correspond to years' references, and returns the first occurrence.
#'
#' @export 

get_year_from_datestring <- function( line){

	aux <- line
	aux_v <- unlist(strsplit(aux,"[ ]"))

	aux_year <- aux_v[nchar(aux_v)==4][1]
	
	return(aux_year)
}









#' A function that computes the node positions for the ancestors' tree according to the number of generations
#' @examples
#' tree_ordering( 5 ) 
#' @param num_generations_arg is the number of generations in the tree

tree_ordering <- function( num_generations_arg ){  

	## dataframe to keep the node positions
	ordem <- data.frame( id=integer(), geracao=integer(), ordem=integer() )

	## auxiliar variables
	loop_counter <- 0 ## counts the number of generations
	ele_counter <- 0  ## counts the number of elements processed
	
## vectors that correspond to the dataframe columns	
	v_id <- integer( 2 ^ num_generations_arg - 1 )
	v_ger <- integer( 2 ^ num_generations_arg - 1 )
	v_ordem <- integer( 2 ^ num_generations_arg - 1 )
	
	for ( i in num_generations_arg:1){

		ele_counter <- ele_counter +1
		ele_generation = 2^(i-1) ## number of individuals in the current generation

		init_position_ger <- 2^loop_counter  ## position of the first individual in the current generation

		v_id[ele_counter] <- ele_counter
		v_ger[ele_counter] <- i
		v_ordem[ele_counter] <- init_position_ger

		if(i > 1) {  ##  For the generation with a single individual the loop is not needed
			
			for (j in 1:(ele_generation-1) ){
				ele_counter <- ele_counter +1
				v_id[ele_counter] <- ele_counter
				v_ger[ele_counter] <- i
				
			## the space between two individuals depends on the depth of the tree 
				v_ordem[ele_counter] <- (init_position_ger + j*(2^(loop_counter+1)) )
			}
		}
		loop_counter <- loop_counter + 1
		
	}
	v_ordem <- ((2^num_generations_arg)-v_ordem)
	ordem <- data.frame(id=v_id, geracao=v_ger, ordem=v_ordem)
	ordem <- ordem[with(ordem, order(geracao,id)),]
	
	return(ordem)
}


