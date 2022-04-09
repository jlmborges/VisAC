
#' Computes the inbreeding coefficient for the individuals in a family tree
#'
#' @param ds_tree_arg a dataframe containing the individuals in a family tree, without node collapse

family_tree_ic <- function( ds_tree_arg ) {
 # print ("In: ... ftm_ci <- function( ds ) { ")

	## a local copy of the dataframe
	ds_tree_1 <- ds_tree_arg[!is.na(ds_tree_arg$id) & ds_tree_arg$id != "I0000",c('id','father','mother', 'geracao','dob')]

	## need to sort the records by dob (date of birth) because the method requires a certain 
	## order of the records. A father cannot be before their children
	ds_tree_1 <- ds_tree_1[order(ds_tree_1$dob ), ]
	ds_tree_1 <- ds_tree_1[!duplicated( ds_tree_1[,c('id','father','mother')]) ,]

	label <-  as.character(ds_tree_1$id)
	sire <-  as.character(ds_tree_1$father)
	dam <-  as.character(ds_tree_1$mother)
	ger <- as.character(ds_tree_1$geracao)

	inbds <- data.frame(label, sire, dam, stringsAsFactors = FALSE)

	allvalues <- unique( inbds$label ) 
	inbds$label <-  factor(inbds$label, levels = allvalues)
	inbds$sire <-   factor(inbds$sire, levels = allvalues) 
	inbds$dam <-    factor(inbds$dam, levels = allvalues)

	## calling a method from the pedigree package
	ord <- orderPed(inbds) ## Orders a pedigree so that offspring follow parents.

	results <- NULL
	
	if (-1 %in% ord){
		results <- NULL
	
	}else {
		inbds <- ( inbds[order(ord),] )
		inbds <- inbds %>% mutate_all( ~ as.character(.))

		## calling a method from the pedigreeTools package	
		## REF: A simple constructor for a pedigree object. The main point for the constructor is to use coercions to make the calls easier.
		myped <- pedigree(c(inbds$sire), c(inbds$dam), c(inbds$label) )

		## calling a method from the pedigreeTools package	
		## REF: Create the inbreeding coefficients according to the algorithm given in "Comparison of four direct algorithms for computing inbreeding coefficients" 
		## by Mehdi Sargolzaei and Hiroaki Iwaisaki, Animal Science Journal (2005) 76, 401–406.
		values <- inbreeding(myped)

		results <- data.frame(inbds$label, values)
		results$values <- round(results$values,5)	
		names(results) <- c("id","ic")

	}

	return(results)
}

