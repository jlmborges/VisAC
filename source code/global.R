
.create_DF_persons <- function(){
  my_df <- data.frame( personid = character(),
                       name= character(),
                       sex= character(),
                       birthdate= character(),
                       birthlocal= character(),
                       deathdate= character(), 
                       alive= character(),
                       famc= character(),
                       fatherid= character(),
                       motherid= character(), 
                       spouse1= character(),
                       marriagedate1= character(), 
                       marriagestatus1= character(), 
                       splitdate1= character(), 
                       spouse2= character(),
                       marriagedate2= character(), 
                       marriagestatus2= character(), 
                       splitdate2= character(), 	
                       spouse3= character(),
                       marriagedate3=character(),
                       marriagestatus3= character(), 
                       splitdate3= character(), 
                       stringsAsFactors=FALSE )
  
  return(my_df)
}


##############################################################################
############################################################################
## Variables shared by several files
pkg.globals <- new.env()


pkg.globals$dst_id <- rep(NA, 1)       ## individual id
pkg.globals$dst_father <- rep(NA, 10)	 ## father 
pkg.globals$dst_mother <- rep(NA, 10)   ## mother
pkg.globals$dst_sex <- rep(NA, 10)      ## sex  
pkg.globals$dst_child <- rep(NA, 10)     ## name	
pkg.globals$dst_count  <- rep(NA, 10) ## counts the number of times the individual occurs in the tree
pkg.globals$dst_to_color  <- rep(NA, 10) ## counts the number of times the individual occurs in the tree
pkg.globals$dst_common_highlight <- rep(NA, 10)
pkg.globals$dst_path_to_both <- rep(NA, 10)


pkg.globals$ID_inicial <- c("","")
  
################################
################################




