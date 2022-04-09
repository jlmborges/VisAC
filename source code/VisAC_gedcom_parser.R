#' Parses a standard GEDCOM file and returns an excel file with the parsed information.
#'
#' Parsed from a GEDCOM file, the information necessary to create a Contextual Family Tree. 
#' The input must be of the type "filename.ged" and the output is the "outfile.xlsx"
#'
#' The parsed data is returned in text format and will be stored in a .xlsx file.
#' Each row in the dataframe corresponds to an individual and has the following attributes:
#'
#' \itemize{
#' \item personId        # individual ID
#' \item name
#' \item sex
#' \item birthdate
#' \item birthlocal
#' \item deathdate
#' \item alive 			 # Y/N flag indicating if the individual is alive
#' \item famc            # ID of the family in which the individual is a child
#' \item fatherid        # ID of his father
#' \item motherid        # ID of his mother
#' \item spouse1         # ID of the first spouse
#' \item marriagedate1   # date of the first marriage
#' \item marriagestatus1 # status of the first marriage  M:married, D:divorced, A:annulled, O:other  
#' \item splitdate1      # date of the break-up from the first marriage
#' \item spouse2       
#' \item marriagedate2   
#' \item marriagestatus2 
#' \item splitdate2
#' \item spouse3       
#' \item marriagedate3   
#' \item marriagestatus3 
#' \item splitdate3
#'}
#'
#' In the current version only three marriages or marriage break-ups are handled.
#'
#' If your GEDCOM includes latin characters make sure that it is saved in the ANSI encoding (you can use notepad to do that).
#'
#' @param input_gedcom_file the filename of the GEDCOM file (without the extension)
#' @param output_xlsx_file  filename for the output xlsx file(without the extension). It is an optional parameter that if null the output file will have the same name as the input file, but with the xlsx extension
#'
#' @examples
#' parse_gedcom("myGEDCOM")  # to parse myGEDCOM.ged and obtain myGEDCOM.xlsx
#' parse_gedcom("myGEDCOM","myExcel") # to parse myGEDCOM.ged and obtain myExcel.xlsx
#' @export
parse_gedcom <- function(input_gedcom_file, output_xlsx_file ){
  
  input_filename <- input_gedcom_file$name
  
  # print("parse_gedcom()")
  
  if ( missing(output_xlsx_file)){
    output_xlsx_file <- input_filename
    if( stri_sub( input_filename ,-4,-1) != ".ged" ){
      output_xlsx_file <- input_filename
    } else {
      output_xlsx_file <-  paste( stri_sub( input_filename ,1 ,-4) , "xlsx" , sep = "" )
    }

  } else {
    if( stri_sub( output_xlsx_file ,-5,-1) != ".xlsx" ){
      
      output_xlsx_file <-  paste( output_xlsx_file , ".xlsx" , sep = "" )
    }		
  }
  
  output_file_name <- output_xlsx_file 
  
  ## Retrieves from a gedcom file a set of selected attributes for all the persons in it
  ## data frame to keep the records
  persons_df <- NULL
  
  complete_filename <- paste(input_gedcom_file$datapath, input_filename, sep="")
  
  conn <- file( input_gedcom_file$datapath , "r")
  
  ## auxiliar variables
  person_counter <- 0
  family_counter <- 0
  
  husband <- ""
  wife <- ""
  marriagedate <- "z"
  marriagestatus <- "M"  
  aux_marriage <- "z" ## Variable that will enable to check if a given family had previously de keyword MARR
  splitdate <- "z"
  familyid <- "z"
  
  rectype0 <- "z"
  rectype1 <- "z"
  rectype2 <- "z"
  
  while(length(line <- readLines( conn , 1 , encoding="UTF-8" )) > 0) {
    
    line <- str_trim(line, side = "both")
    # print (line )	
    ## gets the number at the beginning of the line
    ## This number sets the level of the key to which the information corresponds
    firstnum <- str_extract(line,"[0-9]")
    if (is.na(firstnum)) firstnum = -1  ## there are files with garbage lines
    
    ## I've removed the rectypes that I am not handling
    if (firstnum == 0 ){ ## GEDCOM has a 3-levels hierarchy of keys
      rectype0 <- str_extract(line, "HEAD|INDI|FAM")
      rectype1 <- "z"
      rectype2 <- "z"
      
    } else if (firstnum == 1 ){
      rectype1 <- str_extract(line, "NAME|BIRT|DEAT|SEX|FAMC|HUSB|WIFE|MARR|CHIL|DIV|ANUL|DIVF|ENGA|MARB|MARC|MARL|MARS|EVEN")
      rectype2 <- "z"
      
    } else if (firstnum == 2 ){
      rectype2 <- str_extract(line, "GIVN|SURN|DATE|PLAC")	
    }
    
    ## to make sure that we have not NA
    if (is.na(rectype0) ) rectype0 = "NOTKNOWN"
    if (is.na(rectype1) ) rectype1 = "NOTKNOWN"	
    if (is.na(rectype2) ) rectype2 = "NOTKNOWN"
    
    
    if(firstnum == 0 && rectype0 == "INDI" && person_counter > 1) { ## third or above person
      persons_df <- smartbind(persons_df, dfTemp, fill = NA)  	
    } else if (firstnum == 0 && rectype0 == "INDI" && person_counter == 1) { ## if yes, we are going to deal with the second person
      persons_df <- dfTemp
    }
    
    ## person id  
    if( rectype0 == "INDI" && firstnum == 0 ) { ## A new person
      
      ## it is important to initialize dfTemp in order to clean the cells
      dfTemp <- .create_DF_persons()
      
      dfTemp[1,]$personid <- str_replace_all(str_extract(line,"@.+@"),"@","")	
      dfTemp[1,]$alive = "Y"  ## By default the person is alive. If the DEAT keyword is found it will be set as not alive
      dfTemp[1,]$marriagestatus1 = "M"
      dfTemp[1,]$marriagestatus2 = "M"
      dfTemp[1,]$marriagestatus3 = "M"
      person_counter <- person_counter + 1	
    }   
    
    # name info
    if( rectype0 == "INDI" && rectype1 == "NAME" && firstnum == 1) {
      dfTemp[1,]$name <- str_replace( str_extract(line, "NAME\\s(.+)"), "NAME ", "")
      dfTemp[1,]$name <- str_replace_all(dfTemp[1,]$name,"/"," ")
      dfTemp[1,]$name <- str_replace_all(dfTemp[1,]$name,"_"," ")
      dfTemp[1,]$name <- str_replace_all(dfTemp[1,]$name,'"'," ")
      dfTemp[1,]$name <- str_replace_all(dfTemp[1,]$name,'-'," ")
      dfTemp[1,]$name <- gsub("\\s+", " ", str_trim(dfTemp[1,]$name))
      
    } else if( rectype0 == "INDI" && rectype1 == "BIRT" && rectype2 =="DATE") {   	##### handle birthdate info
      
      dfTemp[1,]$birthdate = str_replace( str_extract(line, "DATE\\s(.+)"), "DATE ", "")
      
    } else if( rectype0 == "INDI" && rectype1 == "BIRT" && rectype2 =="PLAC") {
      dfTemp[1,]$birthlocal = str_replace( str_extract(line, "PLAC\\s(.+)"), "PLAC ", "")
      
    } else 	if( rectype0 == "INDI" && rectype1 == "DEAT" && rectype2 =="z") {   #### handle death date info
      dfTemp[1,]$alive = "N"
      
    }else 	if( rectype0 == "INDI" && rectype1 == "DEAT" && rectype2 =="DATE") {   #### handle death date info
      
      dfTemp[1,]$deathdate = str_replace( str_extract(line, "DATE\\s(.+)"), "DATE ", "")
      
    } else 	if( rectype0 == "INDI" && rectype1 == "SEX") {  			#### handle sex	
      dfTemp[1,]$sex = str_replace( str_extract(line, "SEX\\s(.+)"), "SEX ", "")
      
    }else if( rectype0 == "INDI" && rectype1 == "FAMC") {   			#### handle family in which the person is a child
      dfTemp[1,]$famc = str_replace_all(str_extract(line,"@.+@"),"@","")
    }
    
    ## the info on the families is at the end of the GEDCOM file
    ## it indicates the role of each person in a family
    
    if( rectype0 == "FAM" && firstnum == 0 ) {  ## Enters for every new family
      
      if(family_counter == 0){
	  ## Families are defined at the end of the file, after all individuals.
	  ## When we find the first family we have to make sure that the last individual is inserted in the dataframe
        persons_df <- smartbind(persons_df, dfTemp, fill=NA)			
        family_counter <- family_counter +1
      }
      
      familyid <- str_replace_all(str_extract(line,"@.+@"),"@","")
      
	## we have to initialize these variables for each family
      husband <- ""
      wife <- ""
      marriagedate  <- "" 	
      marriagestatus <- "M"  ## O: OTHER  M: Married  
      splitdate <- "z"
      
      aux_marriage <- "z"  ## RESETS marriage indicator
      
    }   ## end ... if( rectype0 == "FAM" && firstnum == 0 ) {
    
    ## As variáveis 'husband' e 'wife' são inicializadas no início com 'z'
    ## se tiverem valores diferentes é porque já foi processada pelo menos uma família
    
    ## gets the id of the husband in the family
    if( rectype0 == "FAM" && rectype1 == "HUSB" ) {
      husband <- str_replace_all(str_extract(line,"@.+@"),"@","")
      
      ## gets the id of the wife in the family
    } else if( rectype0 == "FAM" && rectype1 == "WIFE" ) {    
      wife <- str_replace_all(str_extract(line,"@.+@"),"@","")
      
      ## gets the marriage date of the family
    } else if( rectype0 == "FAM" && rectype1 == "MARR") {	 
      marriagestatus = "M"
      aux_marriage = "M"
      if ( rectype2 == "DATE" ) {
        marriagedate <- str_replace( str_extract(line, "DATE\\s(.+)"), "DATE ", "")	
      }
    } else if( rectype0 == "FAM" && rectype1 %in%  c("DIV","DIVF") ) {	 
      marriagestatus = "D"
      if ( rectype2 == "DATE" ) {
        splitdate <- str_replace( str_extract(line, "DATE\\s(.+)"), "DATE ", "")	
      }
    } else if( rectype0 == "FAM" && rectype1 == "ANUL") {	 
      marriagestatus = "A"
      if ( rectype2 == "DATE" ) {
        splitdate <- str_replace( str_extract(line, "DATE\\s(.+)"), "DATE ", "")	
      }			
    } else if( rectype0 == "FAM" && rectype1 == "EVEN") {	## Some events occur after the marriage indicating widowing, for example
      ## but if it occurs without the MARR keyword before it is an indication of not being married
      ## we are assuming that the absence of everything indicates marriage
      if (aux_marriage == "z") {
        marriagestatus = "O"
      }
      
      if ( rectype2 == "DATE" ) {
        splitdate <- str_replace( str_extract(line, "DATE\\s(.+)"), "DATE ", "")	
      }	
      
    } else if( rectype0 == "FAM" && ( rectype1 %in% c("ENGA","MARB", "MARC", "MARL", "MARS")  ) ) {	 
      # print(rectype1)
      marriagestatus = "O"
      if ( rectype2 == "DATE" ) {
        splitdate <- str_replace( str_extract(line, "DATE\\s(.+)"), "DATE ", "")	
      }	
      
    } else if( rectype0 == "FAM" && rectype1 == "CHIL" && firstnum == 1 ) { ## the last condition to avoid other lower level tags
      # print(rectype2)
      ## for each child in the family gets his id and registers his father and mother
      child <- str_replace_all(str_extract(line,"@.+@"),"@","")
      
      ## This if condition is necessary because some gedcom files reference childs that are not defined
      if( nrow( persons_df[persons_df$personid == child,] ) == 0 ){
        
        dfTemp <- .create_DF_persons()
        
        dfTemp[1,]$personid = child
        dfTemp[1,]$name = "unknown"
        dfTemp[1,]$sex = "U"			
        dfTemp[1,]$marriagestatus1 = "M"
        dfTemp[1,]$marriagestatus2 = "M"
        dfTemp[1,]$marriagestatus3 = "M"
        persons_df <- smartbind(persons_df, dfTemp, fill=NA)
      }
      
      persons_df[persons_df$personid == child,]$fatherid <- husband
      persons_df[persons_df$personid == child,]$motherid <- wife
    } 
    
    if( rectype0 == "FAM" && rectype1 == "WIFE") { 
      
      if( husband != "" & wife != "") {  ## Only if we know the husband and the wife
        
        ## we need to check if it is the first marriage. We are allowing 3 marriages at most
        
        if(  is.na(persons_df[persons_df$personid == husband,]$spouse1) ){ 
          persons_df[persons_df$personid== husband,]$spouse1 <- wife
          
        } else if (is.na(persons_df[persons_df$personid == husband,]$spouse2) ){
          persons_df[persons_df$personid== husband,]$spouse2 <- wife
          
        } else if (is.na(persons_df[persons_df$personid == husband,]$spouse3) ){
          persons_df[persons_df$personid== husband,]$spouse3 <- wife
        }
        
        ## Doing the same for the wife
        if( is.na(persons_df[persons_df$personid == wife,]$spouse1) ){ 
          persons_df[persons_df$personid== wife,]$spouse1 <- husband
          
        } else if (is.na(persons_df[persons_df$personid == wife,]$spouse2) ){
          persons_df[persons_df$personid== wife,]$spouse2 <- husband
          
        } else if (is.na(persons_df[persons_df$personid == wife,]$spouse3) ){
          persons_df[persons_df$personid== wife,]$spouse3 <- husband
        }		
      }  ## end ... if( husband != "z" & wife != "z") { 
    }	
    
    ## Herein we will set the marriage status
    ## M: married   D: divorced   A: annulled   O: other (with no marriage)
    if( rectype0 == "FAM" && rectype1 == "MARR" && rectype2 == "z")	 {
      marriagestatus = "M"
      if( husband != "" & wife != "") {  ## Only if we know the husband and the wife
        
        ## we need to check if it is the first marriage. We are allowing 3 marriages at most
        if(  persons_df[persons_df$personid == husband,]$spouse1 == wife ){ 
          persons_df[persons_df$personid== husband,]$marriagestatus1 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == husband,]$spouse2 == wife ){
          persons_df[persons_df$personid== husband,]$marriagestatus2 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == husband,]$spouse3 == wife ){
          persons_df[persons_df$personid== husband,]$marriagestatus3 <- marriagestatus
        }
        
        ## Doing the same for the wife
        if( persons_df[persons_df$personid == wife,]$spouse1 == husband ){ 
          persons_df[persons_df$personid== wife,]$marriagestatus1 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == wife,]$spouse2 == husband ){
          persons_df[persons_df$personid== wife,]$marriagestatus2 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == wife,]$spouse3 == husband){
          persons_df[persons_df$personid== wife,]$marriagestatus3 <- marriagestatus
        }		
      }  ## end ... if( husband != "z" & wife != "z") { 
      
    } else if ( rectype0 == "FAM" && rectype1 == "DIV" && rectype2 == "z") {	
      
      marriagestatus = "D"
      if( husband != "" & wife != "") {  ## Only if we know the husband and the wife
        
        ## we need to check if it is the first marriage. We are allowing 3 marriages at most
        if(  persons_df[persons_df$personid == husband,]$spouse1 == wife ){ 
          persons_df[persons_df$personid== husband,]$marriagestatus1 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == husband,]$spouse2 == wife ){
          persons_df[persons_df$personid== husband,]$marriagestatus2 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == husband,]$spouse3 == wife ){
          persons_df[persons_df$personid== husband,]$marriagestatus3 <- marriagestatus
        }
        
        ## Doing the same for the wife
        if( persons_df[persons_df$personid == wife,]$spouse1 == husband ){ 
          persons_df[persons_df$personid== wife,]$marriagestatus1 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == wife,]$spouse2 == husband ){
          persons_df[persons_df$personid== wife,]$marriagestatus2 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == wife,]$spouse3 == husband){
          persons_df[persons_df$personid== wife,]$marriagestatus3 <- marriagestatus
        }		
      }  ## end ... if( husband != "z" & wife != "z") { 
      
    } else if ( rectype0 == "FAM" && rectype1 == "ANUL" && rectype2 == "z") {	
      marriagestatus = "A"
      if( husband != "" & wife != "") {   ## Only if we know the husband and the wife
        
        
        ## we need to check if it is the first marriage. We are allowing 3 marriages at most
        if(  persons_df[persons_df$personid == husband,]$spouse1 == wife ){ 
          persons_df[persons_df$personid== husband,]$marriagestatus1 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == husband,]$spouse2 == wife ){
          persons_df[persons_df$personid== husband,]$marriagestatus2 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == husband,]$spouse3 == wife ){
          persons_df[persons_df$personid== husband,]$marriagestatus3 <- marriagestatus
        }
        
        ## Doing the same for the wife
        if( persons_df[persons_df$personid == wife,]$spouse1 == husband ){ 
          persons_df[persons_df$personid== wife,]$marriagestatus1 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == wife,]$spouse2 == husband ){
          persons_df[persons_df$personid== wife,]$marriagestatus2 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == wife,]$spouse3 == husband){
          persons_df[persons_df$personid== wife,]$marriagestatus3 <- marriagestatus
        }		
      }  ## end ... if( husband != "z" & wife != "z") { 
    }	else if ( rectype0 == "FAM" && rectype1 == "EVEN" && aux_marriage == "z" && rectype2 == "z") {	
      marriagestatus = "O"
      
      if( husband != "" & wife != "") {   ## Only if we know the husband and the wife
        
        ## we need to check if it is the first marriage. We are allowing 3 marriages at most
        if(  persons_df[persons_df$personid == husband,]$spouse1 == wife ){ 
          persons_df[persons_df$personid== husband,]$marriagestatus1 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == husband,]$spouse2 == wife ){
          persons_df[persons_df$personid== husband,]$marriagestatus2 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == husband,]$spouse3 == wife ){
          persons_df[persons_df$personid== husband,]$marriagestatus3 <- marriagestatus
        }
        
        ## Doing the same for the wife
        if( persons_df[persons_df$personid == wife,]$spouse1 == husband ){ 
          persons_df[persons_df$personid== wife,]$marriagestatus1 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == wife,]$spouse2 == husband ){
          persons_df[persons_df$personid== wife,]$marriagestatus2 <- marriagestatus
          
        } else if ( persons_df[persons_df$personid == wife,]$spouse3 == husband){
          persons_df[persons_df$personid== wife,]$marriagestatus3 <- marriagestatus
        }		
      }  ## end ... if( husband != "z" & wife != "z") { 
    }
    
    if( rectype0 == "FAM" && rectype1 == "MARR" && rectype2 == "DATE") { ## Quando encontra a mulher da família
      ## e se temos o marido e a mulher definido:
      if( husband != "" & wife != "") {  ## Only if we know the husband and the wife
        ## In case there is a wedding we set hte marriage date
        
        ## we need to check if it is the first marriage. We are allowing 3 marriages at most
        if(  persons_df[persons_df$personid == husband,]$spouse1 == wife ){ 
          persons_df[persons_df$personid== husband,]$marriagedate1 <- marriagedate
          
        } else if ( persons_df[persons_df$personid == husband,]$spouse2 == wife ){
          persons_df[persons_df$personid== husband,]$marriagedate2 <- marriagedate
          
        } else if ( persons_df[persons_df$personid == husband,]$spouse3 == wife ){
          persons_df[persons_df$personid== husband,]$marriagedate3 <- marriagedate
        }
        
        ## Doing the same for the wife
        if( persons_df[persons_df$personid == wife,]$spouse1 == husband ){ 
          persons_df[persons_df$personid== wife,]$marriagedate1 <- marriagedate
          
        } else if ( persons_df[persons_df$personid == wife,]$spouse2 == husband ){
          persons_df[persons_df$personid== wife,]$marriagedate2 <- marriagedate
          
        } else if ( persons_df[persons_df$personid == wife,]$spouse3 == husband){
          persons_df[persons_df$personid== wife,]$marriagedate3 <- marriagedate
        }		
      }  ## end ... if( husband != "z" & wife != "z") { 
      
    } else if( rectype0 == "FAM" && (rectype1 == "DIV" || rectype1 == "ANUL" )&& rectype2 == "DATE") { 
      ## In case there is a divorce or it is anuled we will set the splitdate
      
      if( husband != "" & wife != "") {   ## Only if we know the husband and the wife
        
        
        ## we need to check if it is the first marriage. We are allowing 3 marriages at most
        if(  persons_df[persons_df$personid == husband,]$spouse1 == wife ){ 
          persons_df[persons_df$personid== husband,]$splitdate1 <- splitdate
          
        } else if ( persons_df[persons_df$personid == husband,]$spouse2 == wife ){
          persons_df[persons_df$personid== husband,]$splitdate2 <- splitdate
          
        } else if ( persons_df[persons_df$personid == husband,]$spouse3 == wife ){
          persons_df[persons_df$personid== husband,]$splitdate3 <- splitdate
        }
        
        ## Doing the same for the wife
        if( persons_df[persons_df$personid == wife,]$spouse1 == husband ){ 
          persons_df[persons_df$personid== wife,]$splitdate1 <- splitdate
          
        } else if ( persons_df[persons_df$personid == wife,]$spouse2 == husband ){
          persons_df[persons_df$personid== wife,]$splitdate2 <- splitdate
          
        } else if ( persons_df[persons_df$personid == wife,]$spouse3 == husband){
          persons_df[persons_df$personid== wife,]$splitdate3 <- splitdate
        }		
      }  ## end ... if( husband != "z" & wife != "z") { 
    }	
  } ## end ... while
  
  if (nrow( persons_df[is.na(persons_df$spouse1),] ) > 0 ) persons_df[is.na(persons_df$spouse1),]$marriagestatus1 = NA
  if (nrow( persons_df[is.na(persons_df$spouse2),] ) > 0 ) persons_df[is.na(persons_df$spouse2),]$marriagestatus2 = NA
  if (nrow( persons_df[is.na(persons_df$spouse3),] ) > 0 ) persons_df[is.na(persons_df$spouse3),]$marriagestatus3 = NA
  
  
  close(conn)
  write.xlsx2(persons_df, output_file_name, sheetName = "VisAC_data",  col.names = TRUE, append = FALSE)
}

