## 2022.02.18 single click works in the plain tree
## double-click also works in the plain tree but only for root selection
## 
## in this version we can double-click in the plain plot to highlight a connection
##
##  in the plain plot the double click now also  plots larger dots for the selected individual
##  we now have a print_time flag that enables the print outs of running time
##  we have cleaned the portrait variables that were not being used

	
options(dplyr.summarise.inform = FALSE , shiny.useragg = TRUE , warn = -1)



print_time <- FALSE

###############################################################
## Shiny R application. 
## Definition of the interface

#' VisAC: An Interactive Tool for Visual Analysis of Consanguinity in the Ancestry of Individuals
#
#' A Shiny app
#'
#'@export
VisAC <- function() { ## convert into a stanalone function

## parameters for the interface
numbers_width <- 1
pad_margin <- 2

## definition of the interface
ui <- fluidPage( 
  
  tabsetPanel(type="tab",
              
              tabPanel("Main Panel",
                       sidebarLayout(
                         sidebarPanel( width=2,
                                       
						   fluidRow( 	
							 column ( 3 , radioButtons("plotType", "Roots", c("One"=1, "Two"=2), selected = 1) ) ,
							 column ( 9 , selectizeInput("sel_ind", "Locate Individual", c("I1"="I1"), selected = "I1", options = list( maxOptions = 5000) ) )
						   ),
						   
						   fluidRow(  column ( 5 , selectizeInput("root_1", "Root 1 ID", c("I1"="I1"), selected = TRUE, options = list( maxOptions = 5000 , selectOnTab = TRUE ) ) ) ,		
									  column ( 5 ,selectizeInput("root_2", "Root 2 ID", c("I2"="I2"), selected = "I2",  options = list( maxOptions = 5000, selectOnTab = TRUE )  )) ,
									  column ( 2 ,actionButton("revert_plot", "p",style = "color: grey; background-color:LightGrey; padding: 5px 5px; width: 30px; height: 36px; cursor: pointer;  font-size: 12px; font-weight: 100; margin-top: 23px"  ) ) 															
						   ),
						   
						   fluidRow( column ( 12 , selectizeInput("root_1_name", "Root 1 name", c("I1"="I1"), selected = "I1", options = list( maxOptions = 5000 ) ) ) ) ,
						   fluidRow( column ( 12 , selectizeInput("root_2_name", "Root 2 Name", c("I2"="I2"), selected = "I2", options = list( maxOptions = 5000 ) ) ) ) ,
						   
						   fluidRow( 	
							 column( 6 , 
									 fluidRow ( column ( 12 , numericInput("generations", "Generations", value = 5, min =2, max = 20)  ) ) ,
									 fluidRow ( column ( 12 , actionButton("update_plot", "update plot",style = "color: white; background-color:grey; padding: 8px 10px; width: 110px; height: 45px; cursor: pointer;font-size: 16px; font-weight: 600"  ) ) ) ) ,
							 
							 column ( 6 , radioButtons("root_click", "Select", c("Root 1"=1, "Root 2"=2 , "Kinship"=3, "Descendants"=4), selected = 3) ) 
							 
						   ) ,
						   
						   fluidRow( 	
							 column( 5 , 
								checkboxInput("plain_tree", "plain-tree", value = FALSE, width = NULL)
							 ) ,
							 
							 column ( 7 , 
								checkboxInput("pt_conn", "connect common", value = FALSE, width = NULL) 
							 ) 
							 
						   ) ,						   

						   fluidRow( 
							 column ( 12 , 			   
								checkboxInput("high_both", "enhance common", value = FALSE, width = NULL)
							 )
						   ) ,
						   				   
						   fluidRow( verbatimTextOutput("plot_double_click_info") ,style="background-color:#1D79A7 "),										
						   fluidRow( verbatimTextOutput("plot_hoverinfo_1") )
                         ),
                         mainPanel( width = 10 ,	plotOutput("the_plot",height = "900px", click = "the_plot_click", dblclick = "the_plot_dblclick", brush = brushOpts(id = "the_plot_brush", resetOnNew = TRUE ), hover =hoverOpts(id= "plot_hover", delay = 200, delayType = c("debounce",   "throttle")  ) ))
                       )
                       
              ), ## ... tabPanel("Tests",
              tabPanel("Plot Settings",
                       
                       wellPanel(
                         fluidRow( 
                           column( 12 ,  h5("Input file",style="color:Navy;font-weight:bold") ) , 
						   column( 6 , 
							   column(8, fileInput("genFile", "  .xlsx File with GEDCOM data", accept = ".xlsx") ),
							   column(2, htmlOutput("input_file_message") )  
						   )      
                            
                         ) , style = paste("padding: ",pad_margin,"px;", sep="" )	
                       ),				
                       wellPanel( 
                         fluidRow( 
                           column ( 6, h5("Labels for common ancestors",style="color:Navy;font-weight:bold") ) , column( 6 , h5("Labels for all ancestors",style="color:Navy;font-weight:bold") ) ,
                           column ( 6, 
                                    column( 2, checkboxGroupInput("print_names", "include", choices = c("Names"="names", "IDs"="ids") , selected = c("names")  )),
                                    column( 2, checkboxGroupInput("names_FM", "Gender", choices = c("F"="F", "M"="M") , selected = c("F","M")  )),
                                    column( 4, radioButtons("names_inc", "Who", choices = c("common ancestors" = "common" , "ancestors in path" = "path" ) , selected = "path"))		
                           ) ,
                           column ( 6 ,					
                                    column( numbers_width * 2 , radioButtons("all_names", "Names", choices = c("Yes"="yes", "No"="no") ,  selected = c("yes") ) ),
                                    column( numbers_width * 2 , numericInput("all_names_ger", "Generations", value = "4", min = 1, max = 15, step = 1 ) )
                           )
                         ) 
                         , style = paste("padding: ",pad_margin,"px;", sep="" ) ),
                       
                       
                       wellPanel( h5("Inbreeding labels",style="color:Navy;font-weight:bold") ,
                                  fluidRow(
									column( 12 ,
										column( 1, checkboxGroupInput("IC" , "Inbreeding Coeff.", choices = c("include"="inc","standardize"="std") , selected =  c("inc") ) ) ,
										column( numbers_width, numericInput("max_IC", "Max IC value", value = "0.231", min = 0.2, max = 1.0, step = 0.001 ) ) ,
										column( 1, checkboxGroupInput("RC" , "Relationship Coeff.", choices = c("include"="inc","standardize"="std") , selected =  c("inc") ) ) ,
										column( numbers_width, numericInput("max_RC", "Max RC value", value = "0.231", min = 0.2, max = 1.0, step = 0.001 ) ),
										column( 1, radioButtons("kinship", "Plot kinship", choices = c("yes"="yes","no"="no") , selected = "yes")) ,
										column( numbers_width, numericInput("kinship_limit", "Kinship limit", value = 5, min = 0, max = 20, step = 1 ) )
									)
                                  ) 
                                  , style = paste("padding: ",pad_margin,"px;", sep="" ) ),
                       
                       wellPanel( h5("Plot title and subtitle",style="color:Navy;font-weight:bold"),
                                  fluidRow(
									column( 12 , 
										column( 1, checkboxGroupInput("print_title", "Include", choices = c("Title"="title","Subtitle"="subtitle"))),
										column( 1, radioButtons("default_titles", "Defaults", choices = c("yes"="yes","no"="no") , selected = "yes")),
										column( 5, textInput("plot_title", "Plot title", value = "my title") ) ,
										column( 5, textInput("plot_subtitle", "Plot subtitle", value = "my subtitle") )
									)
                                  ) 
                                  , style = paste("padding: ",pad_margin,"px;", sep="" ) ),					

                       wellPanel(	h5("File settings",style="color:Navy;font-weight:bold"),			
                                  fluidRow( 
									column ( 6 , 
										column(numbers_width*2 , selectInput("file_type", "File Type", c("pdf","jpg","png"), selected = "jpg" ) ) ,
										column(numbers_width*2 , selectInput("file_resolution", "dpi", c("100","300", "600", "900", "1200"), selected = "600" ) ),
										column(numbers_width*2 , selectInput("page_size", "Page size", c("A4","A3","A2","GR"), selected = "A3" ) )
 
									),
									column( 6 ,
										column(2, actionButton("save_settings", "save settings",style = "color: white; background-color:grey; padding: 10px 15px; width: 150px; height: 60px; cursor: pointer; font-size: 16px; font-weight: 600;"  ) )  ,	
										column(2, actionButton("reset_settings", "reset settings",style = "color: white; background-color:grey; padding: 10px 15px; width: 150px; height: 60px; cursor: pointer;font-size: 16px; font-weight: 600;"  ) )
									)
                                  )				
                                  , style = paste("padding: ",pad_margin,"px;", sep="" ) ),
                       wellPanel(	h5("File name",style="color:Navy;font-weight:bold"),				
                                  fluidRow(
									column ( 6 , 
										column(2, radioButtons("default_plot_file", "Default?", choices = c("yes"="yes","no"="no") , selected = "yes")),				
										column(10, textInput("file_name", "Plot file Name", value = "plot 1") ) 
									),
									column ( 6 ,
										column(2, actionButton( inputId = "save_plot", label = "save plot", style = "color: white; background-color:grey;
									 padding: 10px 15px; width: 150px; height: 60px; cursor: pointer;
									 font-size: 16px; font-weight: 600;"  ) ) 
								 )
                                  ) , style = paste("padding: ",pad_margin, "px; vertical-align: bot;", sep="" ) )
                       
              ), ## ... tabPanel("Settings",
              
              tabPanel("Other Settings",
                       
                       wellPanel(h5("Markers and lines",style="color:Navy;font-weight:bold"),				
						 fluidRow(
							column( 12 , 
							   column(numbers_width , numericInput("ca_circle_size", "Common Ancestor", value = 5.0 , min=1.0, max=10.0, step=0.1)),
							   column(numbers_width , numericInput("circle_size", "Individual in path", value = 3.5, min=0.0, max=5.0, step=0.1)),					
							   column(numbers_width , numericInput("inb_circle_size", "Inbreeding", value = 2.5 , min=1.0, max=10.0, step=0.1)),	
							   column(numbers_width , numericInput("ks_circle_size", "Kinship", value = 4.0 , min=1.0, max=10.0, step=0.1)),						
							   column(numbers_width , radioButtons("use_arrows", "arc's type", choices = c("arrow"="yes","tapered"="no") , selected = "yes")),
							   column(numbers_width , numericInput("arrow_size", "Arrow size", value = 0.5, min=0.0, max=3.0, step=0.1)),
							   column(numbers_width , numericInput("arrow_angle", "Arrow angle", value = 6 , min = 1, max = 45 , step = 1 )),
							   column(numbers_width , numericInput("line_thin"  , "Thin line width"  , value = 0.3 , min=0.1 , max=2.0 , step=0.1 )),
							   column(numbers_width , numericInput("line_thick" , "Thick line width" , value = 0.6 , min=0.1 , max=5.0 , step=0.1 ))
						   )
						 )  
						 , style = paste("padding: ",pad_margin,"px;", sep="" ) ),	
 
                       wellPanel(  h5("Paths to common ancestors",style="color:Navy;font-weight:bold"),
                         fluidRow(
							column( 12 , 
							   column(numbers_width , offset=0, numericInput("names_font", "Name's label size", value = 4, min=2, max=10 , step = 0.1)),	
							   
							   column(numbers_width , radioButtons("name_alg_F", "Female label align", choices = c("Right"="right", "Center"="center" , "Left"="left") , selected = "left")),
							   column(numbers_width , numericInput("name_x_F", "x offset", value = 0.0, min=-10.0, max=10.0, step=0.1)),
							   column(numbers_width , numericInput("name_y_F", "y offset", value = -2.0, min=-10.0, max=10.0, step=0.1)),							   
							   column(numbers_width , radioButtons("name_alg_M", "Male label align", choices = c("Right"="right", "Center"="center" , "Left"="left") , selected = "right")),
							   column(numbers_width , numericInput("name_x_M", "x offset", value = 0.0, min=-10.0, max=10.0, step=0.1)),
							   column(numbers_width , numericInput("name_y_M", "y offset", value = 2.0, min=-10.0, max=10.0, step=0.1)) ,							   
							   column(numbers_width , offset=0, numericInput("ids_font", "ID label size", value = 4, min=2, max=10 , step = 0.1)),	
							   column(numbers_width , radioButtons("id_alg_F", "Fem. ID align", choices = c("Right"="right", "Center"="center" , "Left"="left") , selected = "left")),
							   column(numbers_width , radioButtons("id_alg_M", "Male ID align", choices = c("Right"="right", "Center"="center" , "Left"="left") , selected = "right")),				
							   column(numbers_width , numericInput("id_x", "ID x offset", value = 0.0, min=-5.0, max=5.0, step=0.1)),
							   column(numbers_width , numericInput("id_y", "ID y offset", value = 2.0, min=-5.0, max=5.0, step=0.1))  
						   )
                         )
                         , style = paste("padding: ",pad_margin,"px;", sep="" ) ),	
                      
                       wellPanel(	h5("Descendants highlight",style="color:Navy;font-weight:bold"), 			
                         fluidRow(
							column( 12 , 
                           
							   column(numbers_width , numericInput("highlight_factor", "Line factor", value = 1.5, min=1.0, max=5.0, step=0.1)),							   
							   column(numbers_width , radioButtons("white_line", "White line?", choices = c("yes"="yes","no"="no") , selected = "yes")),
							   column(numbers_width , numericInput("white_line_factor", "White line width", value = 0.3, min=0.0, max=1.0, step=0.1)),							   
							   column(numbers_width ,  offset=numbers_width, radioButtons("high_labels", "labels?" , choices = c("yes"="yes","no"="no") , selected = "yes" ) ) ,
							   column(numbers_width , numericInput("high_labels_x", "x offset", value = 0.02, min = -5.0, max = 5.0, step = 0.1)),
							   column(numbers_width , numericInput("high_labels_y", "y offset", value = 0.02, min = -5.0, max = 5.0, step = 0.1)),
							   column(numbers_width , numericInput("alpha_labels", "Label alpha", value = 0.8, min=0.30, max=1.00, step=0.01))
						   )                          
                         )  
                         , style = paste("padding: ",pad_margin,"px;", sep="" ) ),	

                       wellPanel(		h5("Inbreeding coefficient and Kinship",style="color:Navy;font-weight:bold"),		
                         fluidRow(
							column( 12 , 
                           
							   column(numbers_width , offset=0, numericInput("ic_font", "IC label size", value = 4, min=2, max=10 , step = 0.1)) ,					
							   column(numbers_width , radioButtons("ic_alg", "label align", choices = c("Right"="right", "Center"="center" , "Left"="left") , selected = "right")),
							   column(numbers_width , numericInput("ic_x", "x offset", value = -1.0, min=-5.0, max=5.0, step=0.1)),
							   column(numbers_width , numericInput("ic_y", "y offset", value = 0.0, min=-5.0, max=5.0, step=0.1)),							   							   
							   column(numbers_width , offset=0, numericInput("kinships_font", "Kinship label size", value = 4, min=2, max=10 , step = 0.1)),					
							   column(numbers_width , radioButtons("kinship_alg", "label align", choices = c("Right"="right", "Center"="center" , "Left"="left") , selected = "center")),
							   column(numbers_width , numericInput("kinship_x", "x offset", value = 0.0 , min=-5.0, max=5.0, step=0.1)),
							   column(numbers_width , numericInput("kinship_y", "y offset", value = -2.0, min=-5.0, max=5.0, step=0.1))
                           )
                         )  
                         , style = paste("padding: ",pad_margin,"px;", sep="" ) ),

                       wellPanel( 
                         fluidRow( 
                           column ( 5, h5("Labels for all ancestors",style="color:Navy;font-weight:bold") ) , column( 6 , h5("Font size",style="color:Navy;font-weight:bold") ) ,
                           column ( 12, 
							   column(numbers_width , offset=0, numericInput("all_names_font", "Font size", value = 3, min=2, max=10 , step = 0.1)),	
							   column(numbers_width , radioButtons("all_name_alg", "Names align", choices = c("Right"="right", "Center"="center" , "Left"="left") , selected = "center")),
							   column(numbers_width , numericInput("all_name_x", "x offset", value = 0.0, min=-10.0, max=10.0, step=0.1)),
							   column(numbers_width , numericInput("all_name_y", "y offset", value = -2.0, min=-10.0, max=10.0, step=0.1)), 

				
							   column(numbers_width , offset=numbers_width, numericInput("title_font", "Title", value = 16, min=5, max=30 , step = 0.5)),
							   column(numbers_width , numericInput("subtitle_font", "Subtitle", value = 12, min=3, max=25 , step = 0.5)),
							   column(numbers_width , numericInput("font_axis_title", "Axis title", value = 12, min=3, max=25 , step = 0.5)),
							   column(numbers_width , numericInput("font_axis_text", "Axis text", value = 10, min=3, max=25 , step = 0.5))
                           )
                         ) 
                         , style = paste("padding: ",pad_margin,"px;", sep="" ) ),
                           
                       wellPanel( h5("Colors",style="color:Navy;font-weight:bold"),		
                         fluidRow(
                           column( 12, 
							   column( 1, textInput("male_color", "Male line", value = "#0072B7") ),
							   column( 1, textInput("female_color", "Female line", value = "#FF3030") ),
							   column( 1, textInput("fill_male", "Male label", value = "#EDF1FF") ),
							   column( 1, textInput("fill_female", "Female label", value = "#F7E8E1" ) ),
							   column( 1, textInput("fill_kinship", "Kinship", value = "#F5DF4D") ),
							   column( 1, textInput("fill_inb", "Inbreeding", value = "#5a9276") ), ## "#7FD37F"
							   column( 1, textInput("fill_rel", "Coef. Relationship", value = "#B9EAB3") )		
							)
                         ) 
                         , style = paste("padding: ",pad_margin,"px;", sep="" ) )						
              ), ## ... tabPanel("Settings",
              
              tabPanel("Input parsed data",
                       DT::dataTableOutput("table_1") 
                       
              ),  ## ... tabPanel("input data",
              
              tabPanel("GEDCOM parser to .xlsx",
                       fluidRow(column(4, fileInput("gedcom", "GEDCOM file", accept = ".ged"))),
                       fluidRow(column(4, textInput("output_file", ".xlsx filename", value = ""))),				
                       fluidRow(column(4, actionButton("parse_file", "Parse file",style = "color: black; background-color: grey97" ) ))
              ) ## ... tabPanel("GEDCOM parser to .xlsx",
              
  ), ## ... tabsetPanel(type="tab",  
  
) ## ... fluidPage( 



#####################################################################################
####################################################################################
## MAIN CODE
server <- function(input, output, session){
options(warn=-1)
  
  ## function to load settings, which is called only if the file exists
  load_settings <- function () {
    
    my_set <- data.frame ( read_csv ("VisAC_settings.csv" , lazy = FALSE)  )
    
    updateCheckboxGroupInput( session , "IC" , selected = c ( my_set[1,2] , my_set [1,3] ) )	
    updateCheckboxGroupInput( session , "print_names" , selected = c ( my_set[3,2] , my_set [3,3] ) )	
    updateCheckboxGroupInput( session , "names_FM" , selected = c ( my_set[4,2] , my_set [4,3] ) )	
    updateCheckboxGroupInput( session , "print_title" , selected = c ( my_set[5,2] , my_set [5,3] ) )	
    
    updateNumericInput ( session , "max_IC" , value = as.numeric ( my_set [2,2] ) )	
    updateRadioButtons ( session , "default_titles" , selected = my_set[6,2] )
    updateNumericInput ( session , "labels_font" , value = as.numeric ( my_set [7,2] ) )	
    updateNumericInput ( session , "title_font" , value = as.numeric ( my_set [8,2] ) )	
    updateNumericInput ( session , "subtitle_font" , value = as.numeric ( my_set [9,2] ) )	
    updateNumericInput ( session , "font_axis_title" , value = as.numeric ( my_set [10,2] ) )	
    updateNumericInput ( session , "font_axis_text" , value = as.numeric ( my_set [11,2] ) )	
    updateNumericInput ( session , "line_thick" , value = as.numeric ( my_set [12,2] ) )	
    updateNumericInput ( session , "line_thin" , value = as.numeric ( my_set [13,2] ) )	
    updateNumericInput ( session , "arrow_size" , value = as.numeric ( my_set [14,2] ) )	
    updateNumericInput ( session , "arrow_angle" , value = as.numeric ( my_set [15,2] ) )	
    updateNumericInput ( session , "circle_size" , value = as.numeric ( my_set [16,2] ) )	
    
    updateSelectInput  ( session , "file_type",  selected =  my_set [17,2] )
    updateSelectInput  ( session , "file_resolution",  selected =  my_set [18,2] )
    updateSelectInput  ( session , "file_resolution",  selected =  my_set [18,2] )
    updateSelectInput  ( session , "page_size",  selected =  my_set [19,2] )
    
    updateNumericInput( session, "highlight_factor",  value = as.numeric ( my_set [21,2] ) )
    updateNumericInput( session , "white_line_factor",  value = as.numeric ( my_set [22,2] ) )
    updateRadioButtons( session , "white_line",  selected =  my_set [23,2] )		
    
    updateRadioButtons( session , "use_arrows" , selected = my_set [24,2] )
    
    updateRadioButtons( session , "plotType" , selected = my_set [27,2] )
    updateNumericInput ( session , "generations" , value = as.numeric ( my_set [28,2] ) )	
    
    updateCheckboxGroupInput( session , "RC" , selected = c ( my_set[29,2] , my_set [29,3] ) )	
    updateNumericInput ( session , "max_RC" , value = as.numeric ( my_set [30,2] ) )	
    updateRadioButtons( session , "kinship",  selected =  my_set [31,2] )	
    
    updateNumericInput( session , "names_font",  value =  my_set [32,2] )	
    updateNumericInput( session , "ids_font",  value =  my_set [33,2] )	
    updateNumericInput( session , "kinships_font",  value =  my_set [34,2] )	
    updateNumericInput( session , "ic_font",  value =  my_set [35,2] )	
    
    updateRadioButtons ( session , "name_alg_F" , selected = my_set [36,2] )
    updateNumericInput ( session , "name_x_F" , value = my_set [37,2] )
    updateNumericInput ( session , "name_y_F" , value = my_set [38,2] )
    
    updateRadioButtons ( session , "name_alg_M" , selected = my_set [39,2] )
    updateNumericInput ( session , "name_x_M" , value = my_set [40,2] )
    updateNumericInput ( session , "name_y_M" , value = my_set [41,2] )		
    
    updateNumericInput ( session , "id_x" , value = my_set [42,2] )		
    updateNumericInput ( session , "id_y" , value = my_set [43,2] )		
    updateRadioButtons ( session , "id_alg_F" , selected = my_set [44,2] )
    updateRadioButtons ( session , "id_alg_M" , selected = my_set [45,2] )
    
    updateNumericInput ( session , "ic_x" , value = my_set [46,2] )		
    updateNumericInput ( session , "ic_y" , value = my_set [47,2] )		
    updateRadioButtons ( session , "ic_alg" , selected = my_set [48,2] )	
    
    updateNumericInput ( session , "kinship_x" , value = my_set [49,2] )		
    updateNumericInput ( session , "kinship_y" , value = my_set [50,2] )		
    updateRadioButtons ( session , "kinship_alg" , selected = my_set [51,2] )			
    updateRadioButtons ( session , "names_inc" , selected = my_set [52,2] )		
    
    updateRadioButtons ( session , "all_names" , selected = my_set [53,2] )
    updateRadioButtons ( session , "all_name_alg" , selected = my_set [54,2] )
    updateNumericInput ( session , "all_names_ger" , value = my_set [55,2])
    updateNumericInput ( session , "all_names_font" , value = my_set [56,2] )
    updateNumericInput ( session , "all_name_x" , value = my_set [57,2] )
    updateNumericInput ( session , "all_name_y" , value = my_set [58,2] )		
    
    updateTextInput (session, "male_color", value = my_set [59,2] ) 
    updateTextInput (session, "female_color", value = my_set [60,2] ) 
    updateTextInput (session, "fill_male",  value = my_set [61,2] )
    updateTextInput (session, "fill_female",  value = my_set [62,2] )  
    updateTextInput (session, "fill_kinship", value = my_set [63,2] )  
    updateTextInput (session, "fill_inb", value = my_set [64,2] ) 
    updateTextInput (session, "fill_rel", value = my_set [65,2] ) 		
    
    updateRadioButtons ( session , "high_labels" , selected = my_set [66,2] )
    updateNumericInput ( session , "high_labels_x" , value = my_set [67,2])
    updateNumericInput ( session , "high_labels_y" , value = my_set [68,2] )
    
    
    updateNumericInput ( session , "ca_circle_size" , value = my_set [69,2] )
    updateNumericInput ( session , "ks_circle_size" , value = my_set [70,2] )
    updateNumericInput ( session , "alpha_labels" , value = my_set [71,2] )		
    updateNumericInput ( session , "kinship_limit" , value = my_set [72,2] )
    updateNumericInput ( session , "inb_circle_size" , value = my_set [73,2] ) 
    
  }
  
  ## checks if the settings' file exists, and loads it in case it is true
  ## The call must be after defining the function
  if ( file.exists("VisAC_settings.csv") ) {
    load_settings()
  }
 
## Bellow we define a set of variables that are global but only accessed by methods in this file 
	init_time <- 0 ## for measuring the running time 
	ranges <- reactiveValues(x = NULL, y = NULL)  ## for the zoom feature


	ds <- data.frame()  ## creates de dataframe to get the data from the file

	## vectorized version of the ds dataframe. It is much faster with this.
	ds_personid <- vector()
	ds_name <- vector()
	ds_birthdate <- vector()
	ds_deathdate <- vector()
	ds_sex <- vector()
	ds_motherid <- vector()	
	ds_fatherid <- vector()	

	ds_plain <- data.frame()
	ds_kinship <- data.frame( person_id = character(), spouse_id = character(), ancestor_id = character(), sex = character() , steps = integer())

	
	ds_t <- data.frame()    ## keep info about repeated individuals
	ds_names <- data.frame()  ## keeps the names for the labels in the tree (common ancestors and ancestors in the paths)
	ds_tree <- data.frame()  ## keeps the ancestors tree

	ordem <- data.frame() ## keeps Y coordinates for each node in the tree (depends on the num_generations)
	## keeps the gen for which the current ordem was induced. Need to check for changes	
	## since the method to compute the data frame is more heavy	
	ordem_gen <- 0 
	
	
	num_pessoas <- 1 ## counter of the individuals dealt with
	plot_type <- 1 


	makeReactiveBinding("ds")  
	makeReactiveBinding("the_plot")

	kinship_main_highlight <- FALSE
	kinship_main_highlight_id <- ""

	descendants_highlight <- FALSE  ## to keep track of the setting without having to read the input$
	descendants_highlight_id <- ""

	conn_highlight_id <- ""
	conn_highlight <- FALSE

	max_ger <- -1
	max_y_coord <- -1
	max_x_coord <- -1
	neg_order <- 0  
	num_generations <- NULL
	names_for_filename <- ""



	the_plot <- NULL  ## the plot must have this scope in order to enable the highlighting
	the_plot_base <- NULL
	the_plot_2_print <- NULL
	the_plot_highlight <- NULL

## vars for the revert function
	first_plot <- FALSE 
	prev_id <- ""
	prev_type <- 0
	revert_flag <- FALSE	
	back_id <- ""  ## Keeps the previous roots IDs
	back_type <- 0 ## keeps the type of the previous plot. This is used to only recover one ID for type 1 or two for type 2. But it does not change the type


	the_sel_ind_id <- ""
	## new variable to keep the value of the id selected by double-clicking the plot
	double_click_id <- ""


	## plot parameters (are updated in the interface)	
	file_type <- "pdf"  ## jpg or pdf
	plot_resolution <- "300"
	plot_width <- 297 
	plot_height <- 420
	plot_units="mm"

	image_file_name <- "ficheiro"
	title_plot <- "title"
	sub_title <- "subtitle"
	font_size <- 6
	line_thick <- 1
	line_thin <- 0.5
	circle_size   <- 2.0

	male_color <- ""
	female_color <- ""
	fill_male <- ""
	fill_female <- ""
	fill_kinship <- ""
	fill_inb <- ""
	fill_rel <- ""	
	## plot parameters (NOT updated in the interface)	

	alpha_thick   <- 1.0
	alpha_thin    <- 0.5
	labels_alpha <- 0.8
	grid_color <- "grey65"	

	## for the tapered edges
	te1 <- 1.5*1.2 ## tapered edge 1
	tem <- 1.5*0.2 ## tapered edge middle
	te2 <- 1.5*0.1 ## tapered edge 2
  

  
  ###################
  ## Read parsed ged file
  data_file_w <- reactive({	
   # print("IN - data_file_w <- reactive({ ")
    withProgress(message = "Reading data file", detail="", value=0.9,{
      
	  
      file <- NULL
      inFileW <- input$genFile # apontador para o ficheiro
      if (is.null(inFileW) ){  ## Checks if the filename is valid
        return(NULL)
      }
      if ( tidyrules:::strTail( inFileW$datapath, 5 ) == ".xlsx" ) {			
        
        wb <- loadWorkbook( inFileW$datapath )		
        sheets <- getSheets( wb )	
        
        if ( "VisAC_data" %in% names ( sheets ) ) {
          file <- read.xlsx2(inFileW$datapath, sheetName = "VisAC_data", stringsAsFactors=FALSE,  encoding = "UTF-8")
          output$input_file_message <- renderText({ 
            paste("")			
          })			
        } else {
          output$input_file_message <- renderText({ 
            paste("<b>not valid .xlsx file!<br></b>")			
          })
          file <- NULL
        }		
      } else {
        output$input_file_message <- renderText({ 
          paste("<b>not valid file!<hr></b>")			
        })			
        file <- NULL
      }
		  ds_tree <<- NULL
		  ds_fulltree <- NULL
		  ds_plain <<-NULL
		gc()
	  
      file
    })
  })	
  
  
  #############################3
  ### Takes care of the feature to highlight any given individual on the tree
  ### as long as he is in the tree
  ### The individual is seletect by his name
  observeEvent( input$sel_ind, {
    
    if (input$sel_ind != "" ) {
      #print("someone selected")
      the_sel_ind_id <<- unique( ds[ ds$name == input$sel_ind, ]$personid  )
    } else {
      #print("no one selected")	
      the_sel_ind_id <<- ""			
    }
  }  )
  
  
  #########################################	
  ## Activated when we change the input file that was selected
  ## puts file in the data frame 'ds'	
  observeEvent( input$genFile, { 
    
    #print("IN - observeEvent( input$genFile, { ")
    ds <<- data_file_w()
    if ( is.null( ds ) ) {		
      return
    } else {
      
      ################################### 
      ## Sometimes empty strings in excel are read as NA other times as ""
      ## I guess it depends on having edited each cell, or not
      ## (it was difficult to take care of this issue!!!)
      
      ## (step 1) trims white spaces in the ids
      ds$motherid <<- trimws(ds$motherid)  
      ds$fatherid <<- trimws(ds$fatherid)
      ## (step 2) replaces empty cells by NA
      if ( nrow( ds[(ds$motherid == "" & !is.na(ds$motherid) ),] ) > 0 ) ds[(ds$motherid == "" & !is.na(ds$motherid) ),]$motherid <<- NA
      if ( nrow( ds[(ds$fatherid == "" & !is.na(ds$fatherid) ),] ) > 0 ) ds[(ds$fatherid == "" & !is.na(ds$fatherid) ),]$fatherid <<- NA
      
      ## updates comboboxes with individuals ids and names
      updateSelectInput(session, "root_1",  choices = ds$personid, selected =  ds[1,]$personid )
      updateSelectInput(session, "root_1_name",  choices = ds$name, selected =  ds[1,]$name )
      
      updateSelectInput(session, "root_2",  choices = ds$personid, selected =  ds[2,]$personid )		
      updateSelectInput(session, "root_2_name",  choices = ds$name, selected =  ds[2,]$name )
      
      updateSelectInput(session, "sel_ind",  choices = ds[ order(ds$name), ]$name, selected =  "" )
      
      ds_personid <<- ds$personid
      ds_name <<- ds$name
      ds_birthdate <<- ds$birthdate
      ds_deathdate <<- ds$deathdate
      ds_sex <<- ds$sex
      ds_motherid <<- ds$motherid	
      ds_fatherid <<- ds$fatherid							
    }			
  } )
  #################################	
  
  
  
  ###############################################	
  ## activates updtate button
  observeEvent( input$revert_plot, {
    #print("IN - observeEvent(input$revert_plot, { ")
    
    isolate ({		
      temp_id = c( input$root_1 , input$root_2  )
      revert_flag <<- TRUE
      
      if ( back_type == 2 ) {
        if ( back_id[1] != temp_id[1] | back_id[2] != temp_id[2] ) {
          
          updateSelectInput(session, "root_1",  selected =  back_id[1] )
          updateSelectInput(session, "root_2",  selected =  back_id[2] )		
          back_id <<- temp_id		
        }
      } else {
        if ( back_id[1] != temp_id[1] ) {
          
          updateSelectInput(session, "root_1",  selected =  back_id[1] )		
          back_id <<- temp_id					
        }	
      }					
    })		
    
  } )
  ########################################
  
  
  
  ###############################################	
  ## activates updtate button
  observeEvent( input$update_plot, {
    #print("IN - observeEvent(input$update_plot, { ")
    init_time <<- Sys.time ()
    
	isolate ({	
      validate ( need ( input$root_1 , 'need root' ) )
      
      if ( input$plotType == 2 ) {
        validate ( need ( input$root_2 , 'need root' ) )
      }
      
      plotInput ()
    })		
  } )
  ########################################	
  
  
  
  ##########################################
  ## Methods to update interface elements
  observeEvent (input$root_1_name,{
#     print("IN - observeEvent (input$root_1_name,{")
    
    ## the following conditions are necessary because of repeated names
    ## and because of the initialization of the interface
    if ( nrow(ds) > 0 & input$root_1_name != ""){ ## only after selecting the file

      ## and only updates the id if it is inconsistent with the selected name
      if( ds[ds$personid == input$root_1,]$name != input$root_1_name ){ 
        updateSelectInput(session, "root_1", selected = ds[ds$name==input$root_1_name,]$personid )
      }
    }
  })
  ###

  ##########################################  
  observeEvent (input$root_1,{
 #   print("IN - observeEvent (input$root_1,{")
    updateSelectInput(session, "root_1_name", selected = ds[ds$personid==input$root_1,]$name )	
  })
  ###
  
  ##########################################  
  observeEvent (input$root_2_name,{
  #  print("IN - observeEvent (input$root_2_name,{")
    
    if ( nrow(ds)>0 & input$root_2 != ""){
      if( ds[ds$personid == input$root_2,]$name != input$root_2_name ){
        updateSelectInput(session, "root_2", selected = ds[ds$name==input$root_2_name,]$personid )
      }
    }
  })	
  ###

  ##########################################  
  observeEvent (input$root_2,{
    # print("IN - observeEvent (input$root_2,{")
    updateSelectInput(session, "root_2_name", selected = ds[ds$personid==input$root_2,]$name )
  })
  ###
  
  
  
  #######################################
  observeEvent(input$default_plot_file,{
    if ( input$default_plot_file  == "yes"  ) {
      
      date_today <- format(Sys.Date(), "%Y-%m-%d")
      image_file_name <<- paste( "VisAC_v1.0_", names_for_filename ,"_g" , input$generations,"_",date_today, sep="")	
      image_file_name <<- iconv(image_file_name, from = 'UTF-8', to = 'ASCII//TRANSLIT')
      updateTextInput(session, "file_name", value = image_file_name )
    } else {
      image_file_name <<- input$file_name
    }		
    
  })
  
  ###########################################################
  ### Reponds to the button to save the plot
  observeEvent(input$save_plot,{
    #print("IN - observeEvent(input$save_plot,{")
    
    withProgress(message = "Saving the file", detail="", value=0.9,{
      
		if( input$page_size == "A4" ){
        plot_height <<- 210
        plot_width  <<- 297	
                
      }else if( input$page_size == "A3" ){
        plot_height <<- 291
        plot_width  <<- 420 
        
      }else if( input$page_size == "A2" ){
        plot_height <<- 420
        plot_width  <<- 594 
		
      }else if( input$page_size == "GR" ){
        plot_height <<- 210
        plot_width  <<- 210*1.61 			
      }
      if ( input$default_plot_file  == "yes"  ) {
        
        data_hoje <- format(Sys.Date(), "%Y-%m-%d")
        image_file_name <<- paste( "VisAC_v1.0_", names_for_filename ,"_g" , input$generations,"_",data_hoje, sep="")	
        image_file_name <<- iconv(image_file_name, from = 'UTF-8', to = 'ASCII//TRANSLIT')
        updateTextInput(session, "file_name", value = image_file_name )
      } else {
        image_file_name <<- input$file_name
      }		
      the_plot_2_print_no_guides <- the_plot_2_print + guides(shape = "none", fill = "none")
      if(input$file_type == "jpg"){
        image_file_name <- paste(image_file_name,".jpg", sep="")
        
        ggsave(image_file_name, the_plot_2_print_no_guides, width = plot_width , height = plot_height, units=plot_units, dpi=as.numeric(input$file_resolution)  )
        
      }else if (input$file_type == "pdf") {
        image_file_name <- paste(image_file_name,".pdf", sep="")
        ggsave(image_file_name, the_plot_2_print_no_guides, device="pdf", width = plot_width , height = plot_height, units=plot_units)
      }else if (input$file_type == "png") {
        
        image_file_name <- paste(image_file_name,".png", sep="")
        ggsave(image_file_name, the_plot_2_print_no_guides, width = plot_width , height = plot_height, units=plot_units, dpi=as.numeric(input$file_resolution)  )			}
      
    }) # end ... withProgress
  })
  ##############################################
 
 
  ###########################################################
  ### Reponds to the button to save the settings
  observeEvent(input$save_settings,{
    #print("IN - observeEvent(input$save_settings,{")
    
    vect_len <- 73
    set_var = vector (length = vect_len )
    set_val1 = vector (length = vect_len )
    set_val2 = vector (length = vect_len )
    withProgress(message = "Saving settings", detail="", value=0.9,{
      
      
      ## Grande confusão com as checkboxGroupInput
      ## se nenhum está selecionado o vetor dá NULL e se um está as cells dão NA
      set_var[1]  = "IC"	
      if ( !is.null( input$IC ) ) {
        set_val1[1] = ifelse( !is.na( input$IC[1] ), input$IC[1] , FALSE ) 
        set_val2[1] = ifelse( !is.na( input$IC[2] ), input$IC[2] , FALSE ) 
      } else {
        set_val1[1] = FALSE
        set_val2[1] = FALSE
      }
      
      set_var[2]  = "max_IC" 
      set_val1[2] = input$max_IC
      
      set_var[3]  = "print_names"
      if ( !is.null( input$print_names ) ) {
        
        set_val1[3] = ifelse( !is.na( input$print_names[1] ), input$print_names[1] , FALSE ) 
        set_val2[3] = ifelse( !is.na( input$print_names[2] ), input$print_names[2] , FALSE ) 
      } else {
        set_val1[3] = FALSE
        set_val2[3] = FALSE
      }
      
      set_var[4]= "names_FM"
      if ( !is.null( input$names_FM ) ) {
        set_val1[4] = ifelse( !is.na( input$names_FM[1] ), input$names_FM[1] , FALSE ) 
        set_val2[4] = ifelse( !is.na( input$names_FM[2] ), input$names_FM[2] , FALSE ) 
      } else {
        set_val1[4] = FALSE
        set_val2[4] = FALSE
      }		
      
      set_var[5]= "print_title"
      if ( !is.null( input$print_title ) ) {
        set_val1[5] = ifelse( !is.na( input$print_title[1] ), input$print_title[1] , FALSE ) 
        set_val2[5] = ifelse( !is.na( input$print_title[2] ), input$print_title[2] , FALSE ) 
      } else {
        set_val1[5] = FALSE
        set_val2[5] = FALSE
      }
      
      set_var[6]  = "default_titles" 
      set_val1[6] = input$default_titles
      
      set_var[8]  = "title_font" 
      set_val1[8] = input$title_font
      
      set_var[9]  = "subtitle_font" 
      set_val1[9] = input$subtitle_font
      
      set_var[10]  = "font_axis_title" 
      set_val1[10] = input$font_axis_title		
      
      set_var[11]  = "font_axis_text" 
      set_val1[11] = input$font_axis_text
      
      set_var[12]  = "line_thick" 
      set_val1[12] = input$line_thick
      
      set_var[13]  = "line_thin" 
      set_val1[13] = input$line_thin
      
      set_var[14]  = "arrow_size" 
      set_val1[14] = input$arrow_size
      
      set_var[15]  = "arrow_angle" 
      set_val1[15] = input$arrow_angle
      
      set_var[16]  = "circle_size" 
      set_val1[16] = input$circle_size
      
      set_var[17]  = "file_type" 
      set_val1[17] = input$file_type
      
      set_var[18]  = "file_resolution" 
      set_val1[18] = input$file_resolution
      
      set_var[19]  = "page_size" 
      set_val1[19] = input$page_size
      
      set_var[21]  = "highlight_factor" 
      set_val1[21] = input$highlight_factor
      
      set_var[22]  = "white_line_factor" 
      set_val1[22] = input$white_line_factor
      
      set_var[23]  = "white_line" 
      set_val1[23] = input$white_line		
      
      set_var[24]  = "use_arrows" 
      set_val1[24] = input$use_arrows	
      
      set_var[27]  = "plotType" 
      set_val1[27] = input$plotType
      
      set_var[28]  = "generations" 
      set_val1[28] = input$generations
      
      set_var[29]  = "RC"	
      if ( !is.null( input$IC ) ) {
        set_val1[29] = ifelse( !is.na( input$RC[1] ), input$RC[1] , FALSE ) 
        set_val2[29] = ifelse( !is.na( input$RC[2] ), input$RC[2] , FALSE ) 
      } else {
        set_val1[29] = FALSE
        set_val2[29] = FALSE
      }
      
      set_var[30]  = "max_RC" 
      set_val1[30] = input$max_RC
      
      set_var[31]  = "kinship" 
      set_val1[31] = input$kinship	
      
      set_var[32]  = "names_font" 
      set_val1[32] = input$names_font	
      
      set_var[33]  = "ids_font" 
      set_val1[33] = input$ids_font	
      
      set_var[34]  = "kinships_font" 
      set_val1[34] = input$kinships_font	
      
      set_var[35]  = "ic_font" 
      set_val1[35] = input$ic_font	
      
      set_var[36]  = "name_alg_F" 
      set_val1[36] = input$name_alg_F		
      
      set_var[37]  = "name_x_F" 
      set_val1[37] = input$name_x_F		
      
      set_var[38]  = "name_y_F" 
      set_val1[38] = input$name_y_F
      
      set_var[39]  = "name_alg_M" 
      set_val1[39] = input$name_alg_M		
      
      set_var[40]  = "name_x_M" 
      set_val1[40] = input$name_x_M		
      
      set_var[41]  = "name_y_M" 
      set_val1[41] = input$name_y_M		
      
      set_var[42]  = "id_x" 
      set_val1[42] = input$id_x			
      
      set_var[43]  = "id_y" 
      set_val1[43] = input$id_y		
      
      set_var[44]  = "id_alg_F" 
      set_val1[44] = input$id_alg_F		
      
      set_var[45]  = "id_alg_M" 
      set_val1[45] = input$id_alg_M				
      
      set_var[46]  = "ic_x" 
      set_val1[46] = input$ic_x			
      
      set_var[47]  = "ic_y" 
      set_val1[47] = input$ic_y			
      
      set_var[48]  = "ic_alg" 
      set_val1[48] = input$ic_alg		
      
      set_var[49]  = "kinship_x" 
      set_val1[49] = input$kinship_x			
      
      set_var[50]  = "kinship_y" 
      set_val1[50] = input$kinship_y			
      
      set_var[51]  = "kinship_alg" 
      set_val1[51] = input$kinship_alg		
      
      set_var[52]  = "names_inc" 
      set_val1[52] = input$names_inc	
      
      set_var[53]  = "all_names" 
      set_val1[53] = input$all_names
      
      set_var[54]  = "all_name_alg" 
      set_val1[54] = input$all_name_alg		
      
      set_var[55]  = "all_names_ger" 
      set_val1[55] = input$all_names_ger			
      
      set_var[56]  = "all_names_font" 
      set_val1[56] = input$all_names_font		
      
      set_var[57]  = "all_name_x" 
      set_val1[57] = input$all_name_x
      
      set_var[58]  = "all_name_y" 
      set_val1[58] = input$all_name_y			
      
      set_var[59]  = "male_color" 
      set_val1[59] = input$male_color		
      
      set_var[60]  = "female_color" 
      set_val1[60] = input$female_color		
      
      set_var[61]  = "fill_male" 
      set_val1[61] = input$fill_male		
      
      set_var[62]  = "fill_female" 
      set_val1[62] = input$fill_female		
      
      set_var[63]  = "fill_kinship" 
      set_val1[63] = input$fill_kinship		
      
      set_var[64]  = "fill_inb" 
      set_val1[64] = input$fill_inb		
      
      set_var[65]  = "fill_rel" 
      set_val1[65] = input$fill_rel				
      
      set_var[66]  = "high_labels" 
      set_val1[66] = input$high_labels
      
      set_var[67]  = "high_labels_x" 
      set_val1[67] = input$high_labels_x
      
      set_var[68]  = "high_labels_y" 
      set_val1[68] = input$high_labels_y
      
      set_var[69]  = "ca_circle_size" 
      set_val1[69] = input$ca_circle_size
      
      set_var[70]  = "ks_circle_size" 
      set_val1[70] = input$ks_circle_size
      
      set_var[71]  = "alpha_labels" 
      set_val1[71] = input$alpha_labels
      
      set_var[72]  = "kinship_limit" 
      set_val1[72] = input$kinship_limit		
      
      set_var[73]  = "inb_circle_size" 
      set_val1[73] = input$inb_circle_size
      
      settings_df <- data.frame ( set_var , set_val1, set_val2 )
      write_csv ( settings_df , "VisAC_settings.csv")
      
    }) # end ... withProgress
  })
  ##############################################	

  
  ###########################################################
  ### Reponds to the button to reset settings
  observeEvent(input$reset_settings,{
    #print("IN - observeEvent(input$reset_settings,{")
    
    updateCheckboxGroupInput( session , "IC" , selected = c ( "inc", FALSE ) )		
    updateCheckboxGroupInput( session , "print_names" , selected = c( "names", FALSE ) )	
    updateCheckboxGroupInput( session , "names_FM" , selected = c("F","M") )	
    
    updateNumericInput ( session , "max_IC" , value = 0.231 )
    updateRadioButtons ( session , "default_titles" , selected = "yes" )
    updateNumericInput ( session , "title_font" , value =  16 )	
    updateNumericInput ( session , "subtitle_font" , value =  12 )	
    updateNumericInput ( session , "font_axis_title" , value = 12 )	
    updateNumericInput ( session , "font_axis_text" , value =  10 )	
    updateNumericInput ( session , "line_thick" , value =  0.6 )	
    updateNumericInput ( session , "line_thin" , value =  0.3 )	
    updateNumericInput ( session , "arrow_size" , value = 0.5 )	
    updateNumericInput ( session , "arrow_angle" , value = 6 )	
    updateNumericInput ( session , "circle_size" , value = 3.5 )	
    updateSelectInput( session, "file_type",  selected = "jpg" )
    updateSelectInput( session, "file_resolution",  selected =  "600" )
    updateSelectInput( session, "page_size",  selected =  "A3" )
    updateSelectInput( session, "highlight_factor",  selected =  1.5 )
    updateSelectInput( session, "white_line_factor",  selected =  0.3 )
    updateSelectInput( session, "white_line",  selected =  "yes" )
    
    updateRadioButtons( session , "use_arrows" , selected = "yes" )		
    updateRadioButtons( session , "plotType" , selected = "1" )
    updateNumericInput ( session , "generations" , value =  5 )	
    .
    updateCheckboxGroupInput( session , "RC" , selected = c ( "inc", FALSE ) )		
    updateNumericInput ( session , "max_RC" , value = 0.231 )
    updateRadioButtons ( session , "kinship" , selected = "yes" )
    
    updateNumericInput( session , "names_font",  value = 4 )	
    updateNumericInput( session , "ids_font",  value = 4 )	
    updateNumericInput( session , "kinships_font",  value = 4 )	
    updateNumericInput( session , "ic_font",  value = 4 )	
    
    updateRadioButtons ( session , "name_alg_F" , selected = "left" )
    updateNumericInput ( session , "name_x_F" , value = 0 )
    updateNumericInput ( session , "name_y_F" , value = -2 )
    
    updateRadioButtons ( session , "name_alg_M" , selected = "right" )
    updateNumericInput ( session , "name_x_M" , value = 0 )
    updateNumericInput ( session , "name_y_M" , value = 2 )		
    
    updateNumericInput ( session , "id_x" , value = 0 )		
    updateNumericInput ( session , "id_y" , value = 2 )		
    updateRadioButtons ( session , "id_alg_F" , selected = "left" )
    updateRadioButtons ( session , "id_alg_M" , selected = "right" )
    
    updateNumericInput ( session , "ic_x" , value =  -1.0)		
    updateNumericInput ( session , "ic_y" , value = 0.0 )		
    updateRadioButtons ( session , "ic_alg" , selected = "right" )	
    
    updateNumericInput ( session , "kinship_x" , value = 0 )		
    updateNumericInput ( session , "kinship_y" , value = -2.0)		
    updateRadioButtons ( session , "kinship_alg" , selected = "center" )			
    updateRadioButtons ( session , "names_inc" , selected = "path" )
    
    updateRadioButtons ( session , "all_names" , selected = "yes" )
    updateRadioButtons ( session , "all_name_alg" , selected = "center" )
    updateNumericInput ( session , "all_names_ger" , value = 4 )
    updateNumericInput ( session , "all_names_font" , value = 3 )
    updateNumericInput ( session , "all_name_x" , value = 0 )
    updateNumericInput ( session , "all_name_y" , value = -2.0 )		
    
    updateTextInput (session, "male_color", value = "#0072B7") 
    updateTextInput (session, "female_color",  value = "#FF3030") 
    updateTextInput (session, "fill_male",  value = "#EDF1FF")
    updateTextInput (session, "fill_female",  value = "#F7E8E1" )  
    updateTextInput (session, "fill_kinship",  value = "#F5DF4D")  
    updateTextInput (session, "fill_inb",  value = "#7FD37F") 
    updateTextInput (session, "fill_rel",  value = "#B9EAB3") 	
    
    updateRadioButtons ( session , "high_labels" , selected = "yes" )
    updateNumericInput ( session , "high_labels_x" , value = 2.0 )
    updateNumericInput ( session , "high_labels_y" , value = 2.0 )	
    
    updateNumericInput ( session , "ca_circle_size" , value = 5 )	
    updateNumericInput ( session , "ks_circle_size" , value = 4 )	
    updateNumericInput ( session , "alpha_labels" , value = 0.80 )	
    updateNumericInput ( session , "kinship_limit" , value = 5 )		
    updateNumericInput ( session , "inb_circle_size" , value = 2.5 )	
    
    
  } )
  
  
  
  #############################################################################
  ###	
  
  plotInput <- function(){
    
    gc()		
# print( "plotInput <- function()")
    
	if ( !is.null ( ds )  ) { ## only if the .xlsx file is valid


      isolate ({
	  
        id <- NULL
        prev_type <<- plot_type
        plot_type <<- input$plotType
        
		
        ## Checks if the rever_plot button was pressed before pressing the update_plot
       
	   if ( revert_flag == FALSE & input$update_plot > 1) { ## Not reverting and it is not the first plot
		  back_type <<- prev_type 	## backups the previous type	

		  ## important: only changes what is stored if there was a new id(s) selection(s)
          if ( back_type == 2 ) {
  
            if ( pkg.globals$ID_inicial[1] != input$root_1 | pkg.globals$ID_inicial[2] != input$root_2 ) {					            
              back_id <<-	pkg.globals$ID_inicial			
            }
			
          } else if ( back_type == 1 ) {
   
            if ( pkg.globals$ID_inicial[1] != input$root_1  ) {					
              back_id  <<-	pkg.globals$ID_inicial			
            }										
          }

        } else if ( input$update_plot == 1 ) {  ## first plot!
			## the first time the plot is created we need to initialize the variables.
			## In particular if the plot is created with the default ids

			back_type <<- input$plotType  ## backups the initial plot type and the corresponding root(s)
			if ( back_type == 1 ) {
				back_id <<-	c( input$root_1 )			
			} else {
				back_id <<-	c( input$root_1 , input$root_2 )			
			}	
		}	
	
        revert_flag <<- FALSE
      
        if ( input$plotType == 2) {
          num_generations <<-  ( input$generations + 2 )
        } else {
          num_generations <<- ( input$generations + 1 )
        }
        
      }) ## end ... isolate()
     
      ## task:
      ## checks if there is a brush selection
      brush <- input$the_plot_brush	
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
      
      ## gets settings from the interface settings	
      file_type <<- isolate( input$file_type  )
      title_font <- isolate( input$title_font )
      subtitle_font <<- isolate( input$subtitle_font )
      line_thick <<- isolate( input$line_thick )
      line_thin <<- isolate( input$line_thin )
      circle_size <<- isolate( input$circle_size )
      plot_resolution <<- isolate( input$file_resolution )						
      local_name <- ""
      font_size <<- isolate( input$labels_font )		
      
      male_color <<- isolate( input$male_color )
      female_color <<- isolate( input$female_color )
      fill_male <<- isolate( input$fill_male )
      fill_female <<- isolate( input$fill_female )
      fill_kinship <<- isolate( input$fill_kinship )
      fill_inb <<- isolate( input$fill_inb )
      fill_rel <<- isolate( input$fill_rel )
      
      labels_alpha <<- isolate( input$alpha_labels )	
      ca_circle_size <<- isolate( input$ca_circle_size )	
      ks_circle_size <<- isolate( input$ks_circle_size )			
      inb_circle_size <<- isolate( input$inb_circle_size )			
      
      isolate( {
        
        ## Checks the type of plot (1 or 2 roots) and 
        if( plot_type == 1 ){
          id = c(  input$root_1 )
          main_person = id
          local_name <- gsub(" ","_", short_name(   input$root_1_name , 2 ) ) 
          
        }else if( plot_type == 2){
          
          local_name   <- gsub(" ","_", short_name(  input$root_1_name  , 2 ) )
          local_name_2 <- gsub(" ","_", short_name(  input$root_2_name  , 2 ) )
          
          local_name <- paste (local_name,local_name_2, sep ="")
          
          id = c( input$root_1 , input$root_2  )
          main_person <- paste( input$root_1  , "_" ,  input$root_2  , sep="")
        }				
        
        ## checks and sets the file name 					
        if ( input$default_plot_file  == "yes"  ) {
          
          names_for_filename <<- paste( local_name ,"-", main_person , sep="")
          data_hoje <- format(Sys.Date(), "%Y-%m-%d")
          image_file_name <<- paste( "VisAC_v1.0",names_for_filename,"_g", input$generations,"_",data_hoje, sep="")	
          image_file_name <- iconv(image_file_name, from = 'UTF-8', to = 'ASCII//TRANSLIT')
          updateTextInput(session, "file_name", value = image_file_name )
        } else {
          image_file_name <<- input$file_name
        }
        
        
        ## updates the plot title and subtitle
        if ( input$default_titles == "yes" ) {
          title_plot <<- ( paste( input$generations , "Generations Family Tree for",(ds[ds$personid == id[1],]$name)) )
          
          if ( plot_type == 2 ) {
            title_plot <<- paste( title_plot, "and", ds[ds$personid == id[2],]$name )
          }

          sub_title <<- paste("printed in:", format(Sys.time(), format="%d %B %Y")) 
          
          updateTextInput(session, "plot_title", value = title_plot )
          updateTextInput(session, "plot_subtitle", value = sub_title )			
          
        } else {
          title_plot <<- input$plot_title
          sub_title <<- input$plot_subtitle			
        }
        
      }) ## end ... isolate()
       
      #########################################################################
      ## This instruction calls a method that computes the y coordinate for 
      ## each individual in the tree
      
      if( ordem_gen != num_generations ){  ## to make sure that the method only runs as few times as possible
        
        ordem_start <- Sys.time ()
        # print("UPDATING Ordem")
        ordem <<- tree_ordering( num_generations )
        ordem_gen <<- num_generations
        num_pessoas <<- nrow( ordem )  ## maximum number of individuals in the tree
        
        if (plot_type == 2){
          ordem <<- ordem[-1,]   ## If it is a plot for two individuals we need to remove the first row
          num_pessoas <<- num_pessoas - 1
        }
        
        ordem_end <- Sys.time ()
if ( print_time == TRUE) print( paste ("Time - Ordem:" , difftime(ordem_end, ordem_start, units='mins')  ) )
         
      }
      pkg.globals$ID_inicial <- id
      
      ############################################################
      
#  print( "IN - plotInput <- reactive({")	
      
      ## Vectorized version of the method (we use vectors instead of a dataframe)
      ds_tree_id <- rep(NA, num_pessoas)       ## individual id
      ds_tree_ordem  <- rep(NA, num_pessoas)	 ## indicates the position in the y axis of the individual
      ds_tree_father <- rep(NA, num_pessoas)	 ## father 
      ds_tree_mother <- rep(NA, num_pessoas)   ## mother
      ds_tree_sex <- rep(NA, num_pessoas)      ## sex  
      ds_tree_name <- rep(NA, num_pessoas)     ## name
      ds_tree_yearrange <- rep(NA, num_pessoas)
      ds_tree_fullname <- rep(NA, num_pessoas)     ## name
      ds_tree_yearrange <- rep(NA, num_pessoas)     ## name
      ds_tree_couple <- rep(NA, num_pessoas)     ## name
      ds_tree_link_from_child <- rep(NA, num_pessoas)     ## name	
      ds_tree_geracao  <- rep(NA, num_pessoas) ## generation count from the root individual
      
      ## each person corresponds to a line from (x,y) to (x2,y2)
      ds_tree_x  <- rep(NA, num_pessoas)		 ## each person is 	
      ds_tree_y  <- rep(NA, num_pessoas)
      ds_tree_x2  <- rep(NA, num_pessoas)
      ds_tree_x2_raw  <- rep(NA, num_pessoas)
      ds_tree_y2  <- rep(NA, num_pessoas)
      ds_tree_count  <- rep(NA, num_pessoas)   ## counts the number of times the individual occurs in the tree

      
      pkg.globals$dst_id <- rep(NA, num_pessoas)       ## individual id
      pkg.globals$dst_father <- rep(NA, num_pessoas)	 ## father 
      pkg.globals$dst_mother <- rep(NA, num_pessoas)   ## mother
      pkg.globals$dst_sex <- rep(NA, num_pessoas)      ## sex  
      pkg.globals$dst_child <- rep(NA, num_pessoas)     ## name	
      pkg.globals$dst_count  <- rep(NA, num_pessoas) ## counts the number of times the individual occurs in the tree
      pkg.globals$dst_to_color  <- rep(NA, num_pessoas) ## counts the number of times the individual occurs in the tree
      pkg.globals$dst_common_highlight <- rep(NA, num_pessoas)
      pkg.globals$dst_path_to_both <- rep(NA, num_pessoas)
	  pkg.globals$dst_to_highlight <- rep(NA, num_pessoas)
	  pkg.globals$dst_highlight_count <- rep(NA, num_pessoas)


 ## this dataframe is to speed up the data preparation for the inbreeding function      
      ds_fulltree_id <- rep(NA, num_pessoas)       ## individual id
      ds_fulltree_father <- rep(NA, num_pessoas)	 ## father 
      ds_fulltree_mother <- rep(NA, num_pessoas)   ## mother
      ds_fulltree_sex <- rep(NA, num_pessoas)      ## sex  
      ds_fulltree_geracao  <- rep(NA, num_pessoas) ## generation count from the root individual
      ds_fulltree_dob <- rep(NA, num_pessoas)

## keeps data for the plain-tree
      ds_plain_id <- rep(NA, num_pessoas)       ## individual id
	  ds_plain_name <- rep(NA, num_pessoas)       
	  ds_plain_sex <- rep(NA, num_pessoas)      ## sex  
      ds_plain_geracao  <- rep(NA, num_pessoas) ## generation count from the root individual
	  ds_plain_x  <- rep(NA, num_pessoas)		 ## each person is 	
	  ds_plain_y  <- rep(NA, num_pessoas)
	  ds_plain_x2  <- rep(NA, num_pessoas)
	  ds_plain_y2  <- rep(NA, num_pessoas)
	  ds_plain_count  <- rep(NA, num_pessoas)      

gc()
          
      ordem_v <- ordem$ordem   ## vector with the orders
      neg_order <<- 0
      
      first_gen_x <- 0
      if (plot_type == 1) {
        first_gen_x <- 0.25
      }else if (plot_type == 2) {
        first_gen_x <- 0
      }
      couple_counter <- 0 ## new variable to count the number of couples
      
      ####################################	
      dstree_start <- Sys.time ()  ## for assessing running time
      
      ## Initialization of the root individuals (one or two depending on the type of plot) 
      ind_atual <- 1

    
      for ( p_ind in 1:length(pkg.globals$ID_inicial)){
        
        the_id <- pkg.globals$ID_inicial[p_ind]
        the_name <- ds_name[which(ds_personid == pkg.globals$ID_inicial[p_ind]) ]
        the_father <- ds_fatherid[which(ds_personid == pkg.globals$ID_inicial[p_ind]) ]
        the_mother <- ds_motherid[which(ds_personid == pkg.globals$ID_inicial[p_ind]) ]
        the_sex <- ds_sex[which(ds_personid == pkg.globals$ID_inicial[p_ind]) ]
        the_order <- ordem_v[ind_atual]
        
        ds_tree_id[ind_atual] <- the_id
        
        ds_tree_name[ind_atual] <- the_name
        ds_tree_fullname[ind_atual] <- the_name
        ds_tree_yearrange[ind_atual] <- paste("b: ", ds_birthdate[which(ds_personid == pkg.globals$ID_inicial[p_ind]) ], ", d: ", ds_deathdate[which(ds_personid == pkg.globals$ID_inicial[p_ind]) ],sep="")
        
        ds_tree_ordem[ind_atual] <- the_order
        ds_tree_geracao[ind_atual] <- 1		
        ds_tree_sex[ind_atual] <- the_sex	
        ds_tree_father[ind_atual] <- the_father	
        ds_tree_mother[ind_atual] <- the_mother
        
        ds_tree_couple[ind_atual] <- couple_counter  ## can be more than one initial persons but both have counter == 0	
        ds_tree_link_from_child[ind_atual] <- "main"
        
        
        ds_tree_x[ind_atual] <- 0
        ds_tree_y[ind_atual] <- the_order
        ds_tree_x2[ind_atual] <-  0
        ds_tree_x2_raw[ind_atual] <- 0
        ds_tree_y2[ind_atual] <- the_order    
        ds_tree_count[ind_atual] <- 1
        
## this dataframe is to speed up the data preparation for the inbreeding function
        ds_fulltree_id[ind_atual] <- the_id
        ds_fulltree_dob[ind_atual] <- get_year(ds[ds$personid == pkg.globals$ID_inicial[p_ind] , ]$birthdate, 1)
        ds_fulltree_geracao[ind_atual] <- 1	
        ds_fulltree_sex[ind_atual] <- the_sex
        ds_fulltree_father[ind_atual] <- the_father
        ds_fulltree_mother[ind_atual] <- the_mother

## 2022-02-18 we are going to include the std layout coordinates for a full tree		
        ds_plain_id[ind_atual] <- the_id
        ds_plain_geracao[ind_atual] <- 1	
        ds_plain_sex[ind_atual] <- the_sex
		ds_plain_name[ind_atual] <- the_name
		ds_plain_x[ind_atual] <- 0
		ds_plain_y[ind_atual] <- the_order
		ds_plain_x2[ind_atual] <- 0
		ds_plain_y2[ind_atual] <- the_order
        ds_plain_count[ind_atual] <- 1
        
        ind_atual <- ind_atual + 1  ## Next person to deal with
      } ### ... 	for ( p_ind in 1:length(pkg.globals$ID_inicial)){
      
      ind_anterior <- 1
      couple_counter <- couple_counter + 1	## increase counter	
       
      
      ## we had to adapt the internal variables in order to generalize the plot for two root individuals
      ## ger <- 1 corresponds to a unique root individual
      ## if we are creating a plot for two root individuals we have to skip ger == 1
      if (plot_type == 1) {
        ger_init <- 2
        
      }else if (plot_type == 2) {
        ger_init <- 3
      }
      
      ger_lag <- 2 ## correction lag to have the first individual with x==0
      
      for(ger in ger_init:num_generations ){  ## for each generation after the root individual(s)

        ## the number of persons in a generation depends only on the generation level (binary tree)
        ## For each person in the previous generation...
        for( j  in  ind_anterior: (ind_anterior - 1  + 2^(ger-2) ) ){ 
 
          ## j - is the person in the previous generation
          ## ind_atual - is a parent of the j person (father or mother)
          
          #####################################
          ## FIRST WE WILL DEAL WITH THE FATHER
          
          ## gets the person's father
          ds_tree_id[ind_atual] <- "I0000"  ## ID artificial to avoid missing values		
          ds_tree_count[ind_atual] <- 1     ## number of times the person occurs in the tree
          ds_tree_link_from_child[ind_atual] <- ds_tree_id[j]			
          
          ## x coordinate of the line ending for that person (in the previous generation)
          ds_tree_x2[ind_atual] <- ifelse(ger - ger_lag < 0, 0 + first_gen_x , ger - ger_lag + first_gen_x)    
          ds_tree_x2_raw[ind_atual] <- ifelse(ger - ger_lag < 0, 0 + first_gen_x , ger - ger_lag + first_gen_x)    

		  ds_plain_count[ind_atual] <- 1
		  ds_plain_x2[ind_atual] <- ds_tree_x2[ind_atual]
          
          the_count <- ds_tree_count[j] ## aux var to reduce the number of searches
          if( !is.na(ds_tree_father[j] )  ) {  ## if we know who is the father
            
            ## gets the father id, who will be inserted as an individual in the current generation
            f_id <- ds_tree_father[j]  
            
            ## set the y coordinate for the individual in the previous generation 
            ## (that must be connected to his child, the current individual)
            ds_tree_y[ind_atual] <- ds_tree_y2[j]
						
			ds_plain_y[ind_atual] <- ds_plain_y2[j]		
			ds_plain_y2[ind_atual] <- ordem_v[ind_atual]
            
            ## Checks if the father is being dealt with for the first time
            if( sum( na.omit(ds_tree_id == f_id) ) == 0){ 
              
              ds_tree_y2[ind_atual] <- ordem_v[ind_atual]
              ds_tree_count[ind_atual] <- 1
              
			  ds_plain_count[ind_atual] <- 1
			  
            } else{  ## In case the father already exists we need to get the first y coordinate
              
              ds_tree_y2[ind_atual]   <- na.omit(  ds_tree_y2[which(ds_tree_id == f_id )][1])
              
              ## if the first occurrence was in a different generation 
              ## we need to get the corresponding x coordinate and 
              ## give it a small deviation x_lag
              the_ger <- ds_tree_geracao[which(ds_tree_id == f_id)][1]  ## aux var to reduce the number of filterings
              
              if ( the_ger != ger ){						 
                if ( the_ger - ger_lag  < 0 ){
                  ds_tree_x2[ind_atual] <- 0 + first_gen_x
                  ds_tree_x2_raw[ind_atual] <- 0 + first_gen_x
                } else {
                  ds_tree_x2[ind_atual] <- the_ger - ger_lag  + first_gen_x				
                  ds_tree_x2_raw[ind_atual] <- the_ger - ger_lag  + first_gen_x
                }												
              }
              
              ## If we are here it is because the father occurred before
			  
			  ## for the fulltree we just need > 1 for all repeated individuals
			  ds_plain_count[ind_atual] <- 2
			  ds_plain_count[ which(ds_plain_id == f_id  & ds_plain_count == 1 )] <- 2
 			  
              if( the_count == 1 ) { ## if it is the first time the child occurs...
                
                ## Need to make sure that all occurrences have the count 2
                ## the condition ( ... & ds_tree_count == 1 ) is essential. Otherwise some links that
                ## were previously updated would be reset			  
                ds_tree_count[which(ds_tree_id == f_id  & ds_tree_count == 1 )] <- 2  										
                ds_tree_count[ind_atual] <- 2	## need this because the father was not yet inserted
                
              }else{ ## if it is the second, or more, time that the child occurs ...
                
                ds_tree_count[ind_atual] <- the_count + 1 ## this occurrence of the father is set an higher count				
              }		
              ## as a result, only when the same child-parent relationship occurs more than once the count will be
              ## higher than 2. Those cases do not need to be plot.
              #########################################################				
            }
            
            ## now we will insert the father id and sex
            ds_tree_id[ind_atual] <- f_id	## insere ID					
            ds_tree_sex[ind_atual] <- "M"		
            
            ds_fulltree_id[ind_atual] <- f_id	## insere ID					
            ds_fulltree_sex[ind_atual] <- "M"	

            ds_plain_id[ind_atual] <- f_id	## insere ID					
            ds_plain_sex[ind_atual] <- "M"	
             
            ## and now we will insert the father (grandfather) and mother (grandmother) of the current f_id
            if( length( ds_personid[ which(ds_personid == f_id) ]) > 0 ){  ## to make sure that the f_id exists in the GEDCOM file as an individual
              
              if( !is.na( ds_fatherid[ which( ds_personid == f_id ) ] ) ){
                
                ds_tree_father[ind_atual] <- (ds_fatherid[ which( ds_personid == f_id ) ])[1]	
                ds_fulltree_father[ind_atual] <- ds_tree_father[ind_atual]
              }
              if( !is.na( ds_motherid[ which( ds_personid == f_id ) ] ) ){
                
                ds_tree_mother[ind_atual] <- (ds_motherid[ which( ds_personid == f_id ) ])[1]	
                ds_fulltree_mother[ind_atual] <- ds_tree_mother[ind_atual]					
                
              }
              
              ds_tree_name[ind_atual] <- short_name( ds_name[ which( ds_personid == f_id ) ] , 2)
              ds_tree_yearrange[ind_atual] <- paste("b: ", ds_birthdate[ which( ds_personid == f_id ) ], ", d: ", ds_deathdate[ which( ds_personid == f_id ) ] ,sep="")
              ds_fulltree_dob[ind_atual] <- get_year ( ds_birthdate[ which( ds_personid == f_id ) ] , 1 )
              
              ds_tree_fullname[ind_atual] <-  ds_name[ which( ds_personid == f_id ) ]
			  
			  ds_plain_name[ind_atual] <- ds_tree_name[ind_atual]
            }	
            
            ds_tree_x[ind_atual] <- ds_tree_x2[j]
			if ( ger == ger_init ) {
				ds_plain_x[ind_atual] <- 0
			} else {
				ds_plain_x[ind_atual] <- ifelse( ger - ger_lag -1 < 0, 0 + first_gen_x , ger - ger_lag -1 + first_gen_x)
			}
          } else { ## end if father exists
            
            ds_tree_x[ind_atual] <- ifelse( ger - ger_lag -1 < 0, 0 + first_gen_x , ger - ger_lag -1 + first_gen_x)			
			ds_plain_x[ind_atual] <- ds_tree_x2[j]
          }		
          ## need to define these variables even if the father does not exist
          ## in order to keep the dataframe rownumber consistent with the indexes of the individuals in the tree
          
          ds_tree_couple[ind_atual] <- couple_counter
          ds_tree_geracao[ind_atual] <- ger
          ds_fulltree_geracao[ind_atual] <- ger	
          ds_plain_geracao[ind_atual] <- ger			  
          ds_tree_ordem[ind_atual] <- ordem_v[ind_atual]		
          
          #####################################
          ## SECOND, repeat the steps for the MOTHER
          
          ## update the variable to deal with the mother
          ## ind_atual is the mother of the j individual
          ind_atual <- ind_atual+1
          
          ds_tree_id[ind_atual] <- "I0000"  ## ID artificial to avoid missing values
          ds_tree_count[ind_atual] <- 1     ## number of times the person occurs in the tree
          ds_tree_link_from_child[ind_atual] <- ds_tree_id[j]			
          
          ## x coordinate of the line ending for that person (in the previous generation)
          ds_tree_x2[ind_atual] <- ifelse(ger - ger_lag <0, 0 + first_gen_x ,  ger - ger_lag + first_gen_x)
          ds_tree_x2_raw[ind_atual] <- ifelse(ger - ger_lag <0, 0 + first_gen_x ,  ger - ger_lag + first_gen_x)


		  ds_plain_count[ind_atual] <- 1
		  ds_plain_x2[ind_atual] <- ds_tree_x2[ind_atual]
           
          if( !is.na(ds_tree_mother[j] )  ) { ## if we know who is the mother
            
            ## gets the mother id, who will be inserted as an individual in the current generation
            m_id <- ds_tree_mother[j]  
            
            ## set the y coordinate for the individual in the previous generation 
            ## (that must be connected to his child, the current individual)			
            ds_tree_y[ind_atual] <- ds_tree_y2[j]
			

			ds_plain_y[ind_atual] <- ds_plain_y2[j]
			ds_plain_y2[ind_atual] <- ordem_v[ind_atual]
             
            ## Checks if the mother is being dealt with for the first time
            if( sum( na.omit(ds_tree_id == m_id)) == 0){ ## note that we only insert the mother below
              
              ds_tree_y2[ind_atual] <- ordem_v[ind_atual]
              ds_tree_count[ind_atual] <- 1	

			  ds_plain_count[ind_atual] <- 1
			  
            } else { ## in case the mother already exists we need to get the first coordinate
              ds_tree_y2[ind_atual]   <- na.omit(ds_tree_y2[which(ds_tree_id == m_id)])[1]	
              
              ## if the first occurrence was in a different generation 
              ## we need to get the corresponding x coordinate and 
              ## give it a small deviation x_lag	
              the_ger <-  ds_tree_geracao[which(ds_tree_id == m_id)][1] 					
              if ( the_ger != ger ){
                if ( the_ger - ger_lag  < 0 ){
                  ds_tree_x2[ind_atual] <- first_gen_x
                  ds_tree_x2_raw[ind_atual] <-  first_gen_x
                }else{
                  ds_tree_x2[ind_atual] <- the_ger - ger_lag  + first_gen_x
                  ds_tree_x2_raw[ind_atual] <- the_ger - ger_lag + first_gen_x
                }
              }				


			  ## for the fulltree we just need > 1 for all repeated individuals
			  ds_plain_count[ind_atual] <- 2
			  ds_plain_count[ which(ds_plain_id == m_id  & ds_plain_count == 1 )] <- 2
              
              ## If we are here it is because the mother occurred before	
              if( the_count == 1 ) {  ## if it is the first time the child occurs...
                
                ## Need to make sure that all occurrences of the mother have count 2
                ## the condition ( ... & ds_tree_count == 1 ) is essential. Otherwise some links that
                ## were previously updated would be reset
                ds_tree_count[which(ds_tree_id == m_id  & ds_tree_count == 1)] <- 2							
                ds_tree_count[ind_atual] <- 2	## need this because the mother was not yet inserted	

                
              }else{  ## if it is the second, or more, time that the child occurs...
                ds_tree_count[ind_atual] <- the_count + 1  ## this occurrence of the mother is set an higher count
                
              }				
              ## as a result, only when the same child-parent relationship occurs more than once the count will be
              ## higher than 2. Those cases do not need to be plot.
              #########################################################
              
            }		
            
            ## now we will insert the mother id and sex
            ds_tree_id[ind_atual] <- m_id
            ds_tree_sex[ind_atual] <- "F"			
            
            ds_fulltree_id[ind_atual] <- m_id
            ds_fulltree_sex[ind_atual] <- "F"	

            ds_plain_id[ind_atual] <- m_id
            ds_plain_sex[ind_atual] <- "F"	

            
            ## and now we will insert the father (grandfather) and mother (grandmother) of the current m_id
            if( length( ds_personid[ which(ds_personid == m_id ) ] ) > 0 ){
              
              if( !is.na( ds_fatherid[ which(ds_personid == m_id ) ] ) ){
                
                ds_tree_father[ind_atual] <- (ds_fatherid[ which(ds_personid == m_id ) ])[1]
                ds_fulltree_father[ind_atual] <- ds_tree_father[ind_atual]
              }
              if( !is.na(  ds_motherid[ which( ds_personid == m_id ) ] ) ){
                
                ds_tree_mother[ind_atual] <- (ds_motherid[ which(ds_personid == m_id ) ])[1]
                ds_fulltree_mother[ind_atual] <- ds_tree_mother[ind_atual]
              }			
              
              ds_tree_name[ind_atual] <- short_name( ds_name[ which(ds_personid == m_id ) ] ,2)
              ds_tree_fullname[ind_atual] <-  ds_name[ which(ds_personid == m_id ) ]
              ds_tree_yearrange[ind_atual] <- paste( "b: ", ds_birthdate[ which(ds_personid == m_id ) ] , ", d: ", ds_deathdate[ which(ds_personid == m_id ) ] ,sep="")
              ds_fulltree_dob[ind_atual] <-  get_year( ds_birthdate[ which(ds_personid == m_id ) ] , 1 )	

			  ds_plain_name[ind_atual] <- ds_tree_name[ind_atual]
            }
            
            ds_tree_x[ind_atual] <- ds_tree_x2[j]
            
			if ( ger == ger_init ) {
				ds_plain_x[ind_atual] <- 0
			} else {
				ds_plain_x[ind_atual] <- ifelse( ger - ger_lag -1 < 0, 0 + first_gen_x, ger - ger_lag -1 + first_gen_x)
			}			
          } else { ## end if mother exists			
            ds_tree_x[ind_atual] <- ifelse( ger - ger_lag -1 < 0, 0 + first_gen_x, ger - ger_lag -1 + first_gen_x)			
			ds_plain_x[ind_atual] <- ds_tree_x2[j]			
          }
          
          ## need to define these variables even if the mother does not exist
          ## in order to keep the dataframe rownumber consistent with the indexes of the individuals in the tree
          
          ds_tree_couple[ind_atual] <- couple_counter
          couple_counter <- couple_counter + 1
          ds_tree_ordem[ind_atual] <- ordem_v[ind_atual]
          ds_tree_geracao[ind_atual] <- ger
          ds_fulltree_geracao[ind_atual] <- ger			
          ds_plain_geracao[ind_atual] <- ger	
          ## update variable for the next level
          ind_atual <- ind_atual+1		
          
        } ### end ... for( j  in  ind_anterior: (ind_anterior - 1  + 2^(ger-2) ) ){
        
        ## The variable to count the number of persons in the previous generation have to take into
        ## account the type of plot (because of the initial generation that is removed for plot_type == 2)
        if (plot_type == 1){
          ind_anterior <- 2^(ger-1)
        } else if (plot_type == 2){
          ind_anterior <- 2^(ger-1) - 1
        }
      } ### end ... for(ger in ger_init:num_geracoes){ 
      
      ## Will keep this global vars for the paint functions
      ## I think it is enough to copy them at the end of the process of creating the ds_tree
      pkg.globals$dst_id <- ds_tree_id     ## individual id
      pkg.globals$dst_father <- ds_tree_father	 ## father 
      pkg.globals$dst_mother <- ds_tree_mother   ## mother
      pkg.globals$dst_sex <- ds_tree_sex      ## sex  
      pkg.globals$dst_child <- ds_tree_link_from_child     ## name	
      pkg.globals$dst_count  <- ds_tree_count ## counts the number of times the individual occurs in the tree
      gc()   
      
      
      ## concatenates the vectors into a dataframe for the ggplot method
      ds_tree <<- data.frame(id=ds_tree_id,ordem = ds_tree_ordem, father= ds_tree_father, mother= ds_tree_mother,geracao = ds_tree_geracao,x=ds_tree_x, y=ds_tree_y, x2 = ds_tree_x2,  y2 = ds_tree_y2, count = ds_tree_count,  name=ds_tree_name, sex = ds_tree_sex, fullname=ds_tree_fullname, yearrange=ds_tree_yearrange, couple =ds_tree_couple, child = ds_tree_link_from_child, x2_raw = ds_tree_x2_raw , stringsAsFactors=FALSE )	
 
# write_csv( ds_tree , file = "dst.csv") 

## full tree for computing the inbreeding coefficient
      ds_fulltree <- data.frame(id=ds_fulltree_id, father= ds_fulltree_father, mother= ds_fulltree_mother,geracao = ds_fulltree_geracao, dob = ds_fulltree_dob, sex = ds_fulltree_sex, stringsAsFactors=FALSE )
## plain tree for the tree without collapsing
      ds_plain <<- data.frame(id=ds_plain_id,geracao = ds_plain_geracao, sex = ds_plain_sex, name = ds_plain_name, x = ds_plain_x, y = ds_plain_y, x2=ds_plain_x2, y2=ds_plain_y2, count = ds_plain_count, stringsAsFactors=FALSE ) 
  
 
gc()

      ##############
      ############
      
      ### Check if the persons in the last generation have their parents already in the tree
      ### ( last generation but on the last level of the tree - x coordinate )
      
      ## data frame with individuals to check
      ds_last_id <- ds_tree_id[ which ( na.omit( ds_tree_geracao) == num_generations & na.omit( ds_tree_x2 )== num_generations - ger_lag + first_gen_x & ds_tree_id != "I0000" ) ]
      
      for ( i in 1:length( ds_last_id ) ) {  ## For each of the individuals
        local_fid <- ds_tree_father[ which ( na.omit( ds_tree_id ) == ds_last_id[i] ) ]
        ds_tree_new <- ds_tree_id[ which ( na.omit( ds_tree_id ) == local_fid[1] ) ]  ## check if the father is defined
        
        if ( length( ds_tree_new ) > 0 ) {
          
          ## creates the row corresponding to the link from the child to the father
          
		  ## local copy of ds_tree
          ds_tree_L <- ds_tree[ ds_tree$id ==  local_fid[1] ,]    ## check if the father is defined       
          ds_tree_L <- ds_tree_L[ 1 , ]
          
          neg_order <- neg_order - 1
          ds_tree_L$ordem = neg_order
          ds_tree_L$geracao <- ds_tree_L$geracao + 1
          ds_tree_L$x <-  (ds_tree_x2[  which ( na.omit( ds_tree_id ) == ds_last_id[i] & na.omit( ds_tree_geracao) == num_generations & na.omit( ds_tree_x2 )== num_generations - ger_lag + first_gen_x  ) ] )[1]
          ds_tree_L$x2 <- ds_tree_L$x2
          
          ds_tree_L$y <-  (ds_tree_y2[  which ( na.omit( ds_tree_id ) == ds_last_id[i] & na.omit( ds_tree_geracao) == num_generations & na.omit( ds_tree_x2 )== num_generations - ger_lag + first_gen_x ) ])[1]    
          ds_tree_L$child <- ds_last_id[i]
          ds_tree_L$count <- ds_tree_L$count + 1
          
          ds_tree[ds_tree$id == ds_tree_L$id, ]$count <<- ds_tree[ds_tree$id == ds_tree_L$id , ]$count + 1
          
          ##DST		
          pkg.globals$dst_count[pkg.globals$dst_id == ds_tree_L$id] <- ds_tree[ds_tree$id == ds_tree_L$id , ]$count + 1
          
          # and appends the link to the ds_tree
          ds_tree <<- rbind ( ds_tree, ds_tree_L[1,] )	
          
          pkg.globals$dst_id[ind_atual] <- ds_tree_L$id
          pkg.globals$dst_father[ind_atual] <- ds_tree_L$father
          pkg.globals$dst_mother[ind_atual] <- ds_tree_L$mother
          pkg.globals$dst_sex[ind_atual] <- ds_tree_L$sex
          pkg.globals$dst_child[ind_atual] <- ds_tree_L$child
          pkg.globals$dst_count[ind_atual] <- ds_tree_L$count
          ind_atual <- ind_atual+1		
        }
        
        
        local_mid <- ds_tree_mother[ which ( na.omit( ds_tree_id ) == ds_last_id[i] ) ]
        
        ds_tree_new <- ds_tree_id[ which ( na.omit( ds_tree_id ) == local_mid[1] ) ]  ## check if the father is defined
        
        if ( length( ds_tree_new ) > 0 ) {
          ## creates the row corresponding to the link from the child to the mother
          
          ds_tree_L <- ds_tree[ ds_tree$id ==  local_mid[1] ,]   ## check if the father is defined	
          ds_tree_L <- ds_tree_L[ 1 , ]
          
          neg_order <- neg_order - 1			
          ds_tree_L$ordem = neg_order
          ds_tree_L$geracao <- ds_tree_L$geracao + 1
          ds_tree_L$x <- (ds_tree_x2[  which ( ds_tree_id == ds_last_id[i] & na.omit( ds_tree_geracao) == num_generations & na.omit( ds_tree_x2 )== num_generations - ger_lag + first_gen_x ) ] )[1]
          ds_tree_L$x2 <- ds_tree_L$x2	
          
          ds_tree_L$y <- (ds_tree_y2[  which ( ds_tree_id == ds_last_id[i] & na.omit( ds_tree_geracao) == num_generations & na.omit( ds_tree_x2 )== num_generations - ger_lag + first_gen_x  ) ])[1]
          ds_tree_L$child <- ds_last_id[i]
          ds_tree_L$count <- ds_tree_L$count + 1		
          ds_tree[ds_tree$id == ds_tree_L$id, ]$count <<- ds_tree[ds_tree$id == ds_tree_L$id, ]$count + 1
          
          pkg.globals$dst_count[ pkg.globals$dst_id == ds_tree_L$id ] <- ds_tree[ds_tree$id == ds_tree_L$id, ]$count + 1
          
          # and appends the link to the ds_tree
          ds_tree <<- rbind ( ds_tree, ds_tree_L[1,] )	
          ###  DST
          pkg.globals$dst_id[ind_atual] <- ds_tree_L$id
          pkg.globals$dst_father[ind_atual] <- ds_tree_L$father
          pkg.globals$dst_mother[ind_atual] <- ds_tree_L$mother
          pkg.globals$dst_sex[ind_atual] <- ds_tree_L$sex
          pkg.globals$dst_child[ind_atual] <- ds_tree_L$child
          pkg.globals$dst_count[ind_atual] <- ds_tree_L$count
          ind_atual <- ind_atual+1			
        }		
      }	
      
      ds_local_tree <- ds_tree
      ds_local_tree$id <- as.factor( ds_local_tree$id )
      ds_local_tree$child <- as.factor( ds_local_tree$child )	
      ds_local_tree <- ds_local_tree %>% group_by( id ) %>% mutate( num_children = dplyr::n_distinct(child) ) %>% ungroup() %>% mutate (count = ifelse( num_children > 1 , 2, 1) ) %>% mutate ( count = ifelse(id =="I0000", 0, count) )
      ds_tree$count <<- ds_local_tree$count
      
      pkg.globals$dst_count <- ds_local_tree$count
       
      ## removing duplicated links 
      
      vect_check <- duplicated( ds_tree[,c("id","child","x","y","x2","y2") ] )
      
      ds_tree <<- ds_tree[ !duplicated( ds_tree[,c("id","child","x","y","x2","y2") ]) , ]
      
      pkg.globals$dst_id <- pkg.globals$dst_id[ !vect_check ]
      pkg.globals$dst_father <- pkg.globals$dst_father[ !vect_check ]
      pkg.globals$dst_mother <- pkg.globals$dst_mother[ !vect_check ]
      pkg.globals$dst_sex <- pkg.globals$dst_sex[ !vect_check ]
      pkg.globals$dst_child <- pkg.globals$dst_child[ !vect_check ]
      pkg.globals$dst_count <- pkg.globals$dst_count[ !vect_check ]		  
      
      #######
      #####
      dstree_end <- Sys.time ()
if ( print_time == TRUE)       print ( paste ( "Time - dstree:" ,  difftime( dstree_end, dstree_start , units='mins')  ) )
if ( print_time == TRUE)  print( paste ("GLOBAL Time - dstree_end:" , dstree_end - init_time ) )
     
 
## new vars for the ds_tree 
      ds_tree$to_highlight <<- 0 ## new variable for the highlighting interaction
      ds_tree$highlight_count <<- 0
      ds_tree$kinship <<- "undefined"
      ds_tree$kinship_shape <<- 0
      ds_tree$kinship_y1 <<- 0
      ds_tree$kinship_ancestor <<- ""
      ds_tree$kinship_relative <<- ""
      ds_tree$kinship_y <<- ""
      ds_tree$kinship_x <<- ""
      ds_tree$kinship_child <<- ""
      ds_tree$kinship_highlight <<- ""
      ds_tree$common_highlight <<- FALSE      
      ds_tree$to_color <<- 1  ## sets everyone with the same colour
      ds_tree$in_path_to_root1 <<- FALSE
      ds_tree$in_path_to_root2 <<- FALSE
      ds_tree$in_path_to_both <<- FALSE	  
	  
      pkg.globals$dst_to_color <- ds_tree$to_color
      pkg.globals$dst_common_highlight <- ds_tree$common_highlight  ## Here I am setting the vector to FALSE 
      pkg.globals$dst_path_to_both <- ds_tree$common_highlight  ## Here I am setting the vector to FALSE
      
      dstreeunique_start <- Sys.time ()
      
      ds_t <<- unique( ds_tree[ds_tree$count == 2 & ds_tree$child != "main"   , c("id","name","father","mother")] )
      ds_t <<- ds_t[ order(ds_t$name) , ]
       
      
      dsnames_start <- Sys.time ()
      
      ## selects individuals with two or more occurrences for placing the names
      ## The condition (... & !ds_tree$id %in% pkg.globals$ID_inicial) removes from the list the initial individuals
      ## who are placed seperatly (root)
      ds_names <<- ds_tree[ds_tree$count == 2 & ds_tree$geracao > 1 & !ds_tree$id %in% pkg.globals$ID_inicial ,c("id","name","x2","y2","sex","geracao")] 
      ds_names <<- ds_names[!duplicated(ds_names[1]),]
      
      
      dsnames_end <- Sys.time ()
 if ( print_time == TRUE)      print ( paste ( "Time - dsnames:" , difftime( dsnames_end , dsnames_start , units='mins')   ) )
     
      
      ## calls method that signals all individuals to be signaled as repeated
      if (nrow(ds_t) > 0){
        
        if ( plot_type == 1 ) { # only when 2-roots
          
          paint_simple_dst_start <- Sys.time ()			
          paint_simple_dst(ds_t$id)
          paint_simple_dst_end <- Sys.time ()			
if ( print_time == TRUE)           print ( paste ( "Time - Paint simple DST:" , difftime( paint_simple_dst_end , paint_simple_dst_start , units='mins')   ) )	
          
        } else if ( plot_type == 2 ) { # only when 2-roots
          
          third_paint_start <- Sys.time ()				
          		  
          # ds_t_to_check <- ds_t[ ! ds_t$id %in% pkg.globals$ID_inicial , ]$id		  
		  shared_common_ancestors( ds_t$id  )  ## Identifies those that are ancestors of both roots		

          ds_tree$in_path_to_both <<- pkg.globals$dst_path_to_both
          
          third_paint_end <- Sys.time ()
if ( print_time == TRUE) print ( paste ( "Time - Third Paint:" ,  difftime( third_paint_end , third_paint_start , units='mins')  ) )	
       
          
          common_paint_start <- Sys.time ()		
          common_paint_dst( unique ( ds_tree[ds_tree$in_path_to_both == TRUE,]$id )  )
          
          common_paint_end <- Sys.time ()
if ( print_time == TRUE) print ( paste ( "Time - Common Paint:" , difftime( common_paint_end , common_paint_start , units='mins')   ) )	
           
          ds_tree$common_highlight <<- pkg.globals$dst_common_highlight
        }
        
        ## when using the dst versions we have to update the $to_color variable
        ds_tree$to_color <<- pkg.globals$dst_to_color
        
      }    
      
      coordinates_start <- Sys.time ()	
      ## necessary to calculate the y coordinate to place the names of selected individuals
      max_y2 <- max(na.omit(ds_tree$y2)) 
      
      ## Diferenciate lines that go backwards	
      # if( nrow( ds_tree[ds_tree$x2 <= ds_tree$x-0.1,]) > 0 ){
        # ds_tree[ds_tree$x2 <= ds_tree$x-0.1,]$to_color <<- 3
        # pkg.globals$dst_to_color[ ds_tree$x2 <= ds_tree$x-0.1 ]$to_color <- 3
      # }
      
      ## NOW preparing the plot	
      
      max_ger <<- isolate ( input$generations )
      max_y_coord <<- isolate ( 2^(input$generations+1)-1	 )
      
      if (plot_type == 2) {
        max_ger <<- isolate ( input$generations+1 )
        max_y_coord <<- isolate ( 2^(input$generations+2)-1 )
      }	     
      
      ######################################################
      ### Computing the inbreeding coefficient
      
      ger_inb <- 0     ## the number of generations to pass to the inbreeding function has to be adjusted	
      if (plot_type == 1) {
        ger_inb <- num_generations		
      } else if ( plot_type == 2 ) {
        ger_inb <- num_generations-1
      }
      
      if (plot_type == 2){
        ## for plots with two root individuals we need to insert an artificial child
		## I_0 is inserted to compute the inbreeding coefficient		
        artificial_child <- c(id="I_0", father= pkg.globals$ID_inicial[1], mother= pkg.globals$ID_inicial[2],geracao = 0,  sex = "M", stringsAsFactors=FALSE)
        
        ds_fulltree <-  rbind(  artificial_child , ds_fulltree )
        ds_fulltree$geracao <- as.numeric(ds_fulltree$geracao)
        ds_fulltree$dob  <- as.numeric(ds_fulltree$dob)	
      }
      
      
      imbreeding_start <- Sys.time ()
      
      ## Calls the method that computes the inbreeding coefficient
      ds_inbreeding <- NULL		  
      ds_inbreeding <- family_tree_ic(ds_fulltree)	
      
      if( is.data.frame(ds_inbreeding)  ){			
        ds_tree_plot <- ds_tree[ds_tree$id != "I0000"  & ds_tree$name !="",]
        allvalues <- c(unique( ds_tree_plot$id ), "I_0") 
        ds_tree_plot$id <- factor(ds_tree_plot$id , levels = allvalues)
        ds_inbreeding$id <- factor(ds_inbreeding$id , levels = allvalues) 
        
        ds_inbreeding$ic <- round ( ds_inbreeding$ic  , digits = 6)
        ds_tree <<- left_join( ds_tree , ds_inbreeding, by = "id" )	
      }
      imbreeding_end <- Sys.time ()
if ( print_time == TRUE)  print ( paste ( "Time - inbreeding:" , difftime( imbreeding_end , imbreeding_start , units='mins') ) )    
      
      if ( isolate ( input$kinship == "yes" ) ) {
        ###################################################################################################
        ####### kinship
        kinship_start <- Sys.time ()	
        num_ic <- nrow ( ds_tree[ !is.na(ds_tree$ic) & ds_tree$ic > 0 ,] )

		
        if ( num_ic > 0) {  ## There is at least one common ancestor
          ds_tree_local <- ds_tree[ !is.na(ds_tree$ic) & ds_tree$ic > 0 ,]
          
          for ( i in 1:num_ic ) {
            
            kinship <- parents_kinship ( ds_tree_local[i,]$id )	
            
            ds_tree[ds_tree$id == ds_tree_local[i,]$mother, ]$kinship <<- kinship[1]
            ds_tree[ds_tree$id == ds_tree_local[i,]$mother, ]$kinship_ancestor <<- kinship[2]				
            ds_tree[ds_tree$id == ds_tree_local[i,]$mother, ]$kinship_relative <<- unique ( ds_tree[ds_tree$id == ds_tree_local[i,]$id, ]$father ) 				
            ds_tree[ds_tree$id == ds_tree_local[i,]$mother, ]$kinship_child <<- ds_tree_local[i,]$id
            ds_tree[ds_tree$id == ds_tree_local[i,]$father, ]$kinship_child <<- ds_tree_local[i,]$id					
            ds_tree[ds_tree$id == ds_tree_local[i,]$father, ]$kinship_shape <<- 1
            ds_tree[ds_tree$id == ds_tree_local[i,]$mother, ]$kinship_shape <<- 1	
            ds_tree[ds_tree$id == ds_tree_local[i,]$mother, ]$kinship_y1 <<- ( ds_tree[ds_tree$id == ds_tree_local[i,]$father, ]$y2	)[1]
            
            ds_tree[ds_tree$id == ds_tree_local[i,]$mother, ]$kinship_y <<- ds_tree[ds_tree$id == ds_tree_local[i,]$mother, ]$y[1] - ( ds_tree[ds_tree$id == 
                                                                                                                                                 ds_tree_local[i,]$mother, ]$y[1] - ds_tree[ds_tree$id == ds_tree_local[i,]$father, ]$y[1] )/2
            
            ds_tree[ds_tree$id == ds_tree_local[i,]$mother, ]$kinship_x <<- ds_tree[ds_tree$id == ds_tree_local[i,]$mother, ]$x2[1] - ( ds_tree[ds_tree$id == 
                                                                                                                                                  ds_tree_local[i,]$mother, ]$x2[1] - ds_tree[ds_tree$id == ds_tree_local[i,]$father, ]$x2[1] )/2
            
          } ### end ... for ( i in 1:num_ic ) {		
        }
      
	  if ( isolate(input$plotType  == 2 ) ) {

          couple_ks <- couple_kinship( input$root_1 , input$root_2, ds_tree[2,]$sex )			

          if ( couple_ks[1] != ""  ) {	
            ds_tree[ 2 , 'kinship' ]  <<- couple_ks[1]
            ds_tree[ 2 , ]$kinship_ancestor <<- couple_ks[2]
            ds_tree[ 2 , ]$kinship_relative <<- input$root_1
            
            ds_tree[1, ]$kinship_shape <<- 1
            ds_tree[2, ]$kinship_shape <<- 1	
            ds_tree[2, ]$kinship_y1 <<-  ( ds_tree[ds_tree$id == input$root_1, ]$y )[1]			
            ds_tree[2, ]$kinship_y <<- ds_tree[ds_tree$id == input$root_2, ]$y[1] - ( ds_tree[ds_tree$id == input$root_2, ]$y[1] - ds_tree[ds_tree$id == input$root_1, ]$y[1] )/2		
            ds_tree[2, ]$kinship_x <<- ds_tree[ds_tree$id == input$root_2, ]$x2[1] - ( ds_tree[ds_tree$id == input$root_2, ]$x2[1] - ds_tree[ds_tree$id == input$root_1, ]$x2[1] )/2
            
          }				
        }
        kinship_end <- Sys.time ()
if ( print_time == TRUE) print ( paste ( "Time - kinship:" , difftime( kinship_end , kinship_start , units='mins')  ) )        
      }
      ### ... Inbreeding and kinship done!
      
      #########################	
      ## creating the plot	
      
      plot_start <- Sys.time ()
      ds_tree <- ds_tree[ds_tree$id != "I0000" & ds_tree$geracao >= 1 & ds_tree$name !="",]
	  
	  ## max width of the plot
	  max_y <- max( ds_tree$y)
	  
	  ## This var helps to set the height limits for the plot when there are horizontal arcs in the last generation
	  ## specially long ones  
	  max_x_limit <- max_ger - 0.3    
	  label_line_w <- line_thin * 1.5	
	  
	 plain_plot <- NULL 
     p <- ggplot( data = ds_tree[ ds_tree$x != ds_tree$x2_raw & ds_tree$geracao > 1,] , aes(x = x, y = y) ) 	

		
		if (input$plain_tree == TRUE ) { ## plotting the plain tree	
		
			kinship_main_highlight <<- FALSE
			output$plot_double_click_info <- renderPrint({ cat( text_2_print) })	 
			descendants_highlight <<- FALSE 
		
			p <- p + geom_segment( data = ds_plain , aes (x = x, y = y, xend = x2, yend = y2, color = sex ) , size = line_thin )     
			p <- p + geom_point( data = ds_plain[ds_plain$count >1,] , aes (x = x2, y = y2, color = sex ) , size =  ca_circle_size * 0.8)  

			if ( input$pt_conn == TRUE ) {
				p <- p + geom_path( data = ds_plain[ds_plain$count >1,] , aes (x = x2, y = y2, color = sex , group = id) ,  linetype = "dashed", size =  line_thin  )  
			}
			
			isolate ({
				## Names in the first few generations in the plain tree
				if( "yes" %in% (input$all_names) ){
				  
					num_ger_all_names <- input$all_names_ger + 1
					if ( plot_type == 2 ) num_ger_all_names <- num_ger_all_names + 1
					
					## If the names for the common ancestors is to be plot we should not include the labels for regular individuals
					if( "names" %in% (input$print_names) ){		## if true omit common ancestors		  
						p <- p + geom_text_repel( data = ds_plain[ds_plain$geracao > 1 & ds_plain$count == 1 & ds_plain$geracao <= num_ger_all_names  & ds_tree$id !="I0000",  ], aes(x=x2 , y=y2 , label = name, color = sex ), segment.size = line_thin, segment.linetype = 3, size = input$all_names_font, nudge_x = max_x_coord * (input$all_name_y) / 100 , nudge_y = max_y_coord * (input$all_name_x) / 100 , hjust = input$all_name_alg )	
					} else { ## print all names in the first generations
						p <- p + geom_text_repel( data = ds_plain[ds_plain$geracao > 1 & ds_plain$geracao <= num_ger_all_names  & ds_tree$id !="I0000",  ], aes(x=x2 , y=y2 , label = name, color = sex ), segment.size = line_thin, segment.linetype = 3, size = input$all_names_font, nudge_x = max_x_coord * (input$all_name_y) / 100 , nudge_y = max_y_coord * (input$all_name_x) / 100 , hjust = input$all_name_alg )										
					}
				}	

				if( "names" %in% (input$print_names) ){

				  if( "M" %in% (input$names_FM) ){		
					p <- p + geom_label_repel(data = ds_plain[ds_plain$geracao > 1 & ds_plain$count > 1 & ds_plain$sex == "M" & ds_tree$id !="I0000" , ], aes(x=x2, y=y2 , label = name ) , segment.size = label_line_w, segment.linetype = 3, size = input$names_font , color = male_color, nudge_x = max_x_coord * (input$name_y_M) / 100 , nudge_y = max_y_coord * (input$name_x_M) / 100 , hjust = input$name_alg_M , fill = fill_male , alpha = labels_alpha)
				  }
				  if( "F" %in% (input$names_FM) ){
					p <- p + geom_label_repel(data =  ds_plain[ds_plain$geracao > 1 & ds_plain$count > 1 & ds_plain$sex == "F" & ds_plain$id !="I0000" , ] , aes(x=x2, y=y2 , label = name ) , segment.size = label_line_w, segment.linetype = 3, size = input$names_font, color = female_color , nudge_x = max_x_coord * (input$name_y_F) / 100 , nudge_y = max_y_coord * (input$name_x_F) / 100 , hjust = input$name_alg_F, fill = fill_female , alpha = labels_alpha)
				  }								

				} ## end if ( "names..."
			})  ## end isolate

			plain_plot <- p


		
		} else { ## if (input$plain_tree == TRUE ) {

 
			  ##### COMMON ANCESTORS
			  ##BOX
			  ## If it is to highlight common ancestors of the two roots it puts a larger square box
			  if (  plot_type == 2 & input$high_both == TRUE ) {
				p <- p + geom_point(data =  unique( ds_tree[ds_tree$geracao > 1 & ds_tree$count > 1 &  ds_tree$id !="I0000" & !duplicated(ds_tree$id) & ds_tree$in_path_to_both == TRUE, c("id","x2","y2", "sex", "to_color") ]), aes(x = x2, y = y2 , alpha = factor(to_color)), stroke = line_thick  ,fill = "grey30",  color = "grey30", shape=22, size =  ca_circle_size * 2.5 ) 	
			  }
			  
			  ##POINT
			  ## A grey box beneath a common ancestor to enhance their identification
			  p <- p + geom_point(data =  unique( ds_tree[ds_tree$geracao > 1 & ds_tree$count > 1 &  ds_tree$id !="I0000" & !duplicated(ds_tree$id), c("id","x2","y2", "sex", "to_color") ]), aes(x = x2, y = y2 , alpha = factor(to_color)), stroke = line_thick  ,fill = "grey92",  color = "grey60", shape=22, size =  ca_circle_size * 2 ) 


			  
			  ##ARCS - with geom_bezier for a more natural look
			  ## In order to use the geom_bezier we need to transform the data
			  ## At the moment I believe it is better to do it in a local variable.
			  
			  dst_bezier <- ds_tree[ ds_tree$x != ds_tree$x2_raw & ds_tree$geracao > 1,] %>%
				dplyr::rename ( x1 = x , y1 = y ) %>% 
				mutate ( xm = x1 + ( x2-x1) * 0.5 , ym = y1 + (y2-y1)*0.75 ) %>% 
				mutate ( s1 = 3 , sm = 4 , s2 = 5 ) %>%  ## size variables for the thicness
				select ( id, sex, ordem, to_color, x1, xm, x2 , y1, ym, y2, s1, sm, s2 , common_highlight) %>% 
				unite( x1_uni, x1, y1, s1, sep = "_" )  %>% 
				unite( xm_uni, xm , ym , sm,  sep = "_" )  %>% 
				unite( x2_uni, x2, y2, s2, sep = "_" ) %>%
				gather("var_x", "values_x" , starts_with("x"))  %>% 
				arrange( id ) %>%
				separate( values_x , c("x","y","s"), sep="_") %>%
				mutate( x = as.numeric(x) , y = as.numeric(y) , s = as.integer(s) ) 
			  

# write_csv( ds_tree[ ds_tree$x != ds_tree$x2_raw & ds_tree$geracao > 1,] , file = "ds_tree_for_arcs.csv")
# write_csv( dst_bezier , file = "ds_tree_for_bezier.csv")
	
			  if ( isolate ( input$use_arrows )  == "yes" )  {
				
				## all arcs except the horizontal ones, which are dealt with below.
				p <- p+geom_bezier2(data = dst_bezier   , aes(x = x, y = y, group = ordem  , color = sex , alpha = factor ( to_color) , size = factor(to_color) ) ,  arrow = arrow(angle = isolate(input$arrow_angle) ,length =  unit( isolate(input$arrow_size),"cm"), type="closed" ) )
					  
				  ## COMMON ARCS
				  ## in a 2-roots plot we can highlight the ancestors common to both roots (enhance common feature)
				  if (  plot_type == 2 &  input$high_both == TRUE ) {
					
					p <- p+geom_bezier2(data = dst_bezier[dst_bezier$common_highlight == TRUE,]  , aes(x = x, y = y, group = ordem  , color = sex  ), size = line_thick * 2.5 ,  arrow = arrow(angle = isolate(input$arrow_angle) ,length =  unit( isolate(input$arrow_size),"cm"), type="closed" ) )
					
					## black background
					p <- p+geom_bezier2(data = dst_bezier[dst_bezier$common_highlight == TRUE,]  , aes(x = x, y = y, group = ordem  ), color = "black" , size = line_thick * 0.35 )	
				  }  
			  
			  } else {  ## For the new method
				p <- p+geom_bezier2(data = dst_bezier , aes(x = x, y = y, group = ordem  , color = sex , alpha = factor ( to_color) , size = factor(to_color * s) ) )
				
				  ## COMMON ARCS
				  ## in a 2-roots plot we can highlight the ancestors common to both roots (enhance common feature)
				  if (  plot_type == 2 &  input$high_both == TRUE ) {
					
					p <- p+geom_bezier2(data = dst_bezier[dst_bezier$common_highlight == TRUE,]  , aes(x = x, y = y, group = ordem  , color = sex, size = factor ( s * 10 ) ) )
					
					## black background
					p <- p+geom_bezier2(data = dst_bezier[dst_bezier$common_highlight == TRUE,]  , aes(x = x, y = y, group = ordem , size = factor ( s * 5 ) ), color = "black" )	
				  }				
				
			  }			  
			  
			  ## HORIZONTAL ARCS
			  #### Horizontal lines need to be dealt with in separate
			  
			  ## dataframe with horizontal lines
			  ds_tree_curves <- ds_tree[ ds_tree$x == ds_tree$x2_raw & ds_tree$geracao > 1 , ]
  			  
			  if ( nrow( ds_tree_curves ) > 0 ) {  ## check if there are horizontal lines
				
				## the same as before but now for the horizontal lines		
				dst_bezier_hor <- ds_tree_curves %>% mutate ( x1 = x , y1 = y ) %>% 
				  mutate ( xm = x1 +  1.5 -  ( max_y - abs(y2-y1) ) / max_y , ym = ifelse( sex=="M" , y1 + (y2-y1)*0.75 , y1 + (y2-y1)*0.35 ) ) %>% 
				  mutate ( s1 = 3 , sm = 4 , s2 = 5 ) %>%  ## size variables for the thicness
				  select ( id, sex, to_color, x, y, x1, xm, x2 , y1, ym, y2, s1, sm, s2, common_highlight) %>% unite (to_group , id, x, y, sep="-" ) %>%
				  unite( x1_uni , x1, y1, s1, sep = "_" )  %>% 
				  unite( xm_uni ,xm , ym, sm , sep = "_" )  %>% 
				  unite( x2_uni ,x2, y2, s2, sep = "_" ) %>%
				  gather("var_x", "values_x" , starts_with("x"))  %>% 
				  separate( values_x , c("x","y","s"), sep="_") %>%
				  mutate( x = as.numeric(x) , y = as.numeric(y) , s = as.integer(s) )

			
				## checks the max coordinate of the horizontal arcs			
				max_x_curve <- max( dst_bezier_hor[ dst_bezier_hor$var_x == "xm_uni",]$x )
				if ( max_x_curve > max_x_limit ) max_x_limit <- max_x_curve ## and if it is higher the the max, sets the new max

				if ( input$use_arrows == "yes" ) { 
					p <- p  +  geom_bezier2(data = dst_bezier_hor , aes(x = x, y = y, group = to_group ,  color = sex, alpha = factor ( to_color) , size = factor(to_color)) ,  arrow = arrow(angle = isolate(input$arrow_angle) ,length =  unit( isolate(input$arrow_size),"cm"), type="closed" ) )  
					
					if ( plot_type == 2 & input$high_both == TRUE ) {
					  p <- p  +  geom_bezier2(data = dst_bezier_hor[dst_bezier_hor$common_highlight == TRUE,] , aes(x = x, y = y, group = to_group ,  color = sex, alpha = factor ( to_color) ) ,  size = line_thick * 2.5, arrow = arrow(angle = isolate(input$arrow_angle) ,length =  unit( isolate(input$arrow_size),"cm"), type="closed" ) )
						
					## black background					  
					  p <- p  +  geom_bezier2(data = dst_bezier_hor[dst_bezier_hor$common_highlight == TRUE,] , aes(x = x, y = y, group = to_group , alpha = factor ( to_color) ) , color = "black" ,  size = line_thick * 0.35  )
					  
					}
				} else {  ## For the new method
					p <- p+geom_bezier2(data = dst_bezier_hor , aes(x = x, y = y, group = to_group ,  color = sex, alpha = factor ( to_color) , size = factor(to_color * s))  )  
				
				}
			  }
      } ## end if (input$plain_tree == TRUE ) {
	  
      ## configures the X axis breaks
      if(plot_type == 1) {
        p <- p +  scale_x_continuous( limits = c(-0.3, max_x_limit ), breaks= seq(from=0+first_gen_x, to=max_ger-1+first_gen_x)  , labels = c("1", seq(from=2, to=max_ger)),expand = c(0,0) )
        
      } else if (plot_type == 2) {
        
        p <- p +  scale_x_continuous( limits = c(-0.3, max_x_limit), breaks= seq(from=0, to=max_ger-1 ) , labels = seq(from=0, to=max_ger-1) , expand = c(0,0))
      }
	  
      max_x_coord <<- max_ger  + 1	
      
      p <- p + labs(x ="generation")		
      
      ## Takes care of the title and subtitle
      if ("title" %in% isolate(input$print_title) ){
        p <- p + labs( title = title_plot )
      }
      
      if ( "subtitle" %in% isolate( input$print_title) ){		
        p <- p + labs( subtitle = sub_title )
      }
     

 
	  ## ROOTS NAMES
	  isolate({
		## print names of Roots				
		  
		  ## Plots the name(s) of the root individual(s)
		  p <- p + geom_text(data = ds_tree[ds_tree$geracao == 1 & ds_tree$sex=="M", ], aes( x= x2 - 0.15  , y= y2 , label=name ), hjust = "center",  size = input$names_font, color = male_color  )
		  p <- p + geom_text(data = ds_tree[ds_tree$geracao == 1 & ds_tree$sex=="F", ], aes( x= x2 - 0.15  , y= y2 , label=name ), hjust = "center",  size = input$names_font , color = female_color )
	  })
			  
		if (input$plain_tree == FALSE ) {	      
			  ## INDIVIDUALS
			  p <- p + geom_point(data = ds_tree[ds_tree$geracao == 1, ], aes( x= x2, y= y2 , colour = factor(sex) , alpha = factor(to_color) ) , shape = 16, size = circle_size )
			  
			  ## individuals on a path to common ancestors
			  p <- p + geom_point(data = unique( ds_tree[ds_tree$to_color == 2, ]), aes(x = x2, y = y2,colour = factor(sex) , alpha = factor(to_color) ),  shape=16, size = circle_size )
			  
			  ## common ancestors have a larger circle
			  p <- p + geom_point(data =  unique( ds_tree[ds_tree$geracao > 1 & ds_tree$count > 1 &  ds_tree$id !="I0000" & !duplicated(ds_tree$id), c("id","x2","y2", "sex", "to_color") ]), aes(x = x2, y = y2,fill = factor(sex) , alpha = factor(to_color)), stroke = line_thick  ,  shape=21, size =  ca_circle_size )
		} ## end ... if (input$plain_tree == TRUE ) {
		
		## Sets the color and alpha values
		p <- p + scale_colour_manual(values = c("F"=female_color, "M"=male_color))
		p <- p + scale_fill_manual(values = c("F"=female_color, "M"=male_color))

		if ( input$use_arrows == "yes" ) { 
			p <- p + scale_size_manual(values = c("1"=line_thin, "2"=line_thick ))
		} else {
			p <- p + scale_size_manual(values = c("3"=line_thin*te1,"4"=line_thin*tem,"5"=line_thin*te2, "6"=line_thick*te1, "8"=line_thick*tem, "10"=line_thick*te2 ,"30"=line_thick*2.5*te1,"40"=line_thick*2.5*tem,"50"=line_thick*2.5*te1, "15"=line_thin*0.5*te1, "20"=line_thin*0.5*tem, "25"=line_thin*0.5*te2 ))
		
		}
		p <- p + scale_alpha_manual(values = c("1"=alpha_thin, "2"=alpha_thick ))
		p <- p + guides(colour="none", size = "none", linetype= "none", alpha="none" , fill = "none" ) 


		## Theme settings

		font_size <- 5
		font_color <- "royalblue4"
		fontfamily <- 'sans'

		p <- p + theme_minimal( base_family = fontfamily )  ## an empty theme with a specified font
		update_geom_defaults("text", list( family = fontfamily ))  ## geoms are not part of the theme so we have to specify their font as well
		update_geom_defaults("label", list( family = fontfamily ))  ## geoms are not part of the theme so we have to specify their font as well

		p <- p + theme(panel.grid.minor = element_blank()) 

		p <- p + theme(axis.title.x = element_blank())
		p <- p + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
		p <- p + theme(axis.text.y = element_text(size=isolate(input$font_axis_text) ),axis.title.y = element_text(size=isolate(input$font_axis_title) ))	
		p <- p + theme(panel.grid.major.x = element_blank()) 


		p <- p + theme ( text =  element_text( face = "plain", size = rel(font_size) , color = font_color  ) )

		p <- p + theme ( plot.title = element_text( size = title_font * 1.5, hjust = 0, face = "bold" ))
		p <- p + theme ( plot.subtitle = element_text( size = subtitle_font * 1.5 , hjust = 0, face="italic" ))
		p <- p + theme ( plot.margin = margin(0.5 , 0.5 , 0.5 , 0.5 , "cm") )
		p <- p + theme ( plot.background = element_rect( fill = "grey94", color = "white" , size = 0.5  ) )
		p <- p + theme ( panel.background = element_rect(fill = "white",  color = "grey55" , size = 0.5 ) )

		the_plot_base <<- p 
		
		the_plot_base <<- the_plot_base + coord_flip() + scale_y_reverse()
			
      
		if (input$plain_tree == FALSE ) {
			  ### Names of all individuals in the first few generations
			  isolate ({
				if( "yes" %in% (input$all_names) ){
				  
				  num_ger_all_names <- input$all_names_ger + 1
				  if ( plot_type == 2 ) num_ger_all_names <- num_ger_all_names + 1
				  
					
					p <- p + geom_text_repel( data = unique( ds_tree[ds_tree$geracao > 1 & ds_tree$geracao <= num_ger_all_names  & ds_tree$to_color == 1  & ds_tree$id !="I0000" & !duplicated(ds_tree$id), c("id","x2","y2","sex","name") ]), aes(x=x2 , y=y2 , label = name, color = sex ), segment.size = line_thin, segment.linetype = 3, size = input$all_names_font, nudge_x = max_x_coord * (input$all_name_y) / 100 , nudge_y = max_y_coord * (input$all_name_x) / 100 , hjust = input$all_name_alg )		
					
				}	
			  })
			  
			  ##############
			  ## Names and ids of the common ancestors
			  

			  isolate ({
				
				if( "ids" %in% (input$print_names) ){	
				  
					if ( input$names_inc == "path" ) { ## all names in path to common ancestors
					  ## repel to try to avoid overlaps
					  if( "M" %in% (input$names_FM) ){
						p <- p + geom_label_repel(data = unique( ds_tree[ds_tree$geracao > 1 & ds_tree$to_color == 2 & ds_tree$sex == "M" & ds_tree$id !="I0000" & !duplicated(ds_tree$id), c("id","x2","y2") ]), aes(x = x2, y = y2, label = paste(id) )  , segment.size = label_line_w, segment.linetype = 3, size = input$ids_font , color = male_color, nudge_x = max_x_coord * (input$id_y) / 100 , nudge_y = max_y_coord * (input$id_x) / 100 , hjust = input$id_alg_M, alpha = labels_alpha )
					  }
					  if( "F" %in% (input$names_FM) ){
						p <- p + geom_label_repel(data = unique( ds_tree[ds_tree$geracao > 1 & ds_tree$to_color == 2 & ds_tree$sex == "F" & ds_tree$id !="I0000" & !duplicated(ds_tree$id), c("id","x2","y2") ]), aes(x = x2, y = y2, label = paste(id) ) , segment.size = label_line_w, segment.linetype = 3,  size = input$ids_font , color = female_color , nudge_x = max_x_coord * (input$id_y) / 100 , nudge_y = max_y_coord * (input$id_x) / 100 , hjust = input$id_alg_F , alpha = labels_alpha)
					  }
					} else { ## only the common ancestors
					  ## repel to try to avoid overlaps
					  if( "M" %in% (input$names_FM) ){
						p <- p + geom_label_repel(data = unique( ds_tree[ds_tree$geracao > 1 & ds_tree$count > 1 & ds_tree$sex == "M" & ds_tree$id !="I0000" & !duplicated(ds_tree$id), c("id","x2","y2") ]), aes(x = x2, y = y2, label = paste(id) ) , segment.size = label_line_w, segment.linetype = 3, size = input$ids_font , color = male_color, nudge_x = max_x_coord * (input$id_y) / 100 , nudge_y = max_y_coord * (input$id_x) / 100 , hjust = input$id_alg_M , alpha = labels_alpha)
					  }
					  if( "F" %in% (input$names_FM) ){
						p <- p + geom_label_repel(data = unique( ds_tree[ds_tree$geracao > 1 & ds_tree$count > 1 & ds_tree$sex == "F" & ds_tree$id !="I0000" & !duplicated(ds_tree$id), c("id","x2","y2") ]), aes(x = x2, y = y2, label = paste(id) ) , segment.size = label_line_w, segment.linetype = 3,  size = input$ids_font , color = female_color , nudge_x = max_x_coord * (input$id_y) / 100 , nudge_y = max_y_coord * (input$id_x) / 100 , hjust = input$id_alg_F , alpha = labels_alpha)
					  }				
					}
				  
				  
				} ## end if ( "ids"...
			  }) ## end isolate
			  
			  
			  isolate ({
				if( "names" %in% (input$print_names) ){
				  
					if ( input$names_inc == "path" ) { ## all names in path to common ancestors
					  
					  if( "M" %in% (input$names_FM) ){		
						p <- p + geom_label_repel(data = unique( ds_tree[ds_tree$geracao > 1 & ds_tree$to_color == 2 & ds_tree$sex == "M" & ds_tree$id !="I0000" & !duplicated(ds_tree$id), c("id","x2","y2","name") ]), aes(x=x2, y=y2 , label = name ) , segment.size = label_line_w, segment.linetype = 3, size = input$names_font , color = male_color, nudge_x = max_x_coord * (input$name_y_M) / 100 , nudge_y = max_y_coord * (input$name_x_M) / 100 , hjust = input$name_alg_M , fill = fill_male , alpha = labels_alpha)
					  }
					  if( "F" %in% (input$names_FM) ){
						p <- p + geom_label_repel(data = unique( ds_tree[ds_tree$geracao > 1 & ds_tree$to_color == 2 & ds_tree$sex == "F" & ds_tree$id !="I0000" & !duplicated(ds_tree$id), c("id","x2","y2","name") ]), aes(x=x2, y=y2 , label = name ) , segment.size = label_line_w, segment.linetype = 3, size = input$names_font, color = female_color , nudge_x = max_x_coord * (input$name_y_F) / 100 , nudge_y = max_y_coord * (input$name_x_F) / 100 , hjust = input$name_alg_F, fill = fill_female , alpha = labels_alpha)
					  }
					} else { 
					  if( "M" %in% (input$names_FM) ){		
						p <- p + geom_label_repel(data = unique( ds_tree[ds_tree$geracao > 1 & ds_tree$count > 1 & ds_tree$sex == "M" & ds_tree$id !="I0000" & !duplicated(ds_tree$id), c("id","x2","y2","name") ]), aes(x=x2, y=y2 , label = name ) , segment.size = label_line_w, segment.linetype = 3, size = input$names_font , color = male_color, nudge_x = max_x_coord * (input$name_y_M) / 100 , nudge_y = max_y_coord * (input$name_x_M) / 100 , hjust = input$name_alg_M , fill = fill_male , alpha = labels_alpha)
					  }
					  if( "F" %in% (input$names_FM) ){
						p <- p + geom_label_repel(data = unique( ds_tree[ds_tree$geracao > 1 & ds_tree$count > 1 & ds_tree$sex == "F" & ds_tree$id !="I0000" & !duplicated(ds_tree$id), c("id","x2","y2","name") ]), aes(x=x2, y=y2 , label = name ) , segment.size = label_line_w, segment.linetype = 3, size = input$names_font, color = female_color , nudge_x = max_x_coord * (input$name_y_F) / 100 , nudge_y = max_y_coord * (input$name_x_F) / 100 , hjust = input$name_alg_F, fill = fill_female , alpha = labels_alpha)
					  }								
					}
				  
				} ## end if ( "names..."
			  })  ## end isolate
			  
			  
			  
			  ######################################################
			  ## Plot kinship
			  isolate({
				if (  input$kinship == "yes" ){
				  
				  ### KINSHIP DOTS
				  p <- p + geom_point( data = ds_tree[ds_tree$kinship_shape == 1 ,] , aes( x= x2, y= y2, color = factor(sex) ), shape = 21 , stroke = line_thick , size = ks_circle_size  , fill = fill_kinship )

					
					p <- p + geom_label(data = ds_tree[(  ( ds_tree$x != ds_tree$x2_raw & ds_tree$geracao > 2 ) | ds_tree$geracao == 1 ) & ds_tree$kinship != "undefined" & ds_tree$kinship != "",] , aes(  x= as.double(kinship_x), y= as.double(kinship_y), label = kinship ), size = input$kinships_font ,  nudge_x = max_x_coord * (input$kinship_y) / 100 , nudge_y = max_y_coord * (input$kinship_x) / 100 , hjust = input$kinship_alg , fill = fill_kinship , alpha = labels_alpha )
					
					p <- p + geom_segment(data = ds_tree[ ( ds_tree$x != ds_tree$x2_raw & ds_tree$geracao == 2 ) & ds_tree$kinship != "undefined" & ds_tree$kinship != "",] , aes( x= as.double(kinship_x) ,xend= as.double(kinship_x) ,y=as.double( kinship_y1) , yend = y2) , color = fill_kinship )
					
					p <- p + geom_label(data = ds_tree[ ( ds_tree$x != ds_tree$x2_raw & ds_tree$geracao == 2 ) & ds_tree$kinship != "undefined" & ds_tree$kinship != "",] , aes(  x= as.double(kinship_x), y= as.double(kinship_y), label = kinship ), size = input$kinships_font ,  nudge_y = max_y_coord * (input$kinship_x) / 100 , fill = fill_kinship , alpha = labels_alpha )
					
			
				}
			  }) ## end isolate()
			  
			  
			  isolate ({	
				#################################
				##Plot IC
				if ( "inc" %in% (input$IC) ) {
				  ## inbreeding coefficient 
				  if(  is.data.frame(ds_inbreeding) ){
					
					ds_local <- ds_tree[ds_tree$geracao>=0 & ds_tree$ic > 0, ]
					ds_local <- ds_local[!duplicated(ds_local$id),]
					
					if ( "std" %in% (input$IC) ) {
					  
					  ds_local$ic <- ds_local$ic / ( input$max_IC ) 
					} else {
					  ds_local$ic <-  ds_local$ic 
					  
					} 
					
					p <- p + geom_point( data = ds_local , aes( x= x2, y= y2 ), color = "black" , shape = 21 , stroke = line_thick*0.5 , size = inb_circle_size , fill = fill_inb )	
					

				  ##  Coefficient of relationship in case it is a type=2 plot
					p <- p + geom_label(data = ds_local , aes( x = x2, y = y2, label = format( round( ic , digits = 3 ), nsmall = 3 ) ), size = ( input$ic_font) , nudge_x = max_x_coord * (input$ic_y) / 100 , nudge_y = max_y_coord * (input$ic_x) / 100 , hjust = input$ic_alg  , fill = fill_inb  , alpha = labels_alpha*0.5)
			
				  }
				}	
				
				#############################
				## Plot RC
				
				if ( "inc" %in% (input$RC) & "inc" %in% (input$IC) ) {
				  if (plot_type == 2){	
					
					if ( nrow(ds_inbreeding[ ds_inbreeding$id == "I_0" & ds_inbreeding$ic >0,  ]) > 0  ){
					  
					  ds_local <- ds_inbreeding[ !is.na(ds_inbreeding$id ) & ds_inbreeding$id == "I_0",  ]		
					  y_local = ds_tree[2,]$y + (ds_tree[1,]$y-ds_tree[2,]$y)/2
					  
					  if ( "std" %in% (input$RC) ) {	
						ds_local$ic <- ds_local$ic / ( input$max_RC ) 
					  } else {
						ds_local$ic <-  ds_local$ic 				
					  } 
					  
					  ##  Coefficient of relationship in case it is a type=2 plot
					  p <- p + geom_label( data = ds_local , aes( x = 0 , y = y_local, label = paste("RC:", format( round( ic * 2 , digits = 3 ), nsmall = 3 ) ) ),  size = ( input$ic_font)  , hjust = "center" , fill = fill_rel , alpha = labels_alpha*0.5)						
					}
				  }
				}		
				
			  }) ## end isolate
		} ## end ... if (input$plain_tree == TRUE ) {
			        

        p <- p + coord_flip()   + scale_y_reverse()

      
      the_plot <<- p 
      the_plot_2_print <<- p 	
      # write.xlsx2(ds_tree, "ds_tree_XYZ.xlsx")
      plot_end <- Sys.time ()
if ( print_time == TRUE)  print ( paste ( "Time - plot:" , difftime( plot_end , plot_start , units='mins') ) )	
if ( print_time == TRUE)  print( paste ("GLOBAL Time - plot_end:" , plot_end - init_time ) )
      
      first_plot <<- TRUE
    }
    the_plot
    
  }  # )
  #########################################################
  
  
  
  ###########################################
  ### method that draws plot on the screen	
  output$the_plot <- renderPlot({
    render_start <- Sys.time ()
    input$update_plot
    	
    if ( first_plot == TRUE ) {
      
     # print("IN - output$the_plot <- renderPlot({")
      
      withProgress(message = "Updating the plot", detail="", value=0.9,{
        
        ## settings for highlighting
        high_marker_size <- isolate( input$circle_size ) 
        high_arrow_size  <- isolate( input$arrow_size ) * 1.5
        high_line_size   <- isolate( input$line_thick ) * 1.5
        high_arrow_angle <- isolate( input$arrow_angle ) 
        
        
        ##########################################################33
        ## highlight in the main panel
        if( descendants_highlight  == TRUE  ){
          
          descendants_highlight <<- FALSE
          
		  
## We are trying to replace the new_paint() by the new_paint_dst()
## in order to make the ds_tree not shared among files
		  
          ds_tree$to_highlight <<- 0  ## resets the variable that identifies individuals to highlight
          ds_tree$highlight_count <<- 0  ## resets the variable that identifies individuals to highlight

		## the following call was replaced by the paint_descendants
         # new_paint( descendants_highlight_id , 2)  ## calls method that identifies individuals that descend from the selected one
		  
		  pkg.globals$dst_to_highlight <- rep(0, length ( ds_tree$to_highlight ) ) ## need for new_paint_dst()
		  pkg.globals$dst_highlight_count <- rep(0, length ( ds_tree$to_highlight ))

      #write_csv ( ds_tree , "ds_tree_bhc.csv")	

local_dds <- data.frame(pkg.globals$dst_id , pkg.globals$dst_father ,      pkg.globals$dst_mother ,pkg.globals$dst_sex ,pkg.globals$dst_child ,pkg.globals$dst_count ,pkg.globals$dst_to_color,pkg.globals$dst_common_highlight ,     pkg.globals$dst_path_to_both,pkg.globals$dst_to_highlight ,
	  pkg.globals$dst_highlight_count )
      write_csv ( local_dds , "local_dds.csv")	
	  
		  paint_descendants_highlight_dst( descendants_highlight_id , 2) 
		  
          ds_tree$to_highlight <<- pkg.globals$dst_to_highlight  ## resets the variable that identifies individuals to highlight
          ds_tree$highlight_count <<- pkg.globals$dst_highlight_count  ## resets the variable that identifies individuals to highlight
		  
     # write_csv ( ds_tree , "ds_tree_hc.csv")		  
		  
          ## If we have two root individuals we want a marker on them, if we have just one root individual
          ## we do not need that
          max_value <- ifelse( plot_type == 1, max(ds_tree$to_highlight) , max(ds_tree$to_highlight) + 1 )
          
			p <- the_plot_base ## original plot without ids and names

			## fading baseplot before inserting the new layer with the highlight
			p <- p + scale_alpha_manual(values = c("1"=0.25, "2"=0.25 ))
			
			## redraws their lines with a thicker line   
			max_count <- max( na.omit( ds_tree$highlight_count ) ) ## number of distinct line's thickness
			
			
			if ( input$use_arrows == "yes" ) {

			#	p <- p + scale_size_manual(values = c("1"=11, "2"=11 ))
				p <- p + scale_size_manual(values = c("1"=line_thin, "2"=line_thin ))
			}else {
				size_vect <- c("3"=line_thin*te1,"4"=line_thin*tem,"5"=line_thin*te2, "6"=line_thin*te1, "8"=line_thin*tem, "10"=line_thin*te2 ,"30"=line_thin*te1,"40"=line_thin*tem,"50"=line_thin*te2, "15"=line_thin*te1, "20"=line_thin*tem, "25"=line_thin*te2,  "60"=line_thick*te1, "80"=line_thick*tem, "100"=line_thick*te2 )

				p <- p + scale_size_manual(values = size_vect )
			}
          
          
        ### regular arcs
		ds_tree_hd_reg <- ds_tree 	%>% filter ( highlight_count > 0 ) %>%
			filter (  x != x2 & x != x2_raw ) %>%
			dplyr::rename ( x1 = x , y1 = y ) %>% 
			mutate ( xm = x1 + ( x2-x1) * 0.5 , ym = y1 + (y2-y1)*0.75 ) %>% 
			mutate ( s1 = 3 , sm = 4 , s2 = 5 ) %>%  ## size variables for the thicness
            unite( uni_1_x1 , x1, y1, s1, sep = "_" )  %>% 
            unite( uni_2_xm , xm, ym, sm, sep = "_" )  %>% 
            unite( uni_3_x2 , x2, y2, s2, sep = "_" ) %>%
			gather("var_x", "values_x" , starts_with("uni_"))  %>% 
			arrange( id , var_x ) %>%
			separate( values_x , c("x","y","s"), sep="_") %>%
			mutate( x = as.numeric(x) , y = as.numeric(y), s = as.integer(s)  )  
        
		max_y <- max( na.omit ( ds_tree$y ) )

        ### horizontal arcs				
		ds_tree_hd_hor <- ds_tree 	%>% filter ( highlight_count > 0 ) %>%
            filter ( geracao > 1 & x == x2_raw ) %>%
            dplyr::rename ( x1 = x , y1 = y ) %>%  
			mutate ( xm = x1 +  1.5 -  ( max_y - abs(y2-y1) ) / max_y  , ym = ifelse( sex=="M" , y1 + (y2-y1)*0.75 , y1 + (y2-y1)*0.35 ) ) %>% 
			mutate ( s1 = 3 , sm = 4 , s2 = 5 ) %>%  ## size variables for the thicness
            unite( uni_1_x1 , x1, y1, s1, sep = "_" )  %>% 
            unite( uni_2_xm , xm, ym, sm, sep = "_" )  %>% 
            unite( uni_3_x2 , x2, y2, s2, sep = "_" ) %>%
            gather("var_x", "values_x" , starts_with("uni_"))  %>% 
			arrange( id , var_x ) %>%
            separate( values_x , c("x","y","s"), sep="_") %>%
            mutate( x = as.numeric(x) , y = as.numeric(y), s = as.integer(s)  )  						
          
          ## Edges with varying widths

            if ( isolate ( input$use_arrows )  == "yes" )  {					

				 for ( i in 1:max_count ) {
					
					line_factor <- 1 + (i-1) * ( isolate( input$highlight_factor ) -1)	
					p <- p+geom_bezier2(data = ds_tree_hd_reg[ds_tree_hd_reg$highlight_count == i,] , aes(x = x, y = y, group = ordem  , color = sex  ), size = high_line_size * line_factor , arrow = arrow(angle = isolate(input$arrow_angle) ,length =  unit( isolate(input$arrow_size),"cm"), type="closed" )  )
				
				#horizontal arcs
					p <- p+geom_bezier2(data = ds_tree_hd_hor[ds_tree_hd_hor$highlight_count == i,] , aes(x = x, y = y, group = ordem  , color = sex ) , size = high_line_size * line_factor  , arrow = arrow(angle = isolate(input$arrow_angle) ,length =  unit( isolate(input$arrow_size),"cm"), type="closed" )  )
				}
			} else {
				p <- p+geom_bezier2(data = ds_tree_hd_reg , aes(x = x, y = y, group = ordem  , color = sex, size = factor(s*20) )  )
				
				#horizontal arcs
				p <- p+geom_bezier2(data = ds_tree_hd_hor , aes(x = x, y = y, group = ordem  , color = sex, size = factor(s*20) )   )
              
            }      				
          
          ## We now have the option to put a white line on top of the highlight lines, 
          ## we believe it eases the comparison of the thickness of the lines
          
          if ( isolate( input$white_line ) == "yes" ) {
            
            thin_line_factor <- isolate( input$white_line_factor )  ## parameter that sets the thickness of the white line
            
            if ( isolate ( input$use_arrows )  == "yes" )  {
				## regular arcs
				p <- p+geom_bezier2( data = ds_tree_hd_reg, aes(x = x, y = y, group = ordem  ), colour = "white" , size = high_line_size * thin_line_factor, arrow = arrow(angle = isolate(input$arrow_angle) ,length =  unit( isolate(input$arrow_size),"cm"), type="closed" ) )
				## horizontal arcs
				p <- p+geom_bezier2(data = ds_tree_hd_hor , aes(x = x, y = y, group = ordem  ) , colour = "white" , size = high_line_size * thin_line_factor  , arrow = arrow(angle = isolate(input$arrow_angle) ,length =  unit( isolate(input$arrow_size),"cm"), type="closed" )  )									


## In this version we are not using the white lines for the arcs with no arrows.
            } else { ## For the new method
				# p <- p+geom_bezier2( data = ds_tree_hd_reg, aes(x = x, y = y, group = ordem  ), colour = "white" , size = high_line_size * thin_line_factor )				
            }
          }
          
          ##To distinguish the marks for the individuals on the path from the highlight person
          ## to the root person we vary the marker size			
          
          max_to_highlight <- max ( na.omit ( ds_tree$to_highlight) )  ## number of diferent marks that we need
          
          if ( plot_type == 1 ) {
            p <- p + geom_point(data = ds_tree[ ds_tree$to_highlight >= 1 & !duplicated(ds_tree$id) & ds_tree$to_highlight > 2 & !ds_tree$id %in% isolate( input$root_1) ,], aes(x = x2, y = y2 ,colour = factor(sex) , fill = factor(sex) ) , size= ca_circle_size * 0.5 , shape = 21 , stroke = 1.5 )
            
          } else if ( plot_type == 2 ) {
            
            p <- p + geom_point(data = ds_tree[ ds_tree$to_highlight >= 1 & !duplicated(ds_tree$id) & ds_tree$to_highlight>2 ,], aes(x = x2, y = y2 ,colour = factor(sex) , fill = factor(sex)  ), size= ca_circle_size * 0.5 , shape = 21 , stroke = 1.5 ) 
          }			
          
          ## marker for the highlight source individual
          p <- p + geom_point(data = ds_tree[ ds_tree$id %in% isolate(descendants_highlight_id)  & ds_tree$x2 == ds_tree$x2_raw ,], aes(x = x2, y = y2 , fill = factor(sex)  ), size = ca_circle_size * 1.5, stroke = 1.3 , shape = 23 , color = "black" ) 	
          
          p <- p + geom_point(data = ds_tree[ ds_tree$geracao == 1 ,], aes(x = x2, y = y2 , fill = factor(sex)  ,colour = factor(sex) ), size = ca_circle_size * 0.75 , stroke = 1.3 , shape = 21  ) 	
          
          
          ##############
          ## Names and ids of the commong ancestors
          
          ##Print IDs
          isolate(
            if( "ids" %in% isolate( input$print_names ) ){
              					
                if( "M" %in% input$names_FM ){
                  p <- p + geom_label_repel(data = ds_tree[!duplicated(ds_tree$id) & ds_tree$geracao > 1 & ds_tree$sex=="M" & ds_tree$to_highlight>2& ds_tree$to_highlight<= max_value,], aes(x = x2, y = y2, label = paste(id) ), size = input$ids_font , color = male_color, nudge_x = max_x_coord * (input$id_y) / 100 , nudge_y = max_y_coord * (input$id_x) / 100 , hjust = input$id_alg_M, alpha = labels_alpha )
                }
                if( "F" %in% input$names_FM ){
                  p <- p + geom_label_repel(data = ds_tree[!duplicated(ds_tree$id) & ds_tree$geracao > 1 & ds_tree$sex== "F" & ds_tree$to_highlight>2& ds_tree$to_highlight<= max_value,] ,  aes(x = x2, y = y2, label = paste(id) ),  size = input$ids_font , color = female_color , nudge_x = max_x_coord * (input$id_y) / 100 , nudge_y = max_y_coord * (input$id_x) / 100 , hjust = input$id_alg_F, alpha = labels_alpha)
                }

              
              ## The highlighted person
              if ( ds_tree[ds_tree$id == descendants_highlight_id ,]$sex == "F" ) {
                p <- p + geom_label(data = ds_tree[!duplicated(ds_tree$id) & ds_tree$id == descendants_highlight_id ,] ,  aes(x = x2, y = y2, label = paste(id) ),  size = input$ids_font , color = female_color , nudge_x = max_x_coord * (input$id_y) / 100 , nudge_y = max_y_coord * (input$id_x) / 100 , hjust = input$id_alg_F, alpha = labels_alpha)
              } else {
                p <- p + geom_label(data = ds_tree[!duplicated(ds_tree$id) & ds_tree$id == descendants_highlight_id ,], aes(x = x2, y = y2, label = paste(id) ), size = input$ids_font , color = male_color, nudge_x = max_x_coord * (input$id_y) / 100 , nudge_y = max_y_coord * (input$id_x) / 100 , hjust = input$id_alg_M , alpha = labels_alpha)
              }
            }
          ) ## end isolate (						
          
          ## Print names
          isolate(
            if( "names" %in% input$print_names ){
 							
                if( "M" %in% input$names_FM ){
                  p <- p + geom_label_repel(data = ds_tree[!duplicated(ds_tree$id) & ds_tree$geracao > 1 & ds_tree$sex=="M" & ds_tree$to_highlight>2& ds_tree$to_highlight <= max_value,] , aes(x=x2, y=y2 , label = name ), size = input$names_font , color = male_color, nudge_x = max_x_coord * (input$name_y_M) / 100 , nudge_y = max_y_coord * (input$name_x_M) / 100 , hjust = input$name_alg_M , alpha = labels_alpha)
                }
                
                if( "F" %in% input$names_FM ){
                  p <- p + geom_label_repel(data = ds_tree[!duplicated(ds_tree$id) & ds_tree$geracao > 1 & ds_tree$sex== "F" & ds_tree$to_highlight>2& ds_tree$to_highlight <= max_value,] , aes(x=x2, y=y2 , label = name ), size = input$names_font, color = female_color , nudge_x = max_x_coord * (input$name_y_F) / 100 , nudge_y = max_y_coord * (input$name_x_F) / 100 , hjust = input$name_alg_F, alpha = labels_alpha)
                }
                
                ## The highlighted person						
                if ( ds_tree[ds_tree$id == descendants_highlight_id ,]$sex == "F" ) {
                  p <- p + geom_label_repel(data = ds_tree[!duplicated(ds_tree$id) & ds_tree$id == descendants_highlight_id,] , aes(x=x2, y=y2 , label = name ), size = input$names_font, color = female_color , nudge_x = max_x_coord * (input$name_y_F) / 100 , nudge_y = max_y_coord * (input$name_x_F) / 100 , hjust = input$name_alg_F, alpha = labels_alpha)
                } else {
                  p <- p + geom_label_repel(data = ds_tree[!duplicated(ds_tree$id) & ds_tree$id == descendants_highlight_id ,], aes(x=x2, y=y2 , label = name ), size = input$names_font , color = male_color, nudge_x = max_x_coord * (input$name_y_M) / 100 , nudge_y = max_y_coord * (input$name_x_M) / 100 , hjust = input$name_alg_M, alpha = labels_alpha )					
                }							
                
               
            }
          )
          isolate(
            if ( input$high_labels == "yes" ) {
              p <- p + geom_label( data = ds_tree[ ds_tree$id != descendants_highlight_id & ds_tree$highlight_count > 0 ,] , aes(x = x2+input$high_labels_y*max_x_coord/100, y = y2-input$high_labels_x*max_y_coord /100, label = paste("l:",(to_highlight-2),",n:",highlight_count, sep="")  ) , size = input$names_font, alpha = input$alpha_labels )
            }
          )
          
          
          
          p <- p + guides ( fill = "none" )
          the_plot_2_print <<- p			
          the_plot_highlight <<- p
          
          
          #################################################################################
          #####################################################################
          ### Kinship highlight
          
        } else if(  kinship_main_highlight  == TRUE  ){
          
          kinship_main_highlight <<- FALSE 
          
          lid <- kinship_main_highlight_id  		## local id of the selected individual
          cid <- unique( ds_tree[ds_tree$id == lid,]$kinship_child )		## id of the child
          
		  ## fading baseplot before inserting the new layer with the highlight        
          the_plot_local <- the_plot_base
		  # the_plot_base <- the_plot_base + scale_size_manual(values = c("1"=line_thin, "2"=line_thin ))

		  the_plot_local <- the_plot_local + scale_alpha_manual(values = c("1"=0.25, "2"=0.25 ))			

		if ( input$use_arrows == "yes" ) { 
			the_plot_local <- the_plot_local + scale_size_manual(values = c("1"=line_thin, "2"=line_thin ))
		} else {
		
		## The thicknes for arcs with varying width are tricky because we have to define it in the aes()
			the_plot_local <- the_plot_local + scale_size_manual(values = c("3"=line_thin*te1,"4"=line_thin*tem,"5"=line_thin*te2, "6"=line_thin*te1, "8"=line_thin*tem, "10"=line_thin*te2 ,"30"=line_thin*2.5*te1,"40"=line_thin*2.5*tem,"50"=line_thick*2.5*te1, "15"=line_thin*0.5*te1, "20"=line_thin*0.5*tem, "25"=line_thin*0.5*te2 , "45"=line_thick*te1, "60"=line_thick*tem, "75"=line_thick*te2 ))
		
		}

          
          oid <-  ""  ## id of the other spouse			
          
          if ( cid != "" ) {
            if ( ds_tree[ds_tree$id == cid , ]$father == lid ) {
              oid <- lid
              lid <- ds_tree[ds_tree$id == cid , ]$mother
            } else {
              oid <- ds_tree[ds_tree$id == cid , ]$father
            }
            
            ## with this we get a list of the ancestors that give the kinship
            ancestor_id <- unlist ( strsplit( ds_tree[ds_tree$id == lid , ]$kinship_ancestor, "," ) )
            
            ## check if it is an individual with a defined kinship
            ## because the ancestors responsible for the kinship is defined only for the wife.
            if ( length( ancestor_id ) == 0 & ds_tree[ds_tree$id == lid,]$kinship_shape > 0) {
              ancestor_id <- unlist ( strsplit( ds_tree[ds_tree$id == oid , ]$kinship_ancestor, "," ) )
            }
            
          } else {  ## It is the root
            ## kinship_child == "" means that one of the roots is a direct spouse of the other
            
            ancestor_id <- unlist ( strsplit( ds_tree[ds_tree$id == pkg.globals$ID_inicial[2] , ]$kinship_ancestor, "," ) )				
            oid <- pkg.globals$ID_inicial[1] 
            lid <- pkg.globals$ID_inicial[2]  	
          }					
                    
          ds_tree$kinship_highlight <<- 0  ## clean the variable
          
          for ( i in 1:length( ancestor_id) ) { ## for each ancestor find the path to the descentants
            path_2_descendant( ancestor_id[i], lid, oid )  ## it is a recursive method that sets the ds_tree$kinship_highlight variable
          }	
          
          ## now we are going to update the plot. Get the base plot a reset the alpha and size
          
          ###################
          ### These lines are the same as in the main plot, but filtering for the arc to highlight
          
          dst_bezier <- ds_tree[ ds_tree$kinship_highlight == 1 & ds_tree$x != ds_tree$x2_raw & ds_tree$geracao > 1,] %>% dplyr::rename ( x1 = x , y1 = y ) %>% 
            mutate ( xm = x1 + ( x2-x1) * 0.5 , ym = y1 + (y2-y1)*0.75 ) %>% 
			mutate ( s1 = 3 , sm = 4 , s2 = 5 ) %>%  ## size variables for the thicness
			select ( id, sex, ordem, to_color, x1, xm, x2 , y1, ym, y2, s1, sm, s2 ) %>% 
			unite( x1_uni, x1, y1, s1, sep = "_" )  %>% 
			unite( xm_uni, xm , ym , sm,  sep = "_" )  %>% 
			unite( x2_uni, x2, y2, s2, sep = "_" ) %>%
			gather("var_x", "values_x" , starts_with("x"))  %>% 
			arrange( id ) %>%
			separate( values_x , c("x","y","s"), sep="_") %>%
			mutate( x = as.numeric(x) , y = as.numeric(y) , s = as.integer(s) ) 
          
          ## We are now printing an arrow and the middle
          ## need to plot two segments, otherwise the alpha will be visible due to the overlap
          
			if ( input$use_arrows == "yes" ) {
				the_plot_local <- the_plot_local+geom_bezier2(data = dst_bezier , aes(x = x, y = y, group = ordem  , color = sex  ) , alpha = 1 , size = line_thick  ,  arrow = arrow(angle = isolate(input$arrow_angle) ,length =  unit( isolate(input$arrow_size),"cm"), type="closed" )  )

				the_plot_local <- the_plot_local+geom_bezier2( data = dst_bezier[dst_bezier$to_color >=2,] , aes(x = x, y = y, group = ordem  , color = sex ), alpha = 1 , size = line_thick  ,  arrow = arrow(angle = isolate(input$arrow_angle) ,length =  unit( isolate(input$arrow_size),"cm"), type="closed" )  )
			} else {  ## For the new method
				the_plot_local <- the_plot_local+geom_bezier2(data = dst_bezier , aes(x = x, y = y, group = ordem , color = sex, size = factor(15 * s) ) , alpha = 1 )
				
			}
          #### Horizontal lines need to be dealt with in separate
          
          ## dataframe with horizontal lines
          ds_tree_curves <- ds_tree[ ds_tree$kinship_highlight == 1 &  ds_tree$x == ds_tree$x2_raw & ds_tree$geracao > 1 , ]
          
          ## max width of the plot
          max_y <- max( na.omit ( ds_tree$y ) )
          
          if ( nrow( ds_tree_curves ) > 0 ) {  ## check if there are horizontal lines
            
			dst_bezier_hor <- ds_tree_curves %>% mutate ( x1 = x , y1 = y ) %>% 
				mutate ( xm = x1 +  1.5 -  ( max_y - abs(y2-y1) ) / max_y  , ym = ifelse( sex=="M" , y1 + (y2-y1)*0.75 , y1 + (y2-y1)*0.35 ) ) %>% 			  			   
				mutate ( s1 = 3 , sm = 4 , s2 = 5 ) %>%  ## size variables for the thicness
				select ( id, sex, to_color, x, y, x1, xm, x2, y1, ym, y2, s1, sm, s2) %>% 
				unite (to_group , id, x, y, sep="-" ) %>%
				unite( x1_uni ,x1, y1, s1, sep = "_" )  %>% 
				unite( xm_uni ,xm, ym, sm, sep = "_" )  %>% 
				unite( x2_uni ,x2, y2, s2, sep = "_" ) %>%
				gather("var_x", "values_x" , starts_with("x"))  %>% 
				separate( values_x , c("x","y", "s"), sep="_") %>%
				mutate( x = as.numeric(x) , y = as.numeric(y), s = as.integer(s) )
			
			if ( input$use_arrows == "yes" ) {            
				the_plot_local <- the_plot_local  +  geom_bezier2(data = dst_bezier_hor , aes(x = x, y = y, group = to_group ,  color = sex ) , alpha = 1 , size = line_thick , arrow = arrow(angle = isolate(input$arrow_angle) ,length =  unit( isolate(input$arrow_size),"cm"), type="closed" ) )    
			} else {  ## For the new method
				the_plot_local <- the_plot_local  +  geom_bezier2(data = dst_bezier_hor , aes(x = x, y = y, group = to_group ,  color = sex, size = factor(15 * s) ) , alpha = 1  )    
			
			}
          }
          ###
          ######
          
          ## and now the symbols
          local_ds_tree_1 <- ds_tree[ ds_tree$id %in% ancestor_id , ]
          local_ds_tree_2 <- ds_tree[ ds_tree$id == lid | ds_tree$id == oid , ]
          
            
		## kinship degree
		   the_plot_local <- the_plot_local + geom_label(data = ds_tree[ ds_tree$id == lid ,] , aes(  x= as.double(kinship_x), y= as.double(kinship_y), label = kinship ), size = input$kinships_font ,  nudge_x = max_x_coord * (input$kinship_y) / 100 , nudge_y = max_y_coord * (input$kinship_x) / 100 , hjust = input$kinship_alg , fill = fill_kinship , alpha = labels_alpha )
            
            
		## labels for the two spouses, but only if they are not the root
			the_plot_local <- the_plot_local + geom_label_repel(data = unique( local_ds_tree_2[ local_ds_tree_2$geracao > 1 & local_ds_tree_2$sex =="M", c("id","x2","y2","name", "sex") ]), aes(x=x2, y=y2 , label = name), size = input$names_font ,  nudge_x = max_x_coord * (input$name_y_M) / 100 , nudge_y = max_y_coord * (input$name_x_M) / 100 , hjust = input$name_alg_M ,  color = male_color, fill = fill_male , alpha = labels_alpha)
            
            the_plot_local <- the_plot_local + geom_label_repel(data = unique( local_ds_tree_2[ local_ds_tree_2$geracao > 1 & local_ds_tree_2$sex =="F"  , c("id","x2","y2","name", "sex") ]), aes(x=x2, y=y2 , label = name ), size = input$names_font ,  nudge_x = max_x_coord * (input$name_y_F) / 100 , nudge_y = max_y_coord * (input$name_x_F) / 100 , hjust = input$name_alg_F ,color = female_color, fill = fill_female , alpha = labels_alpha)					
            
         
		## individuals in the path
            the_plot_local <- the_plot_local + geom_label_repel(data = unique( ds_tree[ds_tree$geracao > 1 & ds_tree$sex =="M" & ds_tree$kinship_highlight > 0 , c("id","x2","y2","name", "sex") ]), aes(x=x2, y=y2 , label = name), size = input$names_font ,  nudge_x = max_x_coord * (input$name_y_M) / 100 , nudge_y = max_y_coord * (input$name_x_M) / 100 , hjust = input$name_alg_M ,  color = male_color, fill = fill_male , alpha = labels_alpha)
            
            the_plot_local <- the_plot_local + geom_label_repel(data = unique( ds_tree[ds_tree$geracao > 1 & ds_tree$sex =="F" & ds_tree$kinship_highlight > 0 , c("id","x2","y2","name", "sex") ]), aes(x=x2, y=y2 , label = name ), size = input$names_font ,  nudge_x = max_x_coord * (input$name_y_F) / 100 , nudge_y = max_y_coord * (input$name_x_F) / 100 , hjust = input$name_alg_F ,color = female_color, fill = fill_female , alpha = labels_alpha)
            
         
          
          the_plot_local <- the_plot_local + geom_point ( data = local_ds_tree_1,  aes( x = x2, y = y2 , fill= factor(sex) ) , color = "black" , shape = 21 , stroke = line_thick * 1.5, size = 7  )	
          
          the_plot_local <- the_plot_local + geom_point( data = local_ds_tree_2 , aes( x= x2, y= y2, color = factor(sex) ), shape = 21 , stroke = line_thick * 1.5 , size = 7 , fill = fill_kinship )
          
          
          the_plot_2_print <<- the_plot_local
          the_plot_highlight <<- the_plot_local
          
          
          
        } else if ( conn_highlight == TRUE ) {
		
			conn_highlight <<- FALSE 
			the_plot_local <- the_plot_base
			
			the_plot_local <- the_plot_local + geom_point( data = ds_plain[ds_plain$id == conn_highlight_id,] , aes (x = x2, y = y2, group = id) , color = "black" ,  size =  ca_circle_size * 1.75  )			
			the_plot_local <- the_plot_local + geom_point( data = ds_plain[ds_plain$id == conn_highlight_id,] , aes (x = x2, y = y2, color = sex , group = id) , size =  ca_circle_size  )

			the_plot_local <- the_plot_local + geom_path( data = ds_plain[ds_plain$id == conn_highlight_id,] , aes (x = x2, y = y2, color = sex , group = id) ,  linetype = "dashed", size =  line_thin * 3  )
			
			the_plot_2_print <<- the_plot_local
			the_plot_highlight <<- the_plot_local
          
				
		} else { ## NO highlight
          ## Standard plot
          
          if ( the_sel_ind_id != "" ) {  ## If there is a single individual to highlight
            if ( input$plain_tree == TRUE ) {

				the_plot <- the_plot + geom_point( data = ds_plain[ds_plain$id ==the_sel_ind_id, ], aes ( x = x2, y = y2, colour = factor(sex) ) ,shape=19, size =  ca_circle_size * 1.5    )
			} else {
				the_plot <- the_plot + geom_point( data = ds_tree[ds_tree$id ==the_sel_ind_id, ], aes ( x = x2, y = y2, colour = factor(sex), alpha = factor(to_color) ) ,shape=19, size =  ca_circle_size * 1.5    )			
			}
          }
          
          the_plot_2_print <<- the_plot
          the_plot_highlight <<- the_plot
          max_y2 <- max(na.omit(ds_tree$y2))
          
          if ( !is.null(ranges$x) ){
            
            ## check for hover
              x_min_rect <- floor( ranges$x[1] )
              x_max_rect <- ceiling( ranges$x[2] ) 
              y_min_rect <- ranges$y[1]
              y_max_rect <- ranges$y[2]
              
              the_plot_highlight <- the_plot_highlight + coord_flip( xlim = c(y_min_rect,y_max_rect) )  + scale_y_reverse( limits = c( x_max_rect, x_min_rect ) )		
              

          } ### ... if ( !is.null(ranges$x) ){
        } ### END global if.
        
        
        
        output$plot_double_click_info <- renderPrint({ cat( "") })	
        render_end <- Sys.time ()
if ( print_time == TRUE)         print ( paste ( "Time - render:" , difftime(render_end, render_start, units='mins') ) )	
        total_time <- render_end - init_time

if ( print_time == TRUE) print ( paste ( "TOTAL TIME:" , difftime(render_end, init_time, units='mins')  ) )	
if ( print_time == TRUE) print ( paste ( "TOTAL TIME sec:" , difftime(render_end, init_time, units='secs')  ) )			
        
        the_plot_highlight	 ## This is the plot to render
      })
      
    }
    ### LLL
    
    
    
    
  })
  ################################################################
  
  ##########################################################
  ### Prints dataset in one panel		
  output$table_1 <- DT::renderDataTable({
    withProgress(message = "Updating the table", detail="", value=0.9,{
      ds
    })
  } )
  ##########################
  
  
  
  gedcom_file <- observeEvent( input$gedcom ,{	
     print("IN - observeEvent(input$gedcom,{ ")
    
    input_filename <- input$gedcom$name
    output_xlsx_file <- ""	
    if( stri_sub( input_filename ,-4,-1) != ".ged" ){
      output_xlsx_file <- input_filename
    } else {
      output_xlsx_file <-  paste( stri_sub( input_filename ,1 ,-4) , "xlsx" , sep = "" )
    }	
    
    updateTextInput(session, "output_file", value = output_xlsx_file )
    
  })
  
  
  
  #####################################################################
  #####################################################################
  ## gedcom parser
  ## Retrieves from a gedcom file a set of selected attributes for all the persons in it
  ## data frame to keep the records
  
  gedcom_file <- observeEvent(input$parse_file,{	
   # print("IN - gedcom_file <- reactive({ ")
    
    if ( input$output_file != "" ) {
      
      output_file_name <- input$output_file 		
      withProgress(message = "Reading gedcom file", detail="", value=0.9,{
        
        parse_gedcom( input$gedcom , output_file_name  )					
      })
    } else { 
      
      withProgress(message = "Reading gedcom file", detail="", value=0.9,{
        parse_gedcom( input$gedcom   )				
      })	
    }
  })
  
  
  #####################################################################  
  # When a single-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$the_plot_click, {
    # print( "clicked in the plot")
    if ( nrow (ds ) > 0 ) {
      
      if (!is.null(input$plot_hover)){

          inputy = input$plot_hover$x
          inputx = input$plot_hover$y			
        
		
        local_index <- NULL 
		dist <- 999
		local_id <- "III"
		if ( input$plain_tree == TRUE ) {
			dist <- sqrt ( (ds_plain$x2/max_ger- inputx/max_ger)^2 + (ds_plain$y2/max_y_coord - inputy/max_y_coord)^2 )      
			local_id <- ds_plain[which.min(dist),]$id     
		} else {
			dist <- sqrt ( (ds_tree$x2/max_ger- inputx/max_ger)^2 + (ds_tree$y2/max_y_coord - inputy/max_y_coord)^2 )      
			local_id <- ds_tree[which.min(dist),]$id     
		}
		
		
        if(min(na.omit(dist) ) < 0.1) { ## Necessary this because of the difference in range between the x and y coordinates
          if ( nrow( ds_tree[which.min(dist),] ) > 0  ) {

            id_selected <- TRUE
            local_index = which (ds_tree$id == local_id & ds_tree$x2 == ds_tree$x2_raw )	
          }
          
          if ( length ( local_index ) > 0 ){
            if ( length ( local_index ) > 1 ) local_index = local_index[1]
            
            text_2_print <- paste( ds_tree[ local_index , ]$id , "("  , ds_tree[ local_index ,]$sex , ")\n", sep="")
            text_2_print <- paste ( text_2_print ,  ds_tree[ local_index , ]$fullname , "\n")
            text_2_print <- paste ( text_2_print ,  ds_tree[ local_index , ]$yearrange , "\n" )
            
            ds_filhos <- NULL
            text_2_print_f <- paste ( text_2_print ,  "Children:\n" , sep="")
            
            if (ds_tree[ local_index ,]$sex == "M" ){
              ds_filhos <- ds[ds$fatherid == local_id,]
            } else {
              ds_filhos <- ds[ds$motherid == local_id,]
            }
            ds_filhos <- ds_filhos[complete.cases(ds_filhos$name),]
            ds_filhos$birthdate <- sapply(ds_filhos$birthdate,get_year_from_datestring)
            ds_filhos$deathdate <- sapply(ds_filhos$deathdate,get_year_from_datestring)
            ds_filhos$age <- as.integer(ds_filhos$deathdate) - as.integer(ds_filhos$birthdate)
            
            ds_filhos <- ds_filhos[order(ds_filhos$birthdate),]
            
            for (i in 1:nrow(ds_filhos)){
              text_2_print_f <- paste ( text_2_print_f ,   ds_filhos[i,]$birthdate ,"/", ds_filhos[i,]$age ,"(" ,ds_filhos[i,]$sex,"), " ,ds_filhos[i,]$personid ,"-", ds_filhos[i,]$name , "\n", sep="")
              
            }
            output$plot_hoverinfo_1 <- renderPrint({ cat( text_2_print_f ) })	
          }		
        } else {
          ## the click was in an empty space
          output$plot_hoverinfo_1 <- renderPrint({ cat( "" ) })				
        }
      }
    }
  })
  
  
  
  ##################################################################### 
  ### double click must be done after selecting the option. That is, selecting an option 
  ### cannot capture a previous double-click
  
  observeEvent(input$root_click, {
    
    double_click_id <<- ""
    isolate ( output$plot_double_click_info <- renderPrint({ cat( "") })		 )
    
  })
  
 
  #####################################################################

  observeEvent(input$plain_tree, {

    isolate ( 
	
		if ( input$plain_tree == FALSE ) {        
			updateCheckboxInput(session, "pt_conn",  value = FALSE )		
	
		} else {
			updateCheckboxInput(session, "high_both",  value = FALSE )			
		}
	)
    
  })
  

  #####################################################################  
  observeEvent(input$high_both, {
    
    isolate ( 
	
		if ( input$high_both == TRUE ) {
        
			updateCheckboxInput(session, "pt_conn",  value = FALSE )		
			updateCheckboxInput(session, "plain_tree",  value = FALSE )		
		}
	) 
  })  
 
  #######################################################################################
  ## When there is a single-click in the plot it selects the closest node as the root 1, root _2 , or ...
  observeEvent(input$the_plot_dblclick, {
    if ( nrow (ds ) > 0 ) {
      
      ## gets the click coordinates
		inputy = input$plot_hover$x
		inputx = input$plot_hover$y				

      
      id_selected <- FALSE
      local_id <- ""
      local_name <- ""
      text_2_print <- ""
      dist <- 999
	  
	  ## Now that we can be plotting our tree or a plain tree we need to make sure
	  ## that when getting the mouse click position we are reading the coordinates on the right plot
	  
	  if ( input$plain_tree == TRUE ) {
		dist <- sqrt ( (ds_plain$x2/max_ger- inputx/max_ger)^2 + (ds_plain$y2/max_y_coord - inputy/max_y_coord)^2 )
        local_id <- ds_plain[which.min(dist),]$id  ## id of the selected person
        local_name <- ds_plain[which.min(dist),]$name  ## name of the selected person
	  
	  } else {
		dist <- sqrt ( (ds_tree$x2/max_ger- inputx/max_ger)^2 + (ds_tree$y2/max_y_coord - inputy/max_y_coord)^2 )
		local_id <- ds_tree[which.min(dist),]$id  ## id of the selected person
		local_name <- ds_tree[which.min(dist),]$fullname  ## name of the selected person

      }
	  
      if(min(na.omit(dist) ) < 0.1) {## Necessary this because of the difference in range between the x and y coordinates
        
        updateCheckboxInput(session, "high_both",  value = FALSE )		
        text_2_print <- "" 
        
        if ( nrow( ds_tree[which.min(dist),] ) > 0  ) {

          double_click_id <<- local_id
          text_2_print <- paste(local_id,"-", local_name, sep="")   ## text to print on the panel
          
          id_selected <- TRUE
        }
        
        if ( input$root_click == "1" & id_selected ) {
          updateSelectInput(session, "root_1",  choices = ds$personid, selected =  local_id )
          output$plot_double_click_info <- renderPrint({ cat( text_2_print) })						
        
		} else if ( input$root_click == "2" & id_selected ){
          updateSelectInput(session, "root_2",  choices = ds$personid, selected =  local_id )		
          output$plot_double_click_info <- renderPrint({ cat( text_2_print) })						
       
	    } else if ( input$root_click == "3" & id_selected  ){
          
          if (  input$plain_tree == FALSE ) {
			 # print ( ds_tree[ds_tree$kinship_shape == 1 ,] )
			  if ( length ( ds_tree[ds_tree$kinship_shape == 1 ,]$id )> 0 &  local_id %in% ds_tree[ds_tree$kinship_shape == 1 ,]$id ) {
				kinship_main_highlight_id <<- local_id 
				kinship_main_highlight <<- TRUE	
				conn_highlight <<- FALSE
				output$plot_double_click_info <- renderPrint({ cat( text_2_print) })	
			  }					

		  } else {
		  
			  if ( length ( ds_plain[ds_plain$count > 1 , ]$id ) > 0 &  local_id %in% ds_plain[ds_plain$count > 1 , ]$id ) {
				conn_highlight_id <<- local_id 
				conn_highlight <<- TRUE	
				kinship_main_highlight <<- FALSE
				output$plot_double_click_info <- renderPrint({ cat( text_2_print) })	
			  }		  
			
		  
		  }
	  
		  descendants_highlight <<- FALSE          
        } else if ( input$root_click == "4" & input$plain_tree == FALSE  ){
          
          if ( length (ds_t$id ) > 0  & local_id %in% ds_t$id ) {
            descendants_highlight_id <<- local_id 
            descendants_highlight <<- TRUE
            output$plot_double_click_info <- renderPrint({ cat( text_2_print) })	
          }
          kinship_main_highlight <<- FALSE			
        }
        
      } else {
        # print("NOTHING SELECTED")
        double_click_id <<- ""
        output$plot_double_click_info <- renderPrint({ cat( "") })	
      } 
    }
  })
  
  
  ########################################################################
  ### method to compute the kinship between spouses
  couple_kinship <- function ( father , mother , mother_sex ) {
    
    # print( "In: couple_kinship() " )
    to_return <- vector()
    to_return[1] <- ""
    to_return[2] <- ""
    
    ds_kinship <<- ds_kinship[0,]  ## delete rows
    check_parents( mother , "main" , 0 , mother , father ) 	
    check_parents( father , "main" , 0 , father , mother )
   #  print( ds_kinship )
	
    if ( length( unique ( ds_kinship$person_id) ) >= 1 ) {	
      if ( father %in% ds_kinship[ds_kinship$person_id == mother,]$ancestor_id ) {
        
        num_steps <- min( as.numeric( ds_kinship[ds_kinship$person_id == mother & ds_kinship$ancestor_id == father,]$steps ) )
        sex <- ds_tree[ds_tree$id == father , ]$sex
        grau_parentesco <- ""
        
        if( num_steps == 1 ) {
          if ( sex == "M" ){
            grau_parentesco <- "father"
          } else {
            grau_parentesco <- "mother"
          }						
        } else if( num_steps == 2 ) {
          if ( sex == "M" ){
            grau_parentesco <- "grandfather"
          } else {
            grau_parentesco <- "grandmother"
          }	
        } else if( num_steps > 2 ) {
          
          grau_parentesco <-  paste( rep( "great-", num_steps - 2) , collapse ="")
          if ( sex == "M" ){
            
            grau_parentesco <- paste ( grau_parentesco , "grandfather", sep ="")
          } else {
            grau_parentesco <- paste ( grau_parentesco , "grandmother", sep ="")
          }							
        }
        to_return[1] <- paste ( grau_parentesco  )
        to_return[2] <- paste( mother )				
      } else if (	 mother %in% ds_kinship[ds_kinship$person_id == father,]$ancestor_id ) {
        
        num_steps <- min( as.numeric( ds_kinship[ds_kinship$person_id == father & ds_kinship$ancestor_id == mother,]$steps ) )
        sex <- ds_tree[ds_tree$id == mother , ]$sex
        grau_parentesco <- ""
        
        if( num_steps == 1 ) {
          if ( sex == "M" ){
            grau_parentesco <- "father"
          } else {
            grau_parentesco <- "mother"
          }						
        } else if( num_steps == 2 ) {
          if ( sex == "M" ){
            grau_parentesco <- "grandfather"
          } else {
            grau_parentesco <- "grandmother"
          }	
        } else if( num_steps > 2 ) {
          
          grau_parentesco <-  paste( rep( "great-", num_steps - 2) , collapse ="")
          
          if ( sex == "M" ){
            
            grau_parentesco <- paste ( grau_parentesco , "grandfather", sep ="")
          } else {
            grau_parentesco <- paste ( grau_parentesco , "grandmother", sep ="")
          }							
        }
        to_return[1] <- paste ( grau_parentesco  )
        to_return[2] <- paste( father )				
        
      } else {
        
        ## very important here
        ## the intersection reduces the dataframe to the paths to the ancestors that are common to both the father and the mother
        ## Note that there may be more than one ancestor in common to both the mother and the father
        ## this gives a vector with the IDs of such ancestors
        in_both <- intersect ( ds_kinship[ds_kinship$person_id == father,]$ancestor_id , ds_kinship[ds_kinship$person_id == mother,]$ancestor_id ) 
        
        ## another dataframe to check for the closest ancestor
        ## here we are calculating the shortest path from each parent to each ancestor and filtering for the min
        
        closest_ancestors <- ds_kinship %>% filter ( ancestor_id %in% in_both ) %>% group_by ( person_id , ancestor_id , sex ) %>%
          summarise ( steps = min (steps) ) %>% 
          group_by ( ancestor_id ) %>% 
          summarise ( sum_paths = sum ( as.numeric(steps) ) ) %>% ungroup () %>% 
          mutate ( min_path = min( sum_paths) ) %>% 
          filter( sum_paths == min_path) 		
        
        
        
        if ( nrow( closest_ancestors ) > 0 ) {
          
          ds_kinship <- ds_kinship %>% filter ( ancestor_id %in% closest_ancestors$ancestor_id )  %>% group_by ( person_id, spouse_id, ancestor_id, sex ) %>% summarise (steps = min (steps))
          
          ds_kinship$steps <- as.numeric( ds_kinship$steps )
          
          ## this var keeps track the number of paths of 
          count_paths <- nrow ( ds_kinship ) / 4
          
          father_steps <- 0
          mother_steps <- 0				
          
          father_steps <- min ( ds_kinship[ds_kinship$person_id == father & ds_kinship$ancestor_id == closest_ancestors[1,]$ancestor_id, ]$steps )
          mother_steps <- min ( ds_kinship[ds_kinship$person_id == mother & ds_kinship$ancestor_id == closest_ancestors[1,]$ancestor_id, ]$steps  )		
          
          ## only when we have two roots the right-side may be a male
          grau_parentesco <- kinship_degree ( father_steps, mother_steps , "F" )
          
          
          to_return[1] <- grau_parentesco
          if ( count_paths != 1 ) {
            to_return[1] <- paste ( count_paths , "x" , grau_parentesco  )
          }
          to_return[2] <- paste( closest_ancestors$ancestor_id,collapse=",")
        }
      }	     
      
    }
    #print( to_return )	
    return ( to_return )
    
  }
  
  
  ########################################################################
  ### Method to be run for every individual having IC > 0
  ### The goal is to print the kinship of his mother with respect to his father
  
  parents_kinship <- function ( ind_id ) {
    # print( "In: parents_kinship() " )
    
    mother <- unique ( ds_tree[ds_tree$id == ind_id, ]$mother )
    father <- unique ( ds_tree[ds_tree$id == ind_id, ]$father )
    
    ds_kinship <<- ds_kinship[0,]  ## delete rows
    check_parents_dst( mother , ind_id , 0 , mother , father , 0) 		 
    check_parents_dst( father , ind_id , 0 , father , mother , 0)		
    
    ## very important here
    ## the intersection reduces the dataframe to the paths to the ancestors that are common to both the father and the mother
    ## Note that there may be more than one ancestor in common to both the mother and the father
    ## this gives a vector with the IDs of such ancestors
    in_both <- intersect ( ds_kinship[ds_kinship$person_id == father,]$ancestor_id , ds_kinship[ds_kinship$person_id == mother,]$ancestor_id ) 
    
    if ( length (in_both) > 0 ) {
      ## another dataframe to check for the closest ancestor
      ## here we are calculating the shortest path from each parent to each ancestor and filtering for the min
      
      closest_ancestors <- ds_kinship %>% filter ( ancestor_id %in% in_both ) %>% 
        group_by ( person_id , ancestor_id , sex ) %>% 
        summarise ( steps = min (steps) ) %>% 
        group_by ( ancestor_id ) %>% 
        summarise ( sum_paths = sum ( as.numeric(steps) ) ) %>% 
        ungroup () %>% 
        mutate ( min_path = min( sum_paths) ) %>% 
        filter( sum_paths == min_path) 	
      
      ds_kinship <- ds_kinship %>% 	filter ( ancestor_id %in% closest_ancestors$ancestor_id ) %>% 
        group_by ( person_id, spouse_id, ancestor_id, sex ) %>% 
        summarise (steps = min (steps))
      
      ds_kinship$steps <- as.numeric( ds_kinship$steps )
      
      ## this var keeps track the number of paths of 
      count_paths <- nrow ( ds_kinship ) / 4
      
      father_steps <- 0
      mother_steps <- 0
      
      father_steps <- min ( ds_kinship[ds_kinship$person_id == father & ds_kinship$ancestor_id == closest_ancestors[1,]$ancestor_id, ]$steps )
      mother_steps <- min ( ds_kinship[ds_kinship$person_id == mother & ds_kinship$ancestor_id == closest_ancestors[1,]$ancestor_id, ]$steps  )		
      
      ## only when we have two roots the right-side may be a male
      
      #print("Going for grau de parentesco")
      grau_parentesco <- kinship_degree ( father_steps, mother_steps , "F" )
      #print("done grau de parentesco")
      
      to_return <- vector()
      to_return[1] <- ""
      to_return[2] <- ""
      
      to_return[1] <- grau_parentesco
      if ( count_paths != 1 ) {
        to_return[1] <- paste ( count_paths , "x" , grau_parentesco  )
      }
      to_return[2] <- paste( closest_ancestors$ancestor_id,collapse=",")
    } else {
      to_return <- vector()
      to_return[1] <- ""
      to_return[2] <- ""			
      
    }
    
    return ( to_return )		
  }
  
  ##################################################################################3
  ### function called by the parents_kinship() function()
  ##	it is recursive
  ##  if ind_id is a common ancestor ( $count > 1) insert it in the ds_kinship dataframe
  ##	in any case (if above is true or not) explore parents in search for an individual having $count > 1
  
  check_parents <- function ( ind_id , child_id , num_steps , init_id , spouse_id ) {
    
   #  print( "IN --- check_parents")
    ## check if it is an individual with several occurrences in the tree
    if ( ds_tree[ds_tree$id == ind_id & ds_tree$child == child_id, ]$count > 1 & num_steps > 0) { ## the last condition is to make sure there are no self links
      
      ## if yes, insert its record in the dataframe
      local_kinship <- c(init_id, spouse_id, ind_id, ds_tree[ds_tree$id == ind_id & ds_tree$child == child_id, ]$sex, num_steps)
      
      ds_kinship <<- rbind ( ds_kinship , local_kinship )
      names( ds_kinship ) <<- c("person_id", "spouse_id","ancestor_id","sex","steps")					
    }
   # print("11") 
    ## Checks if the parents are the root individuals
    if ( num_steps == 0 &  ds_tree[ds_tree$id == ind_id, ]$child[1] == "main" ) {
   # print("111")       
      ## There was a problem with individuals who did not have their parents defined because the if conditions
      ## where checking NAs and returning errors
      ## the lenght condition seems to have solved that
      
      parents_root1 <- na.omit( c ( ds_tree[ds_tree$id == ind_id, ]$father , ds_tree[ds_tree$id == ind_id, ]$mother ) ) 
      parents_root2 <-na.omit( c ( ds_tree[ds_tree$id == spouse_id, ]$father , ds_tree[ds_tree$id == spouse_id, ]$mother )	 )
  
## the following lines were inserted to overcome a problema with NAs in the boolean comparisons
check_1 <- FALSE
check_2 <- FALSE
check_3 <- FALSE
check_4 <- FALSE
if ( !is.na( ds_tree[ds_tree$id == ind_id, ]$father ) & ds_tree[ds_tree$id == ind_id, ]$father == spouse_id  ) check_1 <- TRUE  
if ( !is.na( ds_tree[ds_tree$id == ind_id, ]$mother ) & ds_tree[ds_tree$id == ind_id, ]$mother == spouse_id  ) check_2 <- TRUE 
if ( !is.na( ds_tree[ds_tree$id == spouse_id, ]$father ) & ds_tree[ds_tree$id == spouse_id, ]$father == ind_id  ) check_3 <- TRUE 
if ( !is.na( ds_tree[ds_tree$id == spouse_id, ]$mother ) & ds_tree[ds_tree$id == spouse_id, ]$mother == ind_id  ) check_4 <- TRUE 

      if (  length(parents_root1) > 0  & ( check_1 | check_2 )) {
   # print("111.a")        
        ## if yes, insert its record in the dataframe
        local_kinship <- c(init_id, spouse_id, spouse_id, ds_tree[ds_tree$id == ind_id & ds_tree$child == child_id, ]$sex, 1)
        
        ds_kinship <<- rbind ( ds_kinship , local_kinship )
        names( ds_kinship ) <<- c("person_id", "spouse_id","ancestor_id","sex","steps")			
        
      } else if (  length(parents_root2) > 0  & ( check_3 | check_4 ) ) {
    # print("111.b")          
        ## if yes, insert its record in the dataframe
        local_kinship <- c(spouse_id, init_id, init_id, ds_tree[ds_tree$id == spouse_id & ds_tree$child == child_id, ]$sex, 1)
        
        ds_kinship <<- rbind ( ds_kinship , local_kinship )
        names( ds_kinship ) <<- c("person_id", "spouse_id","ancestor_id","sex","steps")			
        
      } else if ( length(parents_root1) > 0  & all ( parents_root1 == parents_root2 ) ){
     # print("111.c")          
        for ( i in 1: length ( parents_root1) ) {
          
          local_kinship <- c(spouse_id, init_id, parents_root1[i], unique(ds_tree[ds_tree$id == parents_root1[i], ]$sex), 1)
          
          ds_kinship <<- rbind ( ds_kinship , local_kinship )
        }
        names( ds_kinship ) <<- c("person_id", "spouse_id","ancestor_id","sex","steps")						
      }
    }
     # print("22")    
    ## calls the method for his mother
    if ( !is.na( ds_tree[ds_tree$id == ind_id & ds_tree$child == child_id, ]$mother ) )  { ## mother is defined
      ## id of his mother
      local_mother_id <- ds_tree[ds_tree$id == ind_id & ds_tree$child == child_id , ]$mother
      
      ## should be able to simplify this two IFs ( but the second if sometimes give an error without the first if )
      
      ## checks if his mother has a record in the ds_tree
      ## the condition includes mother and child id to make sure that it is the exact link
      if ( nrow ( ds_tree[ds_tree$id == local_mother_id & ds_tree$child == ind_id , ] ) > 0 ) {   ## checks if the link exists
        
        ## checks if the record is a path ( $to_color > 1 )to an individual that occurs more than once in the tree
        if ( ds_tree[ds_tree$id == local_mother_id & ds_tree$child == ind_id , ]$to_color > 1 ) {  ## checks if the variable in the link is > 1
          #  print( paste( local_mother_id , "in the path" ) )
          check_parents ( local_mother_id , ind_id , num_steps + 1 , init_id , spouse_id)
        }
      }				
    }
 # print("33")    
    ## calls the method for his father
    if ( !is.na( ds_tree[ds_tree$id == ind_id & ds_tree$child == child_id  , ]$father ) ) { ## father is defined
      local_father_id <- ds_tree[ds_tree$id == ind_id & ds_tree$child == child_id , ]$father
      
      if ( nrow ( ds_tree[ds_tree$id == local_father_id & ds_tree$child == ind_id, ] ) > 0 ) {
        
        if ( ds_tree[ds_tree$id == local_father_id & ds_tree$child == ind_id, ]$to_color > 1 ) {
          
          check_parents ( local_father_id , ind_id , num_steps + 1 , init_id , spouse_id)
          
        } 
      }
    }
    
  }
  
  
  ##################################################################################3
  ### function called by the parents_kinship() function()
  ##	it is recursive
  ##  if ind_id is a common ancestor ( $count > 1) insert it in the ds_kinship dataframe
  ##	in any case (if above is true or not) explore parents in search for an individual having $count > 1
  
  check_parents_dst <- function ( ind_id , child_id , num_steps , init_id , spouse_id , check_len ) {
    
     # print( "IN --- check_parents DST")
    
    ind_father <- pkg.globals$dst_father [ pkg.globals$dst_id  == ind_id & pkg.globals$dst_child == child_id ]
    ind_mother <- pkg.globals$dst_mother [ pkg.globals$dst_id  == ind_id & pkg.globals$dst_child == child_id ]
    ind_sex <- pkg.globals$dst_sex[ pkg.globals$dst_id == ind_id & pkg.globals$dst_child == child_id  ]
    spouse_father <- pkg.globals$dst_father [ pkg.globals$dst_id  == spouse_id ]
    spouse_mother <- pkg.globals$dst_mother [ pkg.globals$dst_id  == spouse_id ]
    spouse_sex <- pkg.globals$dst_sex [ pkg.globals$dst_id == spouse_id & pkg.globals$dst_child == child_id  ]	
    
    
    ## check if it is an individual with several occurrences in the tree
    if ( pkg.globals$dst_count[ pkg.globals$dst_id == ind_id & pkg.globals$dst_child == child_id ] > 1 & num_steps > 0) { ## the last condition is to make sure there are no self links
      
      local_kinship <- c(init_id, spouse_id, ind_id, pkg.globals$dst_sex[ pkg.globals$dst_id == ind_id & pkg.globals$dst_child == child_id ] , num_steps)
      
      ds_kinship <<- rbind ( ds_kinship , local_kinship )
      names( ds_kinship ) <<- c("person_id", "spouse_id","ancestor_id","sex","steps")					
    } else if ( num_steps == 0 &  pkg.globals$dst_child[ pkg.globals$dst_id == ind_id ][1] == "main" ) { ## Checks if the parents are the root individuals
      
      ## There was a problem with individuals who did not have their parents defined because the if conditions
      ## where checking NAs and returning errors
      ## the lenght condition seems to have solved that
      
      parents_root1 <- na.omit( c ( ind_father , ind_mother ) ) 
      parents_root2 <- na.omit( c ( spouse_father , spouse_mother )	 )
      
      if (  length(parents_root1) > 0  & ( ind_father == spouse_id  |  ind_mother == spouse_id ) ) {
        
        ## if yes, insert its record in the dataframe
        local_kinship <- c( init_id , spouse_id , spouse_id , ind_sex , 1)		
        ds_kinship <<- rbind ( ds_kinship , local_kinship )
        names( ds_kinship ) <<- c("person_id", "spouse_id","ancestor_id","sex","steps")			
        
      } else if (  length(parents_root2) > 0  & ( spouse_father == ind_id | spouse_mother == ind_id ) ) {
        
        ## if yes, insert its record in the dataframe
        local_kinship <- c(spouse_id , init_id , init_id , spouse_sex , 1)				
        ds_kinship <<- rbind ( ds_kinship , local_kinship )
        names( ds_kinship ) <<- c("person_id", "spouse_id","ancestor_id","sex","steps")			
        
      } else if ( length(parents_root1) > 0  & all ( parents_root1 == parents_root2 ) ){
        
        for ( i in 1: length ( parents_root1) ) {			
          local_kinship <- c(spouse_id, init_id, parents_root1[i], unique( pkg.globals$dst_sex[ pkg.globals$dst_id == parents_root1[i] ] ), 1)				
          ds_kinship <<- rbind ( ds_kinship , local_kinship )
        }
        names( ds_kinship ) <<- c("person_id", "spouse_id","ancestor_id","sex","steps")						
      }
    }
    
    if ( check_len < isolate( input$kinship_limit) ) {
      ## even if we have found a common ancestor we need to continue explore the tree because the ancestor found may
      ## not be the same of the spouse
      
      ## calls the method for his mother
      if ( !is.na( ind_mother ) )  { ## mother is defined
        
        ## checks if his mother has a record in the ds_tree
        ## the condition includes mother and child id to make sure that it is the exact link
        
        if ( length ( pkg.globals$dst_id[ pkg.globals$dst_id == ind_mother & pkg.globals$dst_child == ind_id & pkg.globals$dst_to_color > 1  ] ) > 0 ) {   ## checks if the link exists
          
          #if ( ds_tree[ dst_id == ind_mother & dst_child == ind_id , ]$to_color > 1 ) {  ## checks if the variable in the link is > 1
          check_parents_dst ( ind_mother , ind_id , num_steps + 1 , init_id , spouse_id, check_len +1 )
          #}
        }				
      }
      
      ## calls the method for his father
      if ( !is.na( ind_father ) ) { ## father is defined
        if ( length ( pkg.globals$dst_id [ pkg.globals$dst_id == ind_father & pkg.globals$dst_child == ind_id & pkg.globals$dst_to_color > 1 ] ) > 0 ) {			
  6        #if ( ds_tree[ dst_id == ind_father & dst_child == ind_id , ]$to_color > 1 ) {
          check_parents_dst ( ind_father , ind_id , num_steps + 1 , init_id , spouse_id , check_len + 1)
          #} 
        }
      }
    }
  }	### end ... check_parents_dst <- function ( ind_id , child_id , num_steps , init_id , spouse_id ) {
  
  
  
  
  ## returns the kinship between two individuals given their respective number of steps for their
  ## common ancestor
  
  kinship_degree <- function ( father_steps , mother_steps , mother_sex) {
    
    #	  print(" IN kinship_degree() " )
    #	  print( paste ( "steps: " , father_steps , " - " , mother_steps ))
    if ( ( father_steps > 0 | mother_steps > 0 ) & (father_steps > 1 | mother_steps > 1) ) {
      if ( father_steps == mother_steps) {
        grau_parentesco <- paste( toOrdinal(father_steps - 1), "cousin"  ) 
        # print( grau_parentesco )
        
      } else if ( father_steps < mother_steps) {
        
        if ( father_steps == 1 ){
          if ( mother_steps == 2 ) {
            if ( mother_sex == "M" ){
              grau_parentesco <- "nephew"
            } else {
              grau_parentesco <- "niece"
            }
          } else if ( mother_steps == 3 ) {
            if ( mother_sex == "M" ){
              grau_parentesco <- "grand-nephew"
            } else {
              grau_parentesco <- "grand-niece"
            }
            
          } else {
            grau_parentesco <- paste( rep( "great-", mother_steps - 3) , collapse ="")
            if ( mother_sex == "M" ){
              grau_parentesco <- paste ( grau_parentesco , "grand-nephew", sep ="")
            } else {
              grau_parentesco <- paste ( grau_parentesco , "grand-niece", sep ="")
            }						
          }
        } else {
          
          grau_parentesco <-  paste( toOrdinal(father_steps - 1), " cousin " ,  (mother_steps - father_steps) , "R" , sep="") 
        }
        
      } else if ( father_steps > mother_steps){
        
        if ( mother_steps == 1 ){
          if ( father_steps == 2 ) {
            if ( mother_sex == "M" ){
              grau_parentesco <- "uncle"
            } else {
              grau_parentesco <- "aunt"
            }					
            
          } else if ( father_steps == 3 ) {
            if ( mother_sex == "M" ){
              grau_parentesco <- "grand-uncle"
            } else {
              grau_parentesco <- "grand-aunt"
            }
            
          } else  {
            grau_parentesco <- paste( rep( "great-", father_steps - 3) , collapse ="")
            if ( mother_sex == "M" ){
              grau_parentesco <- paste ( grau_parentesco , "grand-uncle", sep ="")
            } else {
              grau_parentesco <- paste ( grau_parentesco , "grand-aunt", sep ="")
            }
          }
        } else {
          
          g1 <- father_steps - 1 - (father_steps - mother_steps )
          r1 <- (father_steps - mother_steps )
          grau_parentesco <- paste ( toOrdinal(g1), " cousin ",r1,"R", sep="")
        }
      }
      # print( "going out of kinship_degree")
      return ( grau_parentesco )
    } else if ( father_steps == 1 & mother_steps == 1) {
      if ( mother_sex == "M" ){
        grau_parentesco <- "brother"
      } else {
        grau_parentesco <- "sister"
      }
      return ( grau_parentesco )
    }
  }
  
  
  
##############################################################	
  path_2_descendant <- function( person_id , target_one, target_two ) {
    
    # print( paste ( "personid: " , person_id ) )
    return_var <- 0
    aux_var <- 0
    person_sex <- ds_tree[ds_tree$id == person_id, ]$sex
    
    children <- NULL
    
    if ( person_sex[1] == "M" ) {
      
      children <-  ds_tree %>% filter (father == person_id) %>% dplyr::pull( id )
    } else {
      
      children <-  ds_tree %>% filter (mother == person_id) %>% dplyr::pull( id )
    }
    
    if ( length (children) > 0 ) { 
      for ( i in 1:length( children ) ) {
        
        if ( target_one == children[i] ) {
          ds_tree[ds_tree$id == person_id  & ds_tree$child == children[i], ]$kinship_highlight <<- 1	
          return_var <- 1
        } else if ( target_two == children[i] ) {
          ds_tree[ds_tree$id == person_id & ds_tree$child == children[i] , ]$kinship_highlight <<- 1	
          return_var <- 1		
        } else {
          
          aux_var <- path_2_descendant(children[i] , target_one, target_two)		
          return_var <- return_var + aux_var			
          
          if (aux_var == 1 ) {
            ds_tree[ds_tree$id == person_id & ds_tree$child == children[i], ]$kinship_highlight <<- 1							
          } else {
            ds_tree[ds_tree$id == person_id & ds_tree$child == children[i], ]$kinship_highlight <<- 0							
          }
        }
      }
    }
    if ( return_var > 0 ) {
      
      return_var <- 1  ## in case is above one we limit it to 1
    } 
    return( return_var)
    
  } ## end ... path_2_descendant
  
  
  
  
} ## END MAIN SERVER


shinyApp(ui = ui, server = server)

}  ## standalone function



