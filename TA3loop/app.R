library(shiny)
library(dplyr)
library(shinyjs)
library(reshape2)
library(gtools)
library(MASS)
library(foreach)
library(iterators)
library(doParallel)
library(randomGLM)
library(msir)
library(shinybusy)
library(tidyverse)


# load trait labeling data
traitNamesVals <- read.csv("traitNamesVals.csv", header = T)
names(traitNamesVals) <- c("TraitDBName", "TraitText", "0", "1", "2", "3", "num", "cols")

# setwd("C:/Users/yo/Dropbox/shiny/TA3loop/TA3loop")



example_input <- matrix(NA, ncol=126, nrow=1)
varnames <- c("ID_catkey", "indDegInput", "recorderInput", "obsDateInput", "noteInput", traitNamesVals$TraitDBName)
colnames(example_input) <- varnames
example_input <- as.data.frame(example_input)

TA3BUM<-readRDS("TA3BUM.Rda") # binary traits
TA3OUM<-readRDS("TA3OUM.Rda") # ordinal traits


######################################################################################
# UI
######################################################################################
ui <- 
  
  fluidPage(
    
    titlePanel("TA3 Age Estimation method: Shiny app replicate + batch upload"),
    
    # sidebarPanel(width = 7,
    
    tabsetPanel(id="inTabset", 
      
      
      tabPanel("About",
               
               br(),
               h4(strong("TA3 Age Estimation method software")),
               
               p("This Shiny app is based on the TA3 Age Estimation software created by Ron Richardson and Stephen Ousley which can be found ",
                 a("here.", href = "https://www.statsmachine.net/software/TA3/", target="_blank"),
               ),
               
               
               p("The original code from  ",
                 a("Ron Richardson's GitHub page ", href = "https://github.com/rer145/ta3", target="_blank"), 
                 "was modified and used in the creation of this Shiny app."),
               
               br(),
               h4(strong("How to use this Shiny app")),
               
               p("As indicated in the TA3", 
                 a("webpage ", href = "https://www.statsmachine.net/software/TA3/", target="_blank"), 
                 "the TA3 Age Estimation method 'uses transition analysis and machine learning to provide 
                     age estimates from skeletal indicators with explicit probabilities and intervals', and 
                   therefore is meant to be used in the context of unidentified human remains."),
               
               p("This Shiny app adds to the original software the possibility to input data as batches of cases for analysis. 
                 It has several tabs, each are described below:"),
               
               tags$ul(
                 tags$li(strong("Individual Case:"), "Enter case information and trait scores for an individual case."),
                 tags$li(strong("Batch Upload:"), "Upload a CSV file with the information of more than one case for batch analysis. Follow the format of the example CSV file provided."),
                 tags$li(strong("Load case/batch:"), "Load the case or batch information and check the information entered is correct."),
                 tags$li(strong("Estimate Age:"), "Select what input data you want to analyze and click on the 'Estimate Age' button and get the age estimations from the Random GLM Analysis. Use the download button to get a CSV file with the results.")
               ),
               
               p(em("Note: input and processing errors have been detected in the original software and are mimicked in this Shiny app to replicate the same results.")),
               
               br(),
               br(),
               
               h3("Dear Coursera Peer-reviewer: If you are not familiar with skeletal traits (which is most probable!) please still try it out by selecting some traits and using the Estimate Age button to check the app works.", 
                  strong("Thank you!")),
               
               p("The code for this app can be found ",
                 a("here.", href = "https://github.com/jgalsku/TA3loop", target="_blank"))
               
      ),
      
      
      
      
      tabPanel("Individual Case", 
               
               
               br(),
               
               tabPanel("Reset Evaluation",           
                        
                        actionButton("reset_input", "Reset selections")
                        
               ),
               
               navlistPanel(widths = c(2,7),
                            
                            
                            
                            tabPanel("Case Info",
                                     

                                     textInput(inputId = "IDInput", label = "Case ID", value = ""),
                                     # numericInput(inputId = "docageInput" , label = "Known age (if available)", value = NA, min = 15, max = 120, step = 1),
                                     textInput(inputId = "indDegInput", label = "Individual Designation", value = ""),
                                     textInput(inputId = "recorderInput", label = "Recorder", value = ""),
                                     textInput(inputId = "obsDateInput", label = "Observation Date", value = ""),
                                     textInput(inputId = "noteInput", label = "Notes", value = "")
                                     
                                     ),
                            
                            
                            tabPanel("Cranium",
                                     
                                     
                                     do.call(splitLayout, lapply(1:2, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     lapply(3:3, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }),
                                     
                                     do.call(splitLayout, lapply(4:5, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }))
                                     
                                     
                                     
                            ),
                            
                            tabPanel("Vertebral Column",
                                     
                                     
                                     lapply(6:8, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }),
                                     
                                     
                                     
                                     do.call(splitLayout, lapply(9:10, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(11:12, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     lapply(17:18, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }),
                                     
                                     do.call(splitLayout, lapply(13:14, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(15:16, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     
                                     
                                     lapply(19:21, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }),
                                     
                                     hr(),
                                     
                                     do.call(splitLayout, lapply(c(22,25), function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(c(23,26), function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(c(24,27), function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(28:30, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }))
                                     
                                     
                                     
                            ),
                            
                            tabPanel("Sternum & Ribs",
                                     
                                     
                                     do.call(splitLayout, lapply(31:32, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     lapply(33:33, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }),
                                     
                                     do.call(splitLayout, lapply(c(34,36), function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(c(35,37), function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     lapply(38:39, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })
                                     
                                     
                            ),
                            
                            tabPanel("Clavicle & Scapula",
                                     
                                     
                                     do.call(splitLayout, lapply(40:41, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(42:43, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(44:45, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(46:47, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(50:51, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }))
                                     
                                     
                            ),
                            
                            tabPanel("Humerus",
                                     
                                     
                                     lapply(52:52, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }),
                                     
                                     do.call(splitLayout, lapply(53:54, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(55:56, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(57:58, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(59:60, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(61:62, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }))
                                     
                                     
                                     
                            ),
                            
                            tabPanel("Radius, Ulna, & Trapezium",
                                     
                                     
                                     do.call(splitLayout, lapply(63:64, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(65:66, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(67:68, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }))
                                     
                                     
                                     
                            ),
                            
                            tabPanel("Femur",
                                     
                                     
                                     do.call(splitLayout, lapply(69:70, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(71:72, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(73:74, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(75:76, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(77:78, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }))
                                     
                                     
                                     
                            ),
                            
                            
                            tabPanel("Tibia, Fibula, Calcaneus",
                                     
                                     
                                     lapply(79:79, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }),
                                     
                                     do.call(splitLayout, lapply(80:81, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     lapply(82:82, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })
                                     
                                     
                                     
                            ),
                            
                            
                            tabPanel("Innominate",
                                     
                                     
                                     lapply(83:83, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }),
                                     
                                     do.call(splitLayout, lapply(84:85, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(86:87, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(88:89, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(90:91, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(92:93, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(94:95, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(96:97, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(98:99, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(100:101, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(102:103, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }))
                                     
                                     
                                     
                                     
                            ),
                            
                            
                            tabPanel("Sacroiliac Joint",
                                     
                                     
                                     do.call(splitLayout, lapply(104:105, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(106:107, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(108:109, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(110:111, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }))
                                     
                                     
                                     
                            ),
                            
                            
                            tabPanel("Pubic Symphysis",
                                     
                                     
                                     do.call(splitLayout, lapply(112:113, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(114:115, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(116:117, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(118:119, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     })),
                                     
                                     do.call(splitLayout, lapply(120:121, function(i) {
                                       radioButtons(inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                                                    choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                                                    selected = character(0), inline = TRUE
                                       )
                                     }))
                                     
                                     
                                     
                            )),
               
               tabPanel("Reset Evaluation",           
                        
                        actionButton("reset_input", "Reset selections")
                        
                        
               )),
      
      tabPanel("Batch upload", 
               
               br(),
               
               fileInput("file1", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               hr(),
               downloadButton("downloadInputFormat", "Download example CSV")
               

      ),
      
      
      tabPanel("Load case/batch", 
               
               br(),
               
               actionButton("loadCase", "Load case data"),
               actionButton("loadBatch", "Load batch file"),
               tableOutput("tableSelections"),
               tableOutput("batchOutput")
                ),
      

      
      tabPanel("Estimate Age",
               br(),
               radioButtons("inputChoice", "Type of input", 
                            choices = c("Individual case", "Batch upload")),
               
               actionButton("calculate", "Estimate age"),
               add_busy_spinner(spin = "fading-circle", position ='bottom-left'),
               tableOutput("allResults"),
               downloadButton("download", "Download results")
               
               
      )))












######################################################################################
# server
######################################################################################


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  

  observeEvent(input$reset_input, {
    
    
    lapply(1:121, function(i) {
      updateRadioButtons(session, inputId = paste0("radioInput", i), label = traitNamesVals[(paste0(i)),2],
                         choiceNames = unname(traitNamesVals[ (paste0(i)) , 3:(traitNamesVals[(paste0(i)),8]) ]),
                         choiceValues = names(traitNamesVals[ , 3:(traitNamesVals[(paste0(i)),8]) ]),
                         selected = character(0), inline = TRUE)
      
      
    })
    
  })
  
  
  
  
  
  
  # LOAD CASE/BATCH - INPUT
  
  data_input <- eventReactive(input$loadCase,{ 
    
    fieldsAll <- c("IDInput", "indDegInput", "recorderInput", "obsDateInput", "noteInput", 
                   lapply(1:121, function(i) {
                     paste0("radioInput", i)
                   })
    )
    
    
    
    list_res <- lapply(fieldsAll, function(x) input[[x]])
    
    list_res[sapply(list_res, is.null)] <- NA
    list_res <- unlist(list_res)
    
    
    data_input <- matrix(NA, ncol=126, nrow=1)
    varnames <- c("ID_catkey", "indDegInput", "recorderInput", "obsDateInput", "noteInput", traitNamesVals$TraitDBName)
    colnames(data_input) <- varnames
    data_input[1,] <- list_res
    data_input <- as.data.frame(data_input)
    
    
  })
  
  
  
  batch_input <- eventReactive(input$loadBatch,{ 

      
      inFile  <- input$file1
      # if (is.null(inFile))
      #   return(NULL)
      
      batch_input <- read.csv(inFile$datapath)
      batch_input <- as.data.frame(batch_input)
      
    })
    

  
  
  
  
  
  # LOAD CASE/BATCH - OUTPUT
  
  
  ## case
  
  data_output <- eventReactive(input$loadCase,{ 
    
    fieldsAll <- c("IDInput", "indDegInput", "recorderInput", "obsDateInput", "noteInput",
                   lapply(1:121, function(i) {
                     paste0("radioInput", i)
                   })
    )
    
    
    
    list_res <- lapply(fieldsAll, function(x) input[[x]])
    
    list_res[sapply(list_res, is.null)] <- NA
    list_res <- unlist(list_res)
    
    
    data_output <- matrix(NA, ncol=2, nrow=126)
    varnames <- c("ID", "Individual designation", "Recorder", "Observed date", "Notes", traitNamesVals$TraitText)
    colnames(data_output) <- c("Variable", "Value")
    data_output[,1] <- varnames
    data_output[,2] <- list_res
    data_output <- as.data.frame(data_output)
    
  })
  
  

  output$tableSelections <- renderTable({  
    
    
    # data_output() %>% select_if(~ !any(is.na(.)))
    
    data_output() %>% drop_na(Value)
    
    
  })
  
  
  
  ## batch
  
  output$batchOutput <- renderTable({  
    
    
    # data_output() %>% select_if(~ !any(is.na(.)))
    
    batch_input() 
    
    
  })
  
  

  
  
  
  # ESTIMATE AGE
  
  
  data_results <- eventReactive(input$calculate,{
    
    
    # calculate age
    
    # ifelse(!is.null(data_input_batch()), 
    #        data_input <- data_input_batch(),
    #        data_input <- data_input()
    #        )
    
    ifelse(input$inputChoice == "Individual case",
           data_input <- data_input(),
           data_input <- batch_input()
           )
    
    
    data_input$cerv_candlewax <- NA #to compensate for TA3 software's error 
    
    

    data_results <- lapply(1:nrow(data_input), function(k)     
      
    {
      
      
    # separate one case/individual
    TA3_Input_wID <- data_input[k,]
    
    
    
    # eliminate doc_age column
    TA3_Input <-  TA3_Input_wID[,-c(1,5)]
    
    TA3_Input <- TA3_Input %>%
      mutate(across(everything(), as.numeric))
    
    
    
    
    
    ################ this code original from TA3 code, parts inactivated and commented ##########
    
    
    
    
    # NEW / MODIFIED BELOW
    # R code version
    #      TA3RCodeVersion <- '0.46.1';
    
    # software program version.  currently or soon 0.70
    #if (development) { TA3ProgramVersion <- '0.71'};
    
    
    # use binary or raw scores?
    UseBinaryScores <- TRUE;
    
    # set some vars to nothing so errors do not get repeated
    PredAge <- 0;
    AnalDat <- 0;
    TA3_Case_Scores <- 0;
    
    
    # # read in reference data files if not already present
    # if (!(exists('TA3BUM'))) {TA3BUM<-readRDS(rda_fileB)}
    # if (!(exists('TA3OUM'))) {TA3OUM<-readRDS(rda_fileO)}
    
    
    TA3_Case_Scores <-  readRDS("TA3_Case_Scores.Rda")
    
    
    
    # convert factors to character format in case scores
    TA3_Case_Scores$TraitDBName <- as.character(TA3_Case_Scores$TraitDBName)
    TA3_Case_Scores$TraitText <- as.character(TA3_Case_Scores$TraitText)
    
    # process trait scores file, has ALL scores including NAs
    # TA3_Input <- read.csv(input_file)
    
    # update values in case file (tall) from scores file (wide)
    for (i in (1:nrow(TA3_Case_Scores) ) ) {
      
      for (j in (1:ncol(TA3_Input) ) ) {
        
        if (TA3_Case_Scores$TraitDBName[i] == names(TA3_Input)[j]) {
          
          TA3_Case_Scores$TraitScore[i]  <- TA3_Input[1,j]
        }
        
      }
      
    }
    
    # Remove NAs in case tabular data (but there may be no NAs, so cant just use !which)
    TA3_Case_Scores <- TA3_Case_Scores[which(complete.cases(TA3_Case_Scores$TraitScore)),]
    
    # copy case scores to analysis data
    AnalCase <- data.frame('parietal_depression' = 0); # create data frame();
    
    for (i in (1:nrow(TA3_Case_Scores) ) ) {
      
      AnalCase[i] <- TA3_Case_Scores[i,'TraitScore']
      #names(AnalCase)[i] <- as.character(TA3_Case_Scores[i,'TraitDBName']);
      names(AnalCase)[i] <- TA3_Case_Scores[i,'TraitDBName'];
      #  NOTE: as.character() added because it failed after running successfully for many time. Why?
      #  BECAUSE they are factors now, need to convert.
      
    }
    
    ### convert left and right Case values into unilateral
    # get L and R fields
    BiTraitList <- NULL
    
    # AnalCase already no NAs (blanks)
    for (i in (1:ncol(AnalCase))) {
      
      lside <- paste(substr(names(AnalCase[i]), 1, nchar(names(AnalCase[i]))-1 ), 'L', sep = '');
      rside <- paste(substr(names(AnalCase[i]), 1, nchar(names(AnalCase[i]))-1 ), 'R', sep = '');
      
      # OR     take care of DISH for now
      if  ( (lside %in% names(AnalCase)    ||  rside %in% names(AnalCase))
            # take care of DISH and max lengths (XL) for now
            & !(regexpr('DISH', lside) > 0) & !(regexpr('XL', lside) > 0)  ) {
        
        BiTraitList <- append(BiTraitList, names(AnalCase[i]) );   #paste(substr(lside,1, nchar(lside)-1), sep = '') )
        
      }
    }
    
    # check if there are indeed bilateral traits scored
    if (length(BiTraitList) > 0) {
      # sweep through all Case fields, assign Left to neutral trait if not null;
      for (i in (1:length(BiTraitList))) {
        
        currfld <- BiTraitList[i];
        biside <- substr(currfld, 1, nchar(currfld)-1);
        
        AnalCase[biside] <- AnalCase[currfld];
        
        # set up lookuplist for later better formatting
        # TraitNames <-   append TraitNames
        
      }  #for
      
      # remove left or right fields
      AnalCase <- AnalCase[,-which(names(AnalCase) %in% BiTraitList)]
      
    }  # if bitraits there, not present id midline
    
    
    
    ##########################################################
    ### Convert raw data into binary-only values
    AnalCaseB <- AnalCase;
    
    # Go through all column names in AnalCase (non-blank ones would be better)
    # see which ones have ordinal values in reference data
    
    # set up list for converted columns
    OrdinalCols <- ''
    i <- 1;
    
    for (col in 1:ncol(AnalCaseB))
    {
      colname <- names(AnalCaseB)[col]
      collen <- length(table(TA3OUM[colname]))
      
      if (collen > 2)
      {
        # print(colname);
        OrdinalCols[i] <- colname;
        i <- i + 1;
        if (collen == 3)
        {
          AnalCaseB[paste(colname, '_0_12', sep = '')] <- NA
          
          AnalCaseB[paste(colname, '_01_2', sep = '')] <- NA
          
          
          AnalCaseB[which(AnalCaseB[colname] == 0), paste(colname, '_0_12', sep = '')] <-  0
          AnalCaseB[which(AnalCaseB[colname] == 0), paste(colname, '_01_2', sep = '')] <-  0
          
          AnalCaseB[which(AnalCaseB[colname] == 1), paste(colname, '_0_12', sep = '')] <-  0
          AnalCaseB[which(AnalCaseB[colname] == 1), paste(colname, '_01_2', sep = '')] <-  1
          
          AnalCaseB[which(AnalCaseB[colname] == 2), paste(colname, '_0_12', sep = '')] <-  1
          AnalCaseB[which(AnalCaseB[colname] == 2), paste(colname, '_01_2', sep = '')] <-  1
          
        } else
        {
          if (collen == 4)
          {
            AnalCaseB[paste(colname, '_0_123', sep = '')] <- NA
            
            AnalCaseB[paste(colname, '_01_23', sep = '')] <- NA
            
            AnalCaseB[paste(colname, '_012_3', sep = '')] <- NA
            
            
            AnalCaseB[which(AnalCaseB[colname] == 0), paste(colname, '_0_123', sep = '')] <- 0
            AnalCaseB[which(AnalCaseB[colname] == 0), paste(colname, '_01_23', sep = '')] <- 0
            AnalCaseB[which(AnalCaseB[colname] == 0), paste(colname, '_012_3', sep = '')] <- 0
            
            AnalCaseB[which(AnalCaseB[colname] == 1), paste(colname, '_0_123', sep = '')] <-  1
            AnalCaseB[which(AnalCaseB[colname] == 1), paste(colname, '_01_23', sep = '')] <-  0
            AnalCaseB[which(AnalCaseB[colname] == 1), paste(colname, '_012_3', sep = '')] <-  0
            
            AnalCaseB[which(AnalCaseB[colname] == 2), paste(colname, '_0_123', sep = '')] <-   1
            AnalCaseB[which(AnalCaseB[colname] == 2), paste(colname, '_01_23', sep = '')] <-   1
            AnalCaseB[which(AnalCaseB[colname] == 2), paste(colname, '_012_3', sep = '')] <-   0
            
            AnalCaseB[which(AnalCaseB[colname] == 3), paste(colname, '_0_123', sep = '')] <-  1
            AnalCaseB[which(AnalCaseB[colname] == 3), paste(colname, '_01_23', sep = '')] <-  1
            AnalCaseB[which(AnalCaseB[colname] == 3), paste(colname, '_012_3', sep = '')] <-  1
            
            #table(AnalCaseB[c(colname, paste(colname,'_0_123', sep= ''), paste(colname,'_01_23', sep= ''), paste(colname,'_012_3', sep= ''))])
            #table(AnalCaseB[colname])
            
            
          } # if
          
        } #else
        
      } #if collen > 2
      
    } # for each col
    
    
    # remove non-binary fields from newly binarized case table, remove fields by name
    if (OrdinalCols[1] > '') {
      AnalCaseB <- AnalCaseB[,-which(names(AnalCaseB) %in% OrdinalCols)]
    }
    
    
    # AnalCase and AnalCaseB are set up;
    # SEE NOTE 4 in TA3_Application Notes-RCran-Electron-Windows.txt
    
    
    ####### Extract Data; choice is for binary or ordinal, still uncertain in many cases  ###########################
    #### Extract binary data for now
    # get field names not blank (no NAs)
    
    if (UseBinaryScores) # binary
    {
      NBF <- names(AnalCaseB)
      #read from binary file
      AnalDat <- na.omit(TA3BUM[c('RandID','age',NBF)])
      
      
    } else   # ordinal
    {
      NBF <- names(AnalCase)
      #read from ordinal file
      AnalDat <- na.omit(TA3OUM[c('RandID','age',NBF)])
      
      
    }
    
    
    # pre-process data
    # separate RandIDs
    RandIDs <- AnalDat$RandID
    # remove column from analyzed data
    AnalDat <- AnalDat[,-which(colnames(AnalDat) == 'RandID')]
    
    ## GLM needs numerical values anyway- so convert all
    
    # separate ages?
    ages <- AnalDat["age"]
    # remove ages column from analyzed data?
    #AnalDat <- AnalDat[,-which(colnames(AnalDat) == 'age')]
    
    
    # y must be numeric (not integer)
    agesn <- lapply(ages, as.numeric)
    
    # strip name from vector
    agesnu <- unlist(agesn, use.names = F)
    
    
    # the predictors need to be numeric
    #AnalDat <- lapply(AnalDat, as.numeric)
    
    # need to convert back into a dataframe:
    AnalDat <- as.data.frame(AnalDat);
    
    #str(AnalDat)
    #'data.frame':	199 obs. of  89 variables:
    cat(nrow(AnalDat), 'records in reference data.');
    
    ages <- AnalDat["age"]
    AnalDat <- AnalDat[,-which(colnames(AnalDat) == 'age')]
    
    agesn <- lapply(ages, as.numeric)
    
    agesnu <- unlist(agesn, use.names = F)
    
    AnalDat <- lapply(AnalDat, as.numeric)
    
    AnalDat <- as.data.frame(AnalDat)
    
    ####################### Preprocessing Done ###################
    
    ####### Start of R Statistical Analysis (example: randomGLM) ############
    
    #require(randomGLM)
    #require(msir)
    #require(glmnet)
    # parallel
    #nThr <- detectCores()
    
    
    # TODO: can we show progress bar and estimate time remaining?
    # nFeaturesinBag
    # nBags usually 100, but 20 or 30 should be just fine
    
    out <- capture.output(RGLM <- randomGLM(AnalDat, agesnu, classify = F, nBags = 100, nThreads=1)) #  , keepModels = T) (no need to keep models)
    #RGLM <- randomGLM(AnalDat, agesnu, classify = F, nBags = 100, keepModels = T, maxInteractionOrder = 2, nFeaturesInBag = 100)
    
    #, nThreads=nThr,
    #replace = TRUE,
    #sampleWeight=NULL,
    # nObsInBag = if (replace) nrow(AnalDat) else as.integer(0.632 * nrow(AnalDat)),
    #nFeaturesInBag = ceiling(ifelse(ncol(AnalDat)<=10, ncol(AnalDat),
    #ifelse(ncol(AnalDat)<=300, (1.0276-0.00276*ncol(AnalDat))*ncol(AnalDat), ncol(AnalDat)/5))),
    #minInBagObs = min( max( nrow(AnalDat)/2, 5), 2*nrow(AnalDat)/3))
    
    
    #GLMN <- glmnet(AnalDat, agesnu, classify = F, nBags = 100, minInBagObs = 20)
    
    # Calculates and plots a 1.96 * SD prediction band, that is,
    # a 95% prediction band
    DFP <- cbind(RGLM$predictedOOB, agesnu)
    DFP <- as.data.frame(DFP)
    names(DFP) <- c('PredAge','Age')
    
    # larger span now makes MUCH better (default = 0.67, Weisberg (2005):112 )
    lol <- loess.sd(DFP, nsigma = 1.96, span = 1)
    
    # predicted age
    # if (UseBinaryScores)
    # { 
    PredAge <- predict(RGLM,AnalCaseB, type='response')
    
    # }  else
    # { PredAge <- predict(RGLM,AnalCase, type='response')
    # }
    PredAge
    
    
    # produce basic output: MSE
    MeanStdError <- sqrt(mean((DFP$PredAge - DFP$Age)^2))
    
    
    # get prediction intervals for this individual
    ADindex <- which.min(abs(lol$x  - PredAge))
    LB <- lol$lower[ADindex]
    UB <- lol$upper[ADindex]
    
    
    # # Save plot to file (enhanced plot)  IF APP RUNNING
    # if (!development) { png(filename=output_image1, width = 1400, height = 1200, res = 300, pointsize = 7)   }
    # 
    # # plot OOB estimated age and 95% CI for OOB individuals
    # plot(DFP$PredAge, DFP$Age, ylim = c(15,110), xlim = c(15,110), , pch = 17, cex = 0.7, col = 'blue',
    #      xlab = 'Predicted Age', ylab = 'Age', main = 'TA3 Age Estimation (Random GLM) 95% OOB PI')
    # 
    # lines(lol$x, lol$y, lw = 3, lty=3, col= 'purple')
    # lines(lol$x, lol$upper, lty=2, lw = 1, col= 'purple')
    # lines(lol$x, lol$lower, lty=2, lw = 1, col= 'purple')
    # # line of perfect agreement
    # abline(0,1, lw = 1)
    # 
    # if (!development) {  dev.off()  }
    # 
    
    #print(paste('The estimated age at death is', round(PredAge), 'years and the Standard Error is', round(MeanStdError,1),'\n', 'using a sample size of',nrow(AnalDat)))
    
    
    ############# End of TA3 Analysis #############
    
    
    
    
    ############# Save output to Application #############
    
    
    # # if (UseBinaryScores) {scorestr <- 'Using binarized ordinal scores'} else {'Using binary and nominal scores'};
    # scorestr <- 'Trait: score'
    # 
    # # write results to a file for reading
    # progVersion<-paste('  Program Version ', TA3ProgramVersion, sep='')
    # codeVersion<-paste('  R Code Version ', TA3RCodeVersion, sep='')
    # if (UseBinaryScores) {scorebasis<-'Using binarized ordinal scores.'} else {scorebasis<-'Using raw scores.'};
    # bumVersion<-paste('  TA3BUM Version ', TA3BUMVersion, sep='')
    # oumVersion<-paste('  TA3OUM Version ', TA3OUMVersion, sep='')
    # write(
    #         paste(
    #                 '---------------------------------------------',
    #                 'TA3 Age Estimation',
    #                 progVersion,
    #                 codeVersion,
    #                 scorebasis,
    #                 bumVersion,
    #                 oumVersion,
    #                 '---------------------------------------------',
    #                 ' ',
    #                 sep='\n'
    #         ),
    #         file=output_file,
    #         append=FALSE,
    #         sep=''
    # )
    # 
    # 
    # write(paste(
    #         scorestr,
    #         '---------------------------------',
    #         sep='\n'),
    #       file=output_file,
    #       append=TRUE,
    #       sep=''
    # )
    # 
    # 
    # for (i in c(1:nrow(TA3_Case_Scores))) {
    #         write(paste(TA3_Case_Scores$TraitText[i], ': ',TA3_Case_Scores$TraitScore[i],  sep=''), file=output_file, append=TRUE, sep='\n')
    # }
    # 
    # write(
    #         paste(
    #                 ' ',
    # paste('Sample size = ', round(nrow(AnalDat)), sep=''),
    # ' ',
    # 'Random GLM Analysis',
    # paste('  Estimated age at death = ', round(PredAge,1), ' years', sep=''),
    # paste('  Estimated lower 95% bound = ', round(LB,1), ' years', sep=''),
    # paste('  Estimated upper 95% bound = ', round(UB,1), ' years', sep=''),
    # paste('  '),
    # paste('  Standard Error = ', round(MeanStdError,1), sep=''),
    # paste('  Corr(Age and Pred Age) = ', round(cor(DFP$Age,DFP$PredAge),3), sep=''),
    # sep='\n'
    #         ),
    #         file=output_file,
    #         append=TRUE,
    #         sep=''
    # )
    # 
    # if (development) {  TA3_Case_Scores  }
    # 
    # if (development) {  read.delim(file.path(temp_dir, 'output.txt'))  }
    
    
    
    #JG save results in list
    
    data_results <- list()
    
    data_results[[k]] <- c(TA3_Input_wID$ID_catkey,
                           paste(nrow(AnalDat), 'records in reference data.'),
                           # ncol(AnalCaseB),
                           paste(round(PredAge,1), "years"),
                           paste(round(unname(LB),1), "years"),
                           paste(round(unname(UB),1), "years"),
                           round(MeanStdError,2),
                           round(cor(DFP$Age,DFP$PredAge),3))
      
      

    
    
    
    
    })
    

})
  
  
  
  
  
  

  results_bind <- reactive({


    
    # load results list
    data_results <- data_results()
    len <- length(data_results)

    # add results to matrix
    results_bind <- data.frame(matrix(unlist(data_results), nrow=len, byrow=TRUE),stringsAsFactors=FALSE)
    results_bind <- as.data.frame(results_bind)
    
    varnames <- c(  "ID", "RefSample", "PredAge", "LB", "UB", "MeanStdError", "Corr(PredAge,Age)")
    colnames(results_bind) <- varnames
    as.data.frame(results_bind)

  })


  
  observeEvent(input$calculate,{
    
    
    
    output$allResults <- renderTable({
      

      
      
      results_bind <- results_bind()
      

      
    })
    
  })
  
  
  
  
  
  
  
  
  output$download <- downloadHandler(
    filename = function() {
      "TA3shinyResults.csv"
    },
    content = function(con) {
      
      
      results_bind <- results_bind()
      
      # data_plus_results <- t(data_plus_results)
      
      
      write.csv(results_bind, con, fileEncoding = "latin1", row.names=FALSE)
    }
  )
  
  
  
  output$downloadInputFormat <- downloadHandler(
    filename = function() {
      "inputFormat.csv"
    },
    content = function(con) {
      
      
      example_input 
      
      # data_plus_results <- t(data_plus_results)
      
      
      write.csv(example_input, con, fileEncoding = "latin1", row.names=FALSE)
    }
  )
  
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
