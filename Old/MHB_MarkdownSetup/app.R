#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(raster)
library(rgdal)
library(leaflet)
library(zonator)
library(zip)

targ.summ<-read.csv("data/actiontargets.csv",header=T)
act.lyrs<-read.csv("data/actionlayers.csv",header=T)

ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    wellPanel(
      textInput("run.name","Give your Zonation run a name:","e.g., 'MyAction_Run1'"),
      
      radioButtons("action.sel", "Select Action of Interest:",
                   c("Conservation easement" = "option1",
                     "Retrofit road culverts/bridges" = "option2",
                     "Road decommissioning" = "option3",
                     "Grazing management" = "option4",
                     "Adjust irrigation practices" = "option5",
                     "Soil health management" = "option6",
                     "Stream/riparian restoration" = "option7",
                     "Thinning/burning" = "option8"
                    )
      )
    ),
    
    wellPanel(
      h5("Save your run settings for use in Zonation:"),
      downloadButton("downloadData", "Download settings file")
      ),
    
    wellPanel(
      textOutput("instruc")
    )
  ),
  
  mainPanel(
    wellPanel(
      h5("Set weights for each target:"),
      uiOutput("wt.sel")
    ),
    
    wellPanel(h5("Choose removal rule:"),
              radioButtons("rem.rule","",c("Basic Core-Area Zonation"="1","Additive Benefit Function"="2","Target-Based Planning"="3","Generalized Benefit Function"="4","Random Removal"="5"),selected="1")
    )
  )
))

server <- function(input, output) {
  output$wt.sel <- renderUI({
    targets<-targ.summ[targ.summ[,input$action.sel]==1,"name"]
    lapply(1:length(targets), function(i) {
      if(length(input$action.sel)==0) {output$wt.sel0 <- renderText({" "})} else {
        sliderInput(inputId=paste0("targ",i),label=targets[i],min=0,max=1,value=0,step=0.1)}
    })
  })
  
  observeEvent(input$save.inputs,{ 
    input_raster_dir<-"data/01_biodiversity_features/02_habitats/forest"
    inputlist<-isolate(reactiveValuesToList(input))
    outdir <- tempdir()
    
    zproject <- function() {
      create_zproject(name=inputlist$run.name,dir=outdir,variants=inputlist$run.name,
                      spp_template_dir=input_raster_dir,spp_file_pattern="*.tif",overwrite=T,
                      weight=c(inputlist$targ1,inputlist$targ2,inputlist$targ3,inputlist$targ4))

    dat<-list("Settings" = list("removal rule"=inputlist$rem.rule, "warp factor"=50, "edge removal"=1, 
                                "use planning unit layer"=0, "use cost"=0, 
                                "use mask"=1, "mask file"=act.lyrs[act.lyrs[,'label']==inputlist$action.sel,"masklyr"],
                                "use tree connectivity"=0))
    write_dat(dat,paste0(outdir,"/",inputlist$run.name,"/",inputlist$run.name,"/",inputlist$run.name,".dat"),overwrite=T)}
    
    output$downloadData <- downloadHandler(     #currently just relaunches app in new window? wtf? decompose simpler case...
      filename = function() {
        paste(inputlist$run.name, "zip", sep=".")
      },
      content = function(file) {
        tmpdir <- tempdir()
        setwd(tmpdir)
        zproject()
        zip(zipfile=file, files=list.files)
      },
      contentType = "application/zip"
    )
    outputOptions(output, 'downloadData', suspendWhenHidden=FALSE)
    
    output$instruc<-renderText({paste0("Your run settings have been saved to ",outdir,"/",inputlist$run.name,"/",inputlist$run.name,".bat.  
                                       Next, open the Zonation tool, load your file, and hit 'Run'.")})
  })
}

shinyApp(ui = ui, server = server)

