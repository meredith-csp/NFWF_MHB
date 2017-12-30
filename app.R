#R Shiny app for Zonation-based prioritization of conservation actions in Missouri Headwaters Basin

library(shiny)
library(raster)
library(rgdal)
library(leaflet)
library(rsconnect)

#Define input data and fixed parameters globally here:
targ.summ<-read.csv("targets.csv")
mhb <- shapefile("MissouriHeadwaters_HUC6/MissouriHeadwaters_HUC6.shp")
huc12 <- shapefile("MissouriHeadwaters_HUC12/MissouriHeadwaters_HUC12.shp")

ui<-fluidPage(titlePanel("Missouri Headwaters Basin Conservation Prioritization Tool"),
              sidebarLayout(
                #User inputs: select targets, run parameters
                sidebarPanel(
                  wellPanel(h5("Give your project a unique name (no spaces):"),
                              textInput("rname","",value="")),   #PUT DELAY (SUBMIT BUTTON) ON THIS (or submit all inputs as group)
                  wellPanel(h5("Select conservation targets of interest:"),
                              checkboxGroupInput("targ.sel","",choiceNames=as.list(as.character(targ.summ$name)),choiceValues=as.list(as.character(targ.summ$abbv)))),
                  wellPanel(h5("Set weights for each target:"),
                              uiOutput("wt.sel")),
                  wellPanel(h5("Set run parameters:"),
                            radioButtons("rem.rule","Choose removal rule",c("Basic Core-Area Zonation"="bca","Additive Benefit Function"="abf","Target-Based Planning"="tbp","Generalized Benefit Function"="gbf","Random Removal"="rr"),selected="abf"),
                            sliderInput("warp","High Quality <---------------> High Speed",min=50.0,max=2000.0,value=100,step=10.0,round=FALSE,ticks=FALSE,width='80%')
                            ),
                  wellPanel(actionButton(inputId="go",label="Run Prioritization"),
                            textOutput("status"))),
                #Output: map, summary plots, etc.
                mainPanel(
                  wellPanel(tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                            leafletOutput('map')),
                wellPanel(#h5("Selected run parameters:"),
                  textOutput("targ.nm"),
                  dataTableOutput("targ.wts"),
                  textOutput(outputId="radio"),
                  textOutput(outputId="warp")))
))

server<-function(input, output) {
  #Render outputs
    output$wt.sel <- renderUI({
      numWts <-length(input$targ.sel)
      lapply(1:numWts, function(i) {
        if(length(input$targ.sel)==0) {output$wt.sel0 <- renderText({" "})} else {
        sliderInput(inputId=paste0("targ",i),label=targ.summ[targ.summ$abbv==input$targ.sel[i],"name"],min=0,max=10,value=1,step=1)}
        })
      })
    output$map <- renderLeaflet({
      leaflet(mhb) %>%
        addProviderTiles("Esri.WorldTopoMap", group="Topographical") %>%
        addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
        addPolygons(data=huc12, fill = FALSE, color = "#a9a9a9", weight = 1, opacity = 0.8) %>%
        addPolygons(fill = FALSE, color = "#8b3a3a", weight = 4, opacity = 0.8) %>%
        #addLegend("bottomright", pal = pal, values = ~y, title = "Temp C" , opacity = 0.9) %>%
        addLegend("bottomright", colors = c("#8b3a3a", "#a9a9a9"), opacity = 0.8, labels = c("Missouri Headwaters Basin", "HUC12 units")) %>%
        addLayersControl(baseGroup = c("Topographical", "Satellite")) %>%
        setView(lng = -112.219, lat = 45.324,  zoom = 8)  
    })
    spp<-eventReactive(input$go, {
      #Create required .spp file
      cbind(input$wt.sel,matrix(-1,nrow=length(input$targ.sel),ncol=3),rep(0.25,length(input$targ.sel)),as.character(targ.summ[which(targ.summ$abbv %in% input$targ.sel),"file"]))
      #wts<-input$wt.sel
      #files<-as.character(targ.summ[which(targ.summ$abbv %in% input$targ.sel),"file"])
      #prj.spp<-cbind(wts,matrix(-1,nrow=length(files),ncol=3),rep(0.25,length(files)),files)
      #row.names(prj.spp)<-NULL; colnames(prj.spp)<-NULL
      #write(prj.spp, file=paste0(input$rname,".spp"))  #"/home/user/",
      })
    row.names(spp)<-NULL; colnames(spp)<-NULL
    write(spp, file=paste0(input$rname,".spp"))
    output$status<-renderText(paste("Spp file created:",input$rname))
    #output$targ.wts<-renderDataTable({targ.summ[targ.summ$abbv==input$targ.sel,c("name","weight")]})
}

shinyApp(ui=ui, server=server)