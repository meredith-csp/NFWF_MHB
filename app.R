#R Shiny app for Zonation-based prioritization of conservation actions in Missouri Headwaters Basin

library(shiny)
library(rsconnect)
#rsconnect::deployApp('/Users/meredith-csp/repos/NFWF_MHB')

#Define input data and fixed parameters here:
targ.summ<-read.csv("targets.csv")

ui<-fluidPage(titlePanel("Missouri Headwaters Basin Conservation Prioritization Tool"),
              sidebarLayout(
                #User inputs: select targets, run parameters
                sidebarPanel(
                  wellPanel(h5("Give your project a unique name (no spaces):"),
                              textInput("rname","",value="")),
                  wellPanel(h5("Select conservation targets of interest:"),
                              checkboxGroupInput("targ.sel","",choiceNames=as.list(as.character(targ.summ$name)),choiceValues=as.list(as.character(targ.summ$abbv)))),
                  wellPanel(h5("Set weights for each target:"),
                              #sliderInput("rh.wt", "Riparian Health",min=0.0,max=10.0,value="1.0",step=1.0,round=FALSE),
                              #sliderInput("rc.wt", "Riparian Configuration",min=0.0,max=10.0,value="1.0",step=1.0,round=FALSE),
                              #sliderInput("uv.wt", "Upland Vegetation",min=0.0,max=10.0,value="1.0",step=1.0,round=FALSE),
                              #sliderInput("whc.wt", "Within Habitat Connectivity",min=0.0,max=10.0,value="1.0",step=1.0,round=FALSE),
                              #sliderInput("nfr.wt", "Normative Flow Regime",min=0.0,max=10.0,value="1.0",step=1.0,round=FALSE)),
                              textOutput("wt.sel0"),
                              uiOutput("wt.sel")),
                  wellPanel(h5("Set run parameters:"),
                            radioButtons("rem.rule","Choose removal rule",c("Basic Core-Area Zonation"="bca","Additive Benefit Function"="abf","Target-Based Planning"="tbp","Generalized Benefit Function"="gbf","Random Removal"="rr"),selected="abf"),
                            sliderInput("warp","High Quality <---------------> High Speed",min=50.0,max=2000.0,value=100,step=10.0,round=FALSE,ticks=FALSE,width='80%')
                            )),
                #Output: eventually mapped priorities, for now show selected targets, params
                mainPanel(
                  wellPanel(#h5("Selected run parameters:"),
                            textOutput("targ.nm"),
                            dataTableOutput("targ.wts"),
                            textOutput(outputId="radio"),
                            textOutput(outputId="warp")),
                  wellPanel(h3(tags$i("Output (map, summary plots) will go here"))))
))

server<-function(input, output) {
  #Render outputs
    #output$targ.nm <- renderText({targ.summ[targ.summ$abbv==input$targ.sel,"name"]})
    output$wt.sel <- renderUI({
      numWts <-length(input$targ.sel)
      lapply(1:numWts, function(i) {
        if(length(input$targ.sel)==0) {output$wt.sel0 <- renderText({" "})} else {
        sliderInput(inputId=paste0("targ",i),label=targ.summ[targ.summ$abbv==input$targ.sel[i],"name"],min=0,max=10,value=1,step=1)}
        })
      })
    #output$targ.wts<-renderDataTable({targ.summ[targ.summ$abbv==input$targ.sel,c("name","weight")]})
    #output$radio<-
    #output$warp<-
}

shinyApp(ui=ui, server=server)