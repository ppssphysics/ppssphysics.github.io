library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(stringr)

rawdata <- read.csv("Data/GS_AS2016_DataSet.csv",header=TRUE,na.strings="NA",quote="",sep=";")
#rawdata <- data.frame(lapply(rawdata, function(x) {gsub("Ethnicity (I am Tutsi / I am Indigenous)", "Ethnicity", x)}))
#rawdata <- data.frame(lapply(rawdata, function(x) {gsub("Global (I am a citizen of the world)", "Global", x)}))
#rawdata <- lapply(rawdata, function(x) {gsub("Nationality (I am Indian / I am Argentinian)", "Nationality", x)})
#rawdata <- lapply(rawdata, function(x) {gsub("Philosophical Beliefs (I am Utopian / I am Communist)", "Philisophical Beliefs", x)})
#rawdata <- lapply(rawdata, function(x) {gsub("Religion (I am Muslim / I am Christian)", "Religion", x)})
#rawdata <- lapply(rawdata, function(x) {gsub("Region (I am European / I am Asian)", "Region", x)})

shinyServer(function(input, output) {
    
   set.seed(1234)
   
   ################################################################
   # Sub-setting the Data Set
   ################################################################
   
   datasetInput <- reactive({
      
      if (input$RegionSel=='All'){
          rawdata
        } 
      else {
          subset(rawdata,Region_UN==input$RegionSel,select=Finished:Identitiy)
        } 
   })
   
   ################################################################
   # Progress Boxes
   ################################################################
   
   output$progressBox1 <- renderInfoBox({
      infoBox("Participation", paste0(nrow(rawdata), " completed surveys"), icon = icon("list"),color = "purple")
   })
   
   output$progressBox2 <- renderInfoBox({
      infoBox("Represented Countries", paste0(length(unique(rawdata$Country)), " countries & territories"), icon = icon("list"),color = "purple")
   })
   
   ################################################################
   # Graphics on Participation Page
   ################################################################
   
   # Completed surveys
   ###############################
   
   output$surveys = renderPlot({
      
       newdata <- datasetInput()
      
       if (input$Normalisation1=='Absolute') {
           ggplot(newdata,aes(factor(newdata$Finished)))+
               geom_bar(width=.3,fill="steelblue",colour="steelblue")+
               labs(title="",x="",y="")+
               scale_x_discrete(labels=c("Partial","Complete"))
       } else {
           ggplot(newdata,aes(factor(newdata$Finished)))+
               geom_bar(aes(y=(..count..)/sum(..count..)),width=.5,fill="darkgreen",colour="darkgreen")+
               scale_y_continuous(labels=percent)+
               labs(title="",x="",y="")+
               scale_x_discrete(labels=c("Partial","Complete"))
       }
   })
   
   # Gender distribution
   ###############################
   
   output$genderplot = renderPlot({
       
      newdata <- datasetInput()
      
      if (input$Normalisation2=='Absolute') {
          ggplot(newdata,aes(factor(newdata$Gender),fill=factor(newdata$Gender)))+
              geom_bar(width=.5,fill="steelblue",colour="steelblue")+
              labs(title="",x="",y="")+ theme(legend.position="none")
      } else{
          ggplot(newdata,aes(factor(newdata$Gender),fill=factor(newdata$Gender)))+
              geom_bar(aes(y=(..count..)/sum(..count..)),width=.5,fill="darkgreen",colour="darkgreen")+
              scale_y_continuous(labels=percent)+
              labs(title="",x="",y="")+ theme(legend.position="none")
      }
          
      
   })
   
   # Regional Distribution
   ###############################
   
   output$regionalplot = renderPlot({
      
       newdata <- datasetInput()
       newdata <- subset(newdata,Region_UN!='#N/A',select=Finished:Identitiy)
      
      if (input$Normalisation3=='Absolute') {
        ggplot(newdata,aes(factor(newdata$Region_UN)))+
            geom_bar(width=.5,fill="steelblue",colour="steelblue")+
            labs(title="",x="",y="")
      } else {
          ggplot(newdata,aes(factor(newdata$Region_UN)))+
              geom_bar(aes(y=(..count..)/sum(..count..)),width=.5,fill="darkgreen",colour="darkgreen")+
              scale_y_continuous(labels=percent)+
              labs(title="",x="",y="")
      }
   })
   
   # Sub-regional distribution
   ################################
   
   #output$subregionalplot = renderPlot({
   #   newdata <- datasetInput()
      
   #   ggplot(newdata,aes(factor(newdata$Region_Sub_UN)))+
   #      geom_bar(aes(y=(..count..)/sum(..count..)),width=.5,fill="steelblue",colour="steelblue")+
   #      scale_y_continuous(labels=percent)+
   #      labs(title="",x="",y="")+
   #      coord_flip()
   #})
   
   ################################################################
   # Graphics on Insights Page
   ################################################################
   
   datasetInput2 <- reactive({
       
       if (input$RegionSel2=='All'){
           rawdata
       } 
       else {
           subset(rawdata,Region_UN==input$RegionSel2,select=Finished:Identitiy)
       } 
   })
   
   # QPlot1
   ################################
   
   output$Qplot1 = renderPlot({
      
       if (input$QuestionSel=='The world is...'){
           
           insdata <- subset(datasetInput2(),WorldIs!='',select=Finished:Identitiy)
           
           ggplot(insdata,aes(factor(insdata$WorldIs)))+
               geom_bar(aes(y=(..count..)/sum(..count..)),width=.5,fill="steelblue",colour="steelblue")+
               scale_y_continuous(labels=percent)+
               labs(title="Would you say the world is...",x="",y="")+
               scale_x_discrete(labels=c("Full of Opportunities","Full of Struggles"))
       } else {
           
           insdata <- subset(datasetInput2(),Identitiy!='',select=Finished:Identitiy)
           
           ggplot(insdata,aes(factor(insdata$Identitiy)))+
               geom_bar(aes(y=(..count..)/sum(..count..)),width=.5,fill="steelblue",colour="steelblue")+
               scale_y_continuous(labels=percent)+
               labs(title="As far as my identity is concerned, what defines me the most is...",x="",y="")+
               coord_flip()+
               scale_x_discrete(labels=c("Ethnicity","Global","Nationality","None","Other","Religion","Philisophical Belief","Region"))
       }
   })
   
   # QPlot2
   ################################
   
   output$Qplot2 = renderPlot({
       
       insdata <- subset(datasetInput2(),Gender=='Male' | Gender=='Female',select=Finished:Identitiy)

       if (input$QuestionSel=='The world is...'){
          
           insdata <- subset(insdata,WorldIs!='',select=Finished:Identitiy)
           
            ggplot(insdata, aes(Gender, fill=WorldIs)) + 
                geom_bar(position="dodge",aes(y=(..count..)/sum(..count..)))+
                scale_y_continuous(labels=percent)+
                labs(title="Would you say the world is...",x="",y="")
           
       } else {
           
           insdata <- subset(insdata,Identitiy!='',select=Finished:Identitiy)
           
           ggplot(insdata, aes(Gender, fill=Identitiy)) + 
               geom_bar(position="dodge",aes(y=(..count..)/sum(..count..)))+
               scale_y_continuous(labels=percent)+
               labs(title="As far as my identity is concerned, what defines me the most is...",x="",y="")+
               theme(legend.title=element_blank())
               #scale_fill_discrete(breaks=c("Ethnicity","Global","Nationality","None","Other","Religion","Philisophical Belief","Region"))
               #scale_x_discrete(labels=c("Religion","Region","Philisophical Belief","Other","None","Nationality","Global","Ethnicity"))
       }
    })
   
   # QPlot3
   ################################
   
   output$Qplot3 = renderPlot({
       
       insdata <- subset(datasetInput2(),Region_UN!='#N/A',select=Finished:Identitiy)
       
       if (input$QuestionSel=='The world is...'){
           
           insdata <- subset(insdata,WorldIs!='',select=Finished:Identitiy)
           
           ggplot(insdata, aes(Region_UN, fill=WorldIs)) + 
               geom_bar(position="dodge",aes(y=(..count..)/sum(..count..)))+
               scale_y_continuous(labels=percent)+
               labs(title="Would you say the world is...",x="",y="")
           
       } else {
           
           insdata <- subset(insdata,Identitiy!='',select=Finished:Identitiy)
           
           ggplot(insdata, aes(Region_UN, fill=Identitiy)) + 
               geom_bar(position="dodge",aes(y=(..count..)/sum(..count..)))+
               scale_y_continuous(labels=percent)+
               labs(title="As far as my identity is concerned, what defines me the most is...",x="",y="")
               #coord_flip()
               #scale_x_discrete(labels=c("Religion","Region","Philisophical Belief","Other","None","Nationality","Global","Ethnicity"))
       }
   })   
}
)