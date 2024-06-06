#Load packages and prepare data
library(plotly)
library(plyr)
library(shiny)
library(shinydashboard)
library(rgdal)
library(dplyr)
library(rgeos)
library(maptools)
library(leaflet)
library(leaflet.extras2)
library(lubridate)
library(sf)
library(crosstalk)
library(RColorBrewer)
library(shinyWidgets)
library(leafem)
library(tidyverse)
library(htmltools)
library(shinythemes)
library(stringr)
library(sp)
library(gapminder)
library(shinyalert)
library(magrittr)
library(ggmap)
library(broom)
library(readxl)
library(readr)

crime<-read_excel(path="Data_Tables_LGA_Criminal_Incidents_Year_Ending_December_2022.xlsx",sheet=3)
rent<-read_excel(path="Quarterly median rents by local government area - March quarter 2023.xlsx",sheet=7)
medianrent<-rent[3:89,c(2,120,128,136,144,152,160,168,176,184,192)]
vic.lga.shp<-readShapeSpatial("vmlite_lga_cm.shp")

#Understand & Tidy Data

colnames(crime)<-c("Year","Ending","Region","LGA","Incidents","Rate")
crime$Year<-factor(crime$Year,levels=c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022),ordered = TRUE)
crime$Region<-factor(crime$Region,levels=c("1 North West Metro","2 Eastern","3 Southern Metro","4 Western","Justice Institutions and Immigration Facilities","Unincorporated Vic"),labels=c("North West Metro","Eastern","Southern Metro","Western","Justice Institutions and Immigration Facilities","Unincorporated Vic"))
crime$LGA<-as.character(crime$LGA)
crime<-crime %>% select(-Ending)
crime<-subset(crime,crime$LGA != "Total" & crime$LGA != "Justice Institutions and Immigration Facilities" & crime$LGA !="Unincorporated Vic")
crime$Rate<-round(crime$Rate,digits=2)
crime<-arrange(crime,LGA)

colnames(medianrent)<-c("LGA","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022")
medianrent<-medianrent %>% pivot_longer(names_to="Year",values_to="Median Rent",cols=2:11)
medianrent$LGA[medianrent$LGA == "Mornington Penin'a"]<-"Mornington Peninsula"
medianrent<-subset(medianrent,medianrent$LGA != "Group Total")
medianrent$`Median Rent`<-as.numeric(medianrent$`Median Rent`)
medianrent<-arrange(medianrent,LGA)

crimerent<-inner_join(crime,medianrent,by=c("Year","LGA"))
crimerent$Year<-factor(crimerent$Year,levels=c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022),ordered = TRUE)
crimerent<-arrange(crimerent,Year)
colnames(crimerent)<-c("year","region","lga_name","incidents","rate","median_rent")

crimerent<-crimerent %>% group_by(lga_name) %>% arrange(year,.by_group = TRUE) %>% mutate_at(vars('median_rent'), ~replace_na(.,median(.,na.rm=TRUE)))
crimerent1<-crimerent[c("year","region","lga_name","incidents","median_rent")]
crimerent1$lga_name<-as.factor(crimerent1$lga_name)

crimerent2013<-filter(crimerent1, year == 2013)

crimeclean<-crimerent1[c("year","region","lga_name","incidents")]
rentclean<-crimerent1[c("year","region","lga_name","median_rent")]

write.csv(crimeclean,"crimeclean.csv",row.names=FALSE)
write.csv(rentclean,"rentclean.csv",row.names=FALSE)

#app.R

#import data
crime<-readr::read_csv("crimeclean.csv")
rent<-readr::read_csv("rentclean.csv")
crimerent<-inner_join(crime,rent,by=c("year","lga_name","region"))
crimerent$lga_name<-as.factor(crimerent$lga_name)
crimerent1<-crimerent
colnames(crimerent1)<-c("Year","Region","lga_name", "Criminal Incidents","Median Rent")

crimerentmedian<-crimerent %>% group_by(year) %>% summarise_at(vars(incidents:median_rent),median,na.rm=TRUE) %>% round()

vicmapdata<-readShapeSpatial("vmlite_lga_cm.shp")
plot(vicmapdata)
vicmapdata$lga_name<-str_to_title(vicmapdata$lga_name)
vicmapdata1<-merge(crimerent,vicmapdata,by="lga_name",duplicateGeoms=TRUE)
test<-as.data.frame(vicmapdata1)
vicmapdatalga<-vicmapdata %>% as.data.frame() 
vicmapdatalga$lga_name<-vicmapdatalga$lga_name %>% as.character() 
vicmapdatalga<-arrange(vicmapdatalga,vicmapdatalga$lga_name)

#ui
ui<-fluidPage(
  theme=shinytheme("lumen"),
  titlePanel("Analysis: Does Criminal Incidents affect the Median Rent in Victoria?"),
  tabsetPanel(
    tabPanel("Data",fluid=TRUE,
             titlePanel("Criminal Incidents and Median Rent by Year"),
             sidebarLayout(position='right', 
                           sidebarPanel(
                             selectInput("type","Data Type",choices=c("Criminal Incidents","Median Rent"),selected = "Criminal Incidents"),
                             sliderInput("year","Year",min=min(crimerent1$Year),max=max(crimerent1$Year),value="2022",sep=""),
                             pickerInput("lgainput","Local Government Area",choices=(unique(vicmapdatalga$lga_name)),
                                         options=pickerOptions(actionsBox=TRUE,liveSearch=TRUE,noneSelectedText="Select Local Government Area",selectedTextFormat='count>4'),
                                         multiple=T,selected = NULL)
                           ),
                           mainPanel(
                             fluidRow(width=6,
                                      leafletOutput("mymap",width="200%")),
                             fluidRow(
                               splitLayout(cellWidths = c("75%","75%"),plotlyOutput("scatter"),plotlyOutput("line"))
                             )
                           )
             )
    ),
    
    tabPanel("Data Cont.",fluid=TRUE,
             titlePanel("Criminal Incidents and Median Rent by Year"),
             mainPanel(
               fluidRow(
                 splitLayout(cellWidths = c("75%","76%"),plotlyOutput("hist1"),plotlyOutput("hist3"))
               ),
               fluidRow(
                 splitLayout(cellWidths = c("75%","20%","55%"),plotlyOutput("hist2"), 
                             verbatimTextOutput("text9"),
                             sliderInput("year2","Year Input",min=min(crimerent1$Year),max=max(crimerent1$Year),value="2022",sep="",width = "75%")
                 )
               ))
    ),
    
    tabPanel("References",fluid=TRUE,
             titlePanel("Reference List"),
             mainPanel(verbatimTextOutput("text"),
                       htmlOutput("text2")))
  )
)

#server
server<-function(input,output,session) {
  
  output$mymap <- renderLeaflet({
    
    df1<-input$type
    df2<-input$year
    df3<-crimerent1[,c("lga_name","Year",df1)]
    df3_filter<-df3 %>% dplyr::filter(df3$Year == df2)
    df4<-df3_filter
    colnames(df4)<-c("lga_name","Year","Value")
    
    crimerentfilter<-crimerent1[,c("lga_name","Year",df1)] %>% dplyr::filter(Year == df2)
    colnames(crimerentfilter)<-c("lga_name","Year","Value")
    
    df5_all<-merge(vicmapdata,crimerentfilter,by="lga_name")
    
    bins<-quantile(df5_all$Value,probs=seq(0,1,.2),names=FALSE,na.rm=TRUE)
    
    pal<-colorBin("PuRd",domain=(df5_all$Value),bins=bins)
    
    mylabels<-paste(
      "LGA: ",df5_all$lga_name,"<br/>",
      "Year: ",df2,"<br/>",
      paste(df1,":",sep=""), df5_all$Value) %>% 
      lapply(htmltools::HTML)
    
    leaflet(vicmapdata) %>%
      setView(lng=147.5,lat=-37.7,zoom=7.5) %>%
      addLegend(pal=pal,
                values=~df4$Value,
                position="bottomleft",
                title=colnames(df3[,3])
      )%>%
      addPolygons(
        fillColor=~pal(df5_all$Value),
        weight=2,
        opacity=1,
        color="white",
        dashArray="3",
        fillOpacity = 0.7,
        label = mylabels,
        labelOptions = labelOptions(
          style=list("font-weight" = "normal", padding= "3px 8px"),
          textsize="13px",
          direction="auto"),
        highlight=highlightOptions(
          weight=5,
          color="black",
          dashArray = "",
          fillOpacity=1,
          bringToFront = TRUE)) %>%
      addStaticLabels(.,label=vicmapdata$lga_name,
                      style=list("color"="black","font-weight"="bold")) %>%
      addPolygons(fillColor="yellow",
                  weight=1,
                  opacity=1,
                  color="black",
                  dashArray="3",
                  fillOpacity=0.5,
                  label=mylabels,
                  labelOptions=labelOptions(
                    style=list("font-weight" = "normal", padding= "3px 8px"),
                    textsize="13px",
                    direction="auto"),
                  highlight=highlightOptions(
                    weight=5,
                    color="black",
                    bringToFront = TRUE,
                    dashArray= "",
                    fillOpacity=1),
                  stroke=TRUE,
                  group=~lga_name) %>%
      hideGroup(group=vicmapdata$lga_name)
    
  })
  
  proxy<-leafletProxy("mymap")
  
  selected<-reactiveValues(groups = vector())
  
  observeEvent(input$map_shape_click, {
    if(input$map_shape_click$lga_name == "lga_name"){
      selected$groups<-c(selected$groups,input$map_shape_click$lga_name)
      proxy %>% showGroup(group = input$map_shape_click$lga_name)
    } else{
      selected$groups<-setdiff(selected$groups, input$map_shape_click$group)
      proxy %>% hideGroup(group = input$map_shape_click$group)
    }
    updatePickerInput(session,
                      "lgainput",
                      choices=unique(vicmapdata$lga_name),
                      selected=selected$groups)
  })
  
  observeEvent(input$lgainput, {
    removed_via_selectInput<-setdiff(selected$groups,input$lgainput)
    added_via_selectInput<-setdiff(input$lgainput,selected$groups)
    
    if(length(removed_via_selectInput)>0){
      selected$groups<-input$lgainput
      proxy %>% hideGroup(group=removed_via_selectInput)
    }
    
    if(length(added_via_selectInput)>0){
      selected$groups<-input$lgainput
      proxy %>% showGroup(group=added_via_selectInput)
    }
  },ignoreNULL = FALSE)
  
  output$scatter<-renderPlotly({
    
    gapminder %>%
      plot_ly() %>%
      add_markers(x=crimerent1$`Median Rent`,
                  y=crimerent1$`Criminal Incidents`,
                  frame=crimerent1$Year,
                  mode='text',
                  text=crimerent1$lga_name,
                  size=crimerent1$`Median Rent`,
                  color=crimerent1$Region,
                  colors="RdPu") %>%
      
      layout(title=list(text="Scatter Plot of Criminal Incidents and Median Rent (2013-2022)",y=0.98,x=0.1),
             xaxis=list(showgrid =FALSE, zeroline=FALSE, title="Median Rent"),
             yaxis=list(showgrid =TRUE, zeroline=FALSE, title="Criminal Incidents")) %>%
      config(displaylogo=FALSE,modeBarButtonsToRemove=c("austoscale2d","lasso2d","zoomIn2d", "zoomOut2d"))%>%
      hide_legend()
    
  })
  
  output$line<-renderPlotly({
    
    plot_ly(
      x=crimerentmedian$year,
      y=(round(crimerentmedian$incidents/10)),
      name="Median Criminal Incidents ('10)",
      type="scatter",
      mode="lines+markers",
      line=list(width=2)) %>%
      
      add_trace(y=crimerentmedian$median_rent, name="Median Rent") %>%
      
      layout(title=list(text="Line Plot of Total Median Criminal Incidents ('10) and Median Rent (2013-2022)",y=0.98,x=0.1),
             xaxis=list(title="Year",showgrid =FALSE,tickvals=unique(crimerentmedian$year)),
             yaxis=list(title="Value")) %>% 
      
      config(displaylogo=FALSE,modeBarButtonsToRemove=c("austoscale2d","lasso2d","zoomIn2d", "zoomOut2d"))
    
    
  })
  
  output$hist1<-renderPlotly({
    
    histdf1<-input$year2
    histdf2<-crimerent
    histdf3<-histdf2 %>% dplyr::filter(histdf2$year == histdf1)
    
    plot_ly(
      data=histdf3,
      y=histdf3$lga_name,
      x=histdf3$incidents,
      type="bar",
      height = 1100
    ) %>%
      layout(yaxis=list(tickvals=unique(histdf3$lga_name),categoryorder="total ascending"),
             xaxis=list(title="Criminal Incidents"),
             title=list(text=paste("Number of Criminal Incidents in",histdf1))) %>%
      config(displaylogo=FALSE,modeBarButtonsToRemove=c("austoscale2d","lasso2d","zoomIn2d", "zoomOut2d"))
    
    
  })
  
  output$hist2<-renderPlotly({
    
    histdf1<-input$year2
    histdf2<-crimerent
    histdf3<-histdf2 %>% dplyr::filter(histdf2$year == histdf1)
    
    plot_ly(
      data=histdf3,
      y=histdf3$lga_name,
      x=histdf3$median_rent,
      type="bar",
      marker=list(color="rgb(255,127,15"),
      height = 1100
    ) %>%
      layout(yaxis=list(tickvals=unique(histdf3$lga_name),categoryorder="total ascending"),
             xaxis=list(title="Median Rent"),
             title=list(text=paste("Number of Median Rent in",histdf1))) %>%
      config(displaylogo=FALSE,modeBarButtonsToRemove=c("austoscale2d","lasso2d","zoomIn2d", "zoomOut2d"))
    
    
  })
  
  
  output$hist3<-renderPlotly({
    
    histdf1<-input$year2
    histdf2<-crimerent
    histdf3<-histdf2 %>% dplyr::filter(histdf2$year == histdf1)
    
    fig<-plot_ly(data=histdf3,
                 x=round(histdf3$incidents/10),
                 y=histdf3$lga_name,
                 type="bar",
                 name="Incidents ('10)",
                 height = 1100)
    
    fig<-fig %>% add_trace(x=histdf3$median_rent,
                           name="Median Rent")
    
    fig<-fig %>% layout(yaxis=list(tickvals=unique(histdf3$lga_name),categoryorder="total ascending"),
                        xaxis=list(title="Incidents ('10) and Median Rent"),
                        title=list(text=paste("Number of Criminal Incidents ('10) v Median Rent in",histdf1)),
                        barmode="stack") %>%
      config(displaylogo=FALSE,modeBarButtonsToRemove=c("austoscale2d","lasso2d","zoomIn2d", "zoomOut2d"))
    
    fig
    
  })
  
  output$text2<-renderUI({
    HTML("DataVic (2023) Crime Statistics Agency Data Tables - Criminal Incidents [data set], DataVic website, accessed 20 May 2023. https://discover.data.vic.gov.au/dataset/criminal-incident","<br/>",
         "<br/>","DFFH (2023) Rental report [data set], DFFH website, accessed 20 May 2023. https://www.dffh.vic.gov.au/publications/rental-report")
    
    
  })
}

shinyApp(ui=ui,server=server)
