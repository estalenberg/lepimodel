#prototype app
#only thermal niche and water turnover maps

#https://github.com/mrke/NicheMapR/blob/master/R/ellipsoid_endo.R

#load packages####
library(shiny)
library(shinythemes)
library(raster)  # grids, rasters
library(rgdal) # to load polygon
library(rasterVis)  # raster visualisation
library(gridExtra)
library(grid)
library(RColorBrewer) 
library(shinyjs)
library(rgdal)
#memory.limit(size=20000)

#detach ggplot#### for mapping
detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}
detach_package("cowplot", TRUE)
detach_package("rms", TRUE)
detach_package("Hmisc", TRUE)
detach_package("ggplot2", TRUE)


#load data####
mins <- read.csv("data/mins.csv", stringsAsFactors = T) #baseline data
#rmrobs<-read.csv("data/rmrobs.csv")


tavg1<-raster("data/tavg1_7506.tif")
tmax1<-raster("data/tmax1_7506.tif")

tavg7<-raster("data/tavg7_7506.tif")
tmax7<-raster("data/tmax7_7506.tif")

list.ras <- list(tavg1,tmax1,tavg7,tmax7)
temp.brick <- brick(list.ras)
names(temp.brick) <- c(1,2,3,4)


mask<-readOGR(dsn = "./data", layer = "mask")
lepishp<-readOGR(dsn = "./data",layer =  "lepishp")
labels=as.character(lepishp@data$binomial)
(data.frame(strsplit(labels, " "))[2,])
l=(lapply(strsplit(labels," "), function(x) x[2]))
lepishp@data$labels=l

#ui####
ui <- tagList(
  useShinyjs(),
  div(id = "myapp",
      navbarPage("",fluid = T,theme = shinytheme("flatly"),
                 tabPanel(h4("Welcome"),
                          fluidPage(
                            fluidRow(
                              column(width = 1),
                              column(5, 
                                     #text####
                                     h2("Modelling the thermal niche of primates"),
                                     h3("DRAFT VERSION: A interactive mechanistic model of",
                                        em("Lepilemur"), "in Madagascar"),
                                     h3("Author: Eleanor Stalenberg, 2018"),
                                     p("Address: Division of Ecology & Evolution, Research School of Biology, College of Science. 46 Sullivan's Creek Rd."),
                                     p("The Australian National University, Acton ACT  2601. Australia. Phone: +61 2 6125 2866; 
                                       Email: Eleanor.Stalenberg@anu.edu.au"),
                                     br(),
                                     h4("Abstract"),
                                     p("An Interactive biophysical model of",
                                       em("Lepilemur"),
                                       "developed using the 'Ellipsoid' endotherm model in the NicheMapR R package (Kearney 2018) with modifications informed 
                                       by our case study of", em("L. leucopus"), "at Berenty, southern Madagascar (e.g. Bethge et al 2017).
                                       Maps of", em("Lepilemur"), "thermal neutral zone and daily water loss predictions are plotted with
                                       mean and maximum monthly temperature layers 
                                       (Stalenberg et al, 2018)."),
                                     br(),
                                     br(),
                                     helpText("Cite as: Stalenberg, E., 2018. Modelling the thermal niche of primates: A draft interactive mechanistic model of",
                                              em("Lepilemur"), "in Madagascar. DRAFT VERSION https://github.com/estalenberg "),
                                     br(),
                                     br(),
                                     helpText("Citations:"),
                                     helpText("- Bethge, J., Wist, B., Stalenberg, E., Dausmann, K. 2017. Seasonal adaptations in energy budgeting in the primate", 
                                              em("Lepilemur leucopus."),
                                              "Journal of comparative physiology. B, Biochemical, systemic, and environmental physiology 187:827-834."),
                                     helpText("- Kearney M. R. 2018. NicheMapR: R implementation of Niche Mapper software for biophysical modelling. R Core Team."),
                                     helpText("- Kearney, M. R., and W. P. Porter. 2017. NicheMapR - 
                                              an R package for biophysical modelling: the microclimate model. 
                                              Ecography 40:664-674."),
                                     helpText("- Porter, W. P., and M. Kearney. 2009. Size, shape, and the 
                                              thermal niche of endotherms. Proceedings of the National
                                              Academy of Sciences 106:19666-19672."),
                                     helpText("- Porter W. P., Munger J. C., Stewart W. E., Budaraju S., Jaeger J. 1994. Endotherm Energetics - 
                                              From a Scalable Individual-Based Model to Ecological Applications. Aust. J. Zool. 42:125"),
                                     helpText("- Stalenberg, E., Hutchinson, M., F., Foley, W., J. 2018.
                                              Using historical normals to improve modern monthly climate normal surfaces for Madagascar. Int. J. Climatol. 24:1-24")
                                     ),
                              column(5,
                                     #pic###
                                     br(),
                                     br(),
                                     br(),
                                     img(src = "IMG_4508.jpg", height = 400, width = 530)
                              ),
                              column(width=1)
                                     )
                              )
                            ),
                 #data ui####
                 tabPanel(h4("Run the model!"),
                          fluidRow(
                            column(3,
                                   tabsetPanel( 
                                     tabPanel(h4("1. Modify the Microclimate"),
                                              h4("Climate"),
                                              helpText("Change the temperature, wind speed and humidity"),
                                              sliderInput("tempInput", "Change in temperature (degC)",-10 , 10, 0),
                                              sliderInput("windInput", "Windspeed (m/s)",0 , 10, 0),
                                              sliderInput("rhInput", "Relative humidity (%)",5 , 95, 50),
                                              sliderInput("pwaterInput", "Water content of food (%)",10 , 90, 54)
                                     ),
                                     tabPanel(h4("2. Modify Your Lemur"),
                                              h4("Physiology and behaviour"),
                                              radioButtons("sexInput", label = h4("Select your lemur"),
                                                           choices = list("Female" = 1, "Male" = 2), inline=T,
                                                           selected = 1),
                                              helpText("Change the physiological inputs"),
                                              sliderInput("massInput", "Lemur mass (g)", 450, 650, 574), #
                                              sliderInput("furdInput", "Fur depth (mm)", 10, 30, 19), #19
                                              sliderInput("furcInput", "Fur conductivity (W.m2)", 0.020, 0.065, 0.044),
                                              sliderInput("qInput", "Q10 (rate of MR inc. with inc temp)", 1.0, 4.5, 2.5),
                                              sliderInput("postureInput", "Possible posture range  (length:width, i.e. the shape of your lemur):",
                                                          min = 1.1, max = 5,
                                                          value = c(1.1,4)),
                                              sliderInput("wetInput", "Percent of surface area that is wet", 0, 15, 0)
                                     ),
                                     tabPanel(h4("3. Select Output and GIS"),
                                              h4("Select output"),
                                              selectInput("output", h5("What do you want to map?"), 
                                                          list("Thermal Niche" = 1, "Water turnover" = 2),selected = 1),
                                              h4("Select temperature layer (1976-2005 average)"), 
                                              selectInput("templayer", h5("Temperature layers"), 
                                                          list("Average temperature 1976-2005" = 1, "Maximum temperature 1976-2005" = 2),selected = 1),
                                              checkboxInput(inputId = "lepishpInput",label="Add IUCN Lepilemur extent (source: http://www.iucnredlist.org/technical-documents/spatial-data)?",value=F)
                                     )
                                   )),
                            #Seasons tabs ####
                            column(width = 9,
                                   tabsetPanel(     
                                     tabPanel(h4("Output: maps"),
                                              tabsetPanel(
                                                tabPanel("Summer wet season",
                                                         br(),
                                                         actionButton("resets", "Reset form"),
                                                         plotOutput("summerplot",width = "150%",height = "700px")
                                                ),
                                                tabPanel("Winter dry season",
                                                         br(),
                                                         actionButton("resetw", "Reset form"),
                                                         plotOutput("winterplot",width = "150%",height = "700px")))),
                                     
                                     #graphs#####
                                     tabPanel(h4("Output: Graphs and data"),
                                              tabsetPanel(
                                                tabPanel("Graphs",
                                                         br(),
                                                         actionButton("resetg", "Reset form"),
                                                         plotOutput("graph.sum",width = "80%", height = "400px"),
                                                         br(),
                                                         br(),
                                                         plotOutput("graph.win",width = "80%", height = "400px"),
                                                         br()
                                                         )
                                                )
                                              )
                                     )
                                     
                                   )
                            )
                          )
                 )
                 )
)

#!server####                 
server <- function(input, output, session) {
  observeEvent(input$resetg, {
    shinyjs::reset("myapp")
  })
  observeEvent(input$resets, {
    shinyjs::reset("myapp")
  })
  observeEvent(input$resetw, {
    shinyjs::reset("myapp")
  })
  
  #graphs plots####
  output$graph.sum<-renderPlot({
    
    category=2+as.numeric(input$sexInput)
    
    source("source_proto.R",keep.source = T,local = TRUE)
    
    #graphs####
    minimummlh=(min(allout$mlO2phL[allout$mlO2phL>0]))
    
    LCT=(min(allout$airT[allout$mlO2phL==minimummlh],na.rm=T))
    if(is.infinite(LCT)){LCT=5} else {LCT=LCT}
    if(LCT<5){LCT=5} else {LCT=LCT}
    UCT=(max(allout$airT[allout$mlO2phL==minimummlh],na.rm=T))
    if(is.infinite(UCT)){UCT=44} else {UCT=UCT}
    if(UCT>44){UCT=44} else {UCT=UCT}
    cold=max(allout$airT[allout$airT<=LCT& allout$mlO2phL>=minimummlh*1.25]) 
    if(is.infinite(cold)){cold=1} else {cold=cold}
    if(cold<1){cold=1} else {cold=cold}
    hot=max(allout$airT[allout$airT>UCT& allout$mlO2phL<=minimummlh*1.25])
    if(is.infinite(hot)){hot=45} else {hot=hot}
    if(hot>45){hot=45} else {hot=hot}
    warm=(((LCT+UCT)/2)+UCT)/2
    
    
    mmass=mass*1000
    mintake=(as.numeric(allout$pintake))*(mmass)
    mpwater=0+(input$pwaterInput/100)
    mfood=as.numeric(mpwater*(mintake/(1-mpwater)))/24
    datfood <- data.frame(y = (mfood), name = "Water from food")
    datWTR <- data.frame(y = (115.7706), name = "Field WTR")
    
    my.col2=c('blue','green','black')
    my.lty=c("solid", "solid", "solid","dashed",'dashed')
    my.settings2 <- list(superpose.line=list(col=my.col2,my.lty)) 
    
    allout$h2od=allout$H2O_gph*24
    allout$h2odL=allout$H2O_gphL*24
    allout$wtrd=allout$wtr*24
    
    
    swtrgraph<-xyplot(allout$h2od~allout$airT,type='l', col="blue",lwd=1.5 ,ylim=c(0,200),xlim=c(0,46),
                      xlab = "Air Temperature (deg C)", ylab = "Water turnover (g/day)", 
                      main=list("Water turnover predictions",cex=1.3),
                      scales=list(x=list(at=seq(0,45,5)),y=list(at=seq(0,200,20))),
                      par.settings = my.settings2,
                      panel = function(...) {
                        panel.xyplot(...)
                        panel.abline(h=(datfood$y*24), lty = "dashed", col = "black")
                        llines(allout$airT, allout$wtrd, col="green", lwd=1.5)
                        panel.key(c("Water tnr default","Water tnr + 'Licking' & evapWL","Water from food"),lines=T, #Water tnr + evapWL
                                  col=c(my.col2),lty=c(my.lty), cex=1,corner = c(.1,.1), points = FALSE)
                      })
    
    
    my.col=c('blue','magenta','white','#2166ac','#67a9cf','#ef8a62','#b2182b')
    
    my.settings <- list(superpose.line=list(col=my.col))  
    
    allout$fmr_kjd=(allout$fmr_kjhL*24)
    datDEE <- data.frame(y = (264.7735), name = "FMR")
   
    
       stnzgraph<-xyplot((allout$fmr_kjd~allout$airT),type='l', col="blue",lwd=1.5 ,ylim=c(0,700),xlim=c(0,46),
                     xlab = "Air Temperature (deg C)", ylab = "Metabolic Rate (kj/day)", 
                     main=list("Metabolic rate predictions",cex=1.3),
                      scales=list(x=list(at=seq(0,45,5)),y=list(at=seq(0,650,50))),
                      par.settings = my.settings,
                      panel = function(...) {
                        panel.abline(v=c(cold,LCT,UCT,hot), 
                                     lty = "dotted", col = c(my.col[4:7]))
                        llines(airT, rmr_statkjd, col="magenta", lwd=1.5)
                        panel.key(c("Mechanistic", "Statistic"," ","Cold stress","LCT","UCT","Heat stress"),lines=T,
                                  col=c(my.col[1:8]), cex=1,corner = c(1,.98), points = FALSE)
                        panel.xyplot(...)})
    
    grid.arrange(stnzgraph,swtrgraph,ncol=2,top = textGrob("Summer Wet Season",gp=gpar(fontsize=18,font=3)))
  })
  
  output$graph.win<-renderPlot({
    
    #win####
    category=0+as.numeric(input$sexInput)
    
    source("source_proto.R",keep.source = T,local = TRUE)
    
    
    #graphs####
    minimummlh=(min(allout$mlO2ph[allout$mlO2ph>0]))
    
    LCT=(min(allout$airT[allout$mlO2phL==minimummlh],na.rm=T))
    if(is.infinite(LCT)){LCT=5} else {LCT=LCT}
    if(LCT<5){LCT=5} else {LCT=LCT}
    UCT=(max(allout$airT[allout$mlO2phL==minimummlh],na.rm=T))
    if(is.infinite(UCT)){UCT=44} else {UCT=UCT}
    if(UCT>44){UCT=44} else {UCT=UCT}
    cold=max(allout$airT[allout$airT<=LCT& allout$mlO2phL>=minimummlh*1.25]) 
    if(is.infinite(cold)){cold=1} else {cold=cold}
    if(cold<1){cold=1} else {cold=cold}
    hot=max(allout$airT[allout$airT>UCT& allout$mlO2phL<=minimummlh*1.25])
    if(is.infinite(hot)){hot=45} else {hot=hot}
    if(hot>45){hot=45} else {hot=hot}
    warm=(((LCT+UCT)/2)+UCT)/2
    
    
    mmass=mass*1000
    mintake=(as.numeric(allout$pintake))*(mmass)
    mpwater=0+(input$pwaterInput/100)
    mfood=as.numeric(mpwater*(mintake/(1-mpwater)))/24
    datfood <- data.frame(y = (mfood), name = "Water from food")
    datWTR <- data.frame(y = (110.9828), name = "Field WTR")
    
    my.col2=c('blue','green','black','grey50')
    my.lty=c("solid", "solid", "solid","dashed",'dashed')
    my.settings2 <- list(superpose.line=list(col=my.col2,my.lty)) 
    
    allout$h2od=allout$H2O_gph*24
    allout$h2odL=allout$H2O_gphL*24
    allout$wtrd=allout$wtr*24
    
    wwtrgraph<-xyplot(allout$h2od~allout$airT,type='l', col="blue",lwd=1.5 ,ylim=c(0,200),xlim=c(0,46),
                      xlab = "Air Temperature (deg C)", ylab = "Water turnover (g/day)", 
                      main=list("Water turnover predictions",cex=1.3),
                      scales=list(x=list(at=seq(0,45,5)),y=list(at=seq(0,200,20))),
                      par.settings = my.settings2,
                      panel = function(...) {
                        panel.xyplot(...)
                        panel.abline(h=(datfood$y*24), lty = "dashed", col = "black")
                        llines(allout$airT, allout$wtrd, col="green", lwd=1.5)
                        panel.key(c("Water tnr default","Water tnr + 'Licking' & evapWL","Water from food"),lines=T, #Water tnr + evapWL
                                  col=c(my.col2),lty=c(my.lty), cex=1,corner = c(.1,.1), points = FALSE)
                      })
    
    
    
    my.col=c('blue','magenta','white','#2166ac','#67a9cf','#ef8a62','#b2182b')
    
    
    my.settings <- list(superpose.line=list(col=my.col))  
    
    allout$fmr_kjd=(allout$fmr_kjhL*24)
    datDEE <- data.frame(y = (257.7118), name = "FMR")
    
    
    wtnzgraph<-xyplot(allout$fmr_kjd~allout$airT,type='l', col="blue",lwd=1.5 ,ylim=c(0,650),xlim=c(0,46),
                      xlab = "Air Temperature (deg C)", ylab = "Metabolic Rate (ml/h)", 
                      main=list("Metabolic rate predictions",cex=1.3),
                      scales=list(x=list(at=seq(0,45,5)),y=list(at=seq(0,650,50))),
                      par.settings = my.settings,
                      panel = function(...) {
                        panel.abline(v=c(cold,LCT,UCT,hot), 
                                     lty = "dotted", col = c(my.col[4:7]))
                        llines(airT, rmr_statkjd, col="magenta", lwd=1.5)
                        panel.abline(h=(datDEE$y), lty = "dashed", col = "white")
                        panel.key(c("Mechanistic", "Statistic"," ","Cold stress","LCT","UCT","Heat stress"),lines=T,
                                  col=c(my.col[1:8]), cex=1,corner = c(1,.98), points = FALSE)
                        panel.xyplot(...)})
    
    #grid arrange####
    grid.arrange(wtnzgraph,wwtrgraph,ncol=2,top = textGrob("Winter Dry Season",gp=gpar(fontsize=18,font=3)))
    
  })
  
  #summerplot####
  output$summerplot<-renderPlot({ 
    
    category=2+as.numeric(input$sexInput) #summer is 3 and 4 (f and m) and dry is 1 and 2 f/m (1 female, 2 male)
    
    
    source("source_proto.R",keep.source = T,local = TRUE)
    
    #TNZ map####
    minimummlh=min(allout$mlO2ph)
    
    LCT=(min(allout$airT[allout$mlO2phL==minimummlh],na.rm=T))
    if(is.infinite(LCT)){LCT=5} else {LCT=LCT}
    if(LCT<5){LCT=5} else {LCT=LCT}
    UCT=(max(allout$airT[allout$mlO2phL==minimummlh],na.rm=T))
    if(is.infinite(UCT)){UCT=44} else {UCT=UCT}
    if(UCT>44){UCT=44} else {UCT=UCT}
    cold=max(allout$airT[allout$airT<=LCT& allout$mlO2phL>=minimummlh*1.25]) 
    if(is.infinite(cold)){cold=0} else {cold=cold}
    if(cold<0){cold=0} else {cold=cold}
    hot=max(allout$airT[allout$airT>UCT& allout$mlO2phL<=minimummlh*1.25])
    if(is.infinite(hot)){hot=46} else {hot=hot}
    if(hot>46){hot=46} else {hot=hot}
    warm=(((LCT+UCT)/2)+UCT)/2
    
    
    
    #this base layer stays the same throughout 1 and 2 for summer avg and max
    layer.sum=0+as.numeric(input$templayer)
    temp <- subset(temp.brick,layer.sum)
    temp<-temp+input$tempInput
    
    list<-c(cold,LCT,warm,UCT,hot)
    list <- list[order(list)]
    list=list[is.finite(list)]
    
    min=ifelse(temp@data@min<min(list),((round(temp@data@min,digits=0))-1),min(list)-2)
    max=ifelse(temp@data@max>max(list),((round(temp@data@max,digits=0))+1),max(list)+2)
    
   
    #tnzbreaks<-c(temp@data@min,cold,LCT,warm,UCT,hot,temp@data@max)
    tnzbreaks<-c(min,cold,LCT,warm,UCT,hot,max)
    tnznames=c(min, "Cold stress","Lower Critical Temp.","Warm","Upper Critical Temp.","Heat stress",max)
    num<-seq(1,7,by=1)
    tnzdata<-cbind.data.frame(num,tnznames,tnzbreaks)
    dat<-tnzdata
    dat$tnzbreaks[!is.finite(dat$tnzbreaks)] <- 0
    dat<-subset(dat, !ave(dat$tnzbreaks, FUN = duplicated))
    dat <- dat[order(dat$tnzbreaks),]
    
    tnzbreak=as.numeric(dat$tnzbreaks)
    tnzmapTheme <- rasterTheme(region=c('#2166ac','#67a9cf','white','#fddbc7','#ef8a62','#b2182b')) # white ,'#f7f7f7' 
    tnz.at=as.character(dat$tnznames)
    tnz.c.key <- list(at=tnzbreak, labels=list(at=tnzbreak, labels=tnz.at))
    
    
    tnzplot<-levelplot(temp,margin=F,maxpixels=1e5, cex.axis=2,par.settings=tnzmapTheme,at=tnzbreak,colorkey=tnz.c.key, 
                       main=list("L. leucopus Thermal Niche Map",cex=1.5))+ 
      layer(sp.polygons(mask),data=list(mask=mask))
    
    #water maps####
    #1 and 2 for summer avg and max
    layer.sum=0+as.numeric(input$templayer)
    tempwtr <- subset(temp.brick,layer.sum)
    tempwtr<-tempwtr+input$tempInput
    
    allout$x<-allout$wtr #may need to subset this to limit the modelled tas to within the range of the spatial layer
    
    w<-seq((min(allout$x[allout$x>0])),(max(allout$x)+0.1),length=40)
    
    t=(c(min(allout$airT[allout$x<=w[1]],na.rm=T),(max(allout$airT[allout$x<w[2]],na.rm=T))
         ,(max(allout$airT[allout$x<w[3]],na.rm=T)),(max(allout$airT[allout$x<w[4]],na.rm=T))
         ,(max(allout$airT[allout$x<w[5]],na.rm=T)),(max(allout$airT[allout$x<w[6]],na.rm=T))
         ,(max(allout$airT[allout$x<w[7]],na.rm=T)),(max(allout$airT[allout$x<w[8]],na.rm=T))
         ,(max(allout$airT[allout$x<w[9]],na.rm=T)),(max(allout$airT[allout$x<w[10]],na.rm=T))
         ,(max(allout$airT[allout$x<w[11]],na.rm=T)),(max(allout$airT[allout$x<w[12]],na.rm=T))
         ,(max(allout$airT[allout$x<w[13]],na.rm=T)),(max(allout$airT[allout$x<w[14]],na.rm=T))
         ,(max(allout$airT[allout$x<w[15]],na.rm=T)),(max(allout$airT[allout$x<w[16]],na.rm=T))
         ,(max(allout$airT[allout$x<w[17]],na.rm=T)),(max(allout$airT[allout$x<w[18]],na.rm=T))
         ,(max(allout$airT[allout$x<w[19]],na.rm=T)),(max(allout$airT[allout$x<w[20]],na.rm=T))
         ,(max(allout$airT[allout$x<w[21]],na.rm=T)),(max(allout$airT[allout$x<w[22]],na.rm=T))
         ,(max(allout$airT[allout$x<w[23]],na.rm=T)),(max(allout$airT[allout$x<w[24]],na.rm=T))
         ,(max(allout$airT[allout$x<w[25]],na.rm=T)),(max(allout$airT[allout$x<w[26]],na.rm=T))
         ,(max(allout$airT[allout$x<w[27]],na.rm=T)),(max(allout$airT[allout$x<w[28]],na.rm=T))
         ,(max(allout$airT[allout$x<w[29]],na.rm=T)),(max(allout$airT[allout$x<w[30]],na.rm=T))
         ,(max(allout$airT[allout$x<w[31]],na.rm=T)),(max(allout$airT[allout$x<w[32]],na.rm=T))    
         ,(max(allout$airT[allout$x<w[33]],na.rm=T)),(max(allout$airT[allout$x<w[34]],na.rm=T))
         ,(max(allout$airT[allout$x<w[35]],na.rm=T)),(max(allout$airT[allout$x<w[36]],na.rm=T))
         ,(max(allout$airT[allout$x<w[37]],na.rm=T)),(max(allout$airT[allout$x<w[38]],na.rm=T))
         ,(max(allout$airT[allout$x<w[39]],na.rm=T)),(max(allout$airT[allout$x<w[40]],na.rm=T))
    ))
    wtr_dat<-cbind.data.frame(t,w)
    wtr_dat<-wtr_dat[is.finite(wtr_dat$t),]
    wtr_dat<-wtr_dat[!duplicated(wtr_dat$t),]
    
    wtr_dat1<-subset(wtr_dat,wtr_dat$t>tempwtr@data@min)
    wtr_dat1<-subset(wtr_dat1,wtr_dat1$t<=tempwtr@data@max)
    
    t<-wtr_dat1$t
    w<-wtr_dat1$w
    
    wtr_h <- reclassify(tempwtr, cbind(0, t[1], (min(allout$x[allout$x>0])))) #from min temp make minimum water loss
    if(length(t)>40){ wtr_h <- reclassify(wtr_h, cbind( t[40],wtr_h@data@max,w[40]))} else {wtr_h<-wtr_h}
    if(length(t)>39){ wtr_h <- reclassify(wtr_h, cbind( t[39],wtr_h@data@max,w[39]))} else {wtr_h<-wtr_h}
    if(length(t)>38){ wtr_h <- reclassify(wtr_h, cbind( t[38],wtr_h@data@max,w[38]))} else {wtr_h<-wtr_h}
    if(length(t)>37){ wtr_h <- reclassify(wtr_h, cbind( t[37],wtr_h@data@max,w[37]))} else {wtr_h<-wtr_h}
    if(length(t)>36){ wtr_h <- reclassify(wtr_h, cbind( t[36],wtr_h@data@max,w[36]))} else {wtr_h<-wtr_h}
    if(length(t)>35){ wtr_h <- reclassify(wtr_h, cbind( t[35],wtr_h@data@max,w[35]))} else {wtr_h<-wtr_h}
    if(length(t)>34){ wtr_h <- reclassify(wtr_h, cbind( t[34],wtr_h@data@max,w[34]))} else {wtr_h<-wtr_h}
    if(length(t)>33){ wtr_h <- reclassify(wtr_h, cbind( t[33],wtr_h@data@max,w[33]))} else {wtr_h<-wtr_h}
    if(length(t)>32){ wtr_h <- reclassify(wtr_h, cbind( t[32],wtr_h@data@max,w[32]))} else {wtr_h<-wtr_h}
    if(length(t)>31){ wtr_h <- reclassify(wtr_h, cbind( t[31],wtr_h@data@max,w[31]))} else {wtr_h<-wtr_h}
    if(length(t)>30){ wtr_h <- reclassify(wtr_h, cbind( t[30],wtr_h@data@max,w[30]))} else {wtr_h<-wtr_h}
    if(length(t)>29){ wtr_h <- reclassify(wtr_h, cbind( t[29],wtr_h@data@max,w[29]))} else {wtr_h<-wtr_h}
    if(length(t)>28){ wtr_h <- reclassify(wtr_h, cbind( t[28],wtr_h@data@max,w[28]))} else {wtr_h<-wtr_h}
    if(length(t)>27){ wtr_h <- reclassify(wtr_h, cbind( t[27],wtr_h@data@max,w[27]))} else {wtr_h<-wtr_h}
    if(length(t)>26){ wtr_h <- reclassify(wtr_h, cbind( t[26],wtr_h@data@max,w[26]))} else {wtr_h<-wtr_h}
    if(length(t)>25){ wtr_h <- reclassify(wtr_h, cbind( t[25],wtr_h@data@max,w[25]))} else {wtr_h<-wtr_h}
    if(length(t)>24){ wtr_h <- reclassify(wtr_h, cbind( t[24],wtr_h@data@max,w[24]))} else {wtr_h<-wtr_h}
    if(length(t)>23){ wtr_h <- reclassify(wtr_h, cbind( t[23],wtr_h@data@max,w[23]))} else {wtr_h<-wtr_h}
    if(length(t)>22){ wtr_h <- reclassify(wtr_h, cbind( t[22],wtr_h@data@max,w[22]))} else {wtr_h<-wtr_h}
    if(length(t)>21){ wtr_h <- reclassify(wtr_h, cbind( t[21],wtr_h@data@max,w[21]))} else {wtr_h<-wtr_h}
    if(length(t)>20){ wtr_h <- reclassify(wtr_h, cbind( t[20],wtr_h@data@max,w[20]))} else {wtr_h<-wtr_h}
    if(length(t)>19){ wtr_h <- reclassify(wtr_h, cbind( t[19],wtr_h@data@max,w[19]))} else {wtr_h<-wtr_h}
    if(length(t)>18){ wtr_h <- reclassify(wtr_h, cbind( t[18],wtr_h@data@max,w[18]))} else {wtr_h<-wtr_h}
    if(length(t)>17){ wtr_h <- reclassify(wtr_h, cbind( t[17],wtr_h@data@max,w[17]))} else {wtr_h<-wtr_h}
    if(length(t)>16){ wtr_h <- reclassify(wtr_h, cbind( t[16],wtr_h@data@max,w[16]))} else {wtr_h<-wtr_h}
    if(length(t)>15){ wtr_h <- reclassify(wtr_h, cbind( t[15],wtr_h@data@max,w[15]))} else {wtr_h<-wtr_h}
    if(length(t)>14){ wtr_h <- reclassify(wtr_h, cbind( t[14],wtr_h@data@max,w[14]))} else {wtr_h<-wtr_h}
    if(length(t)>13){ wtr_h <- reclassify(wtr_h, cbind( t[13],wtr_h@data@max,w[13]))} else {wtr_h<-wtr_h}
    if(length(t)>12){ wtr_h <- reclassify(wtr_h, cbind( t[12],wtr_h@data@max,w[12]))} else {wtr_h<-wtr_h}
    if(length(t)>11){ wtr_h <- reclassify(wtr_h, cbind( t[11],wtr_h@data@max,w[11]))} else {wtr_h<-wtr_h}
    if(length(t)>10){ wtr_h <- reclassify(wtr_h, cbind( t[10],wtr_h@data@max,w[10]))} else {wtr_h<-wtr_h}
    if(length(t)>9){ wtr_h <- reclassify(wtr_h, cbind( t[9],wtr_h@data@max,w[9]))} else {wtr_h<-wtr_h}
    if(length(t)>8){ wtr_h <- reclassify(wtr_h, cbind( t[8],wtr_h@data@max,w[8]))} else {wtr_h<-wtr_h}
    if(length(t)>7){ wtr_h <- reclassify(wtr_h, cbind( t[7],wtr_h@data@max,w[7]))} else {wtr_h<-wtr_h}
    if(length(t)>6){ wtr_h <- reclassify(wtr_h, cbind( t[6],wtr_h@data@max,w[6]))} else {wtr_h<-wtr_h}
    if(length(t)>5){ wtr_h <- reclassify(wtr_h, cbind( t[5],wtr_h@data@max,w[5]))} else {wtr_h<-wtr_h}
    if(length(t)>4){ wtr_h <- reclassify(wtr_h, cbind( t[4],wtr_h@data@max,w[4]))} else {wtr_h<-wtr_h}
    if(length(t)>3){ wtr_h <- reclassify(wtr_h, cbind( t[3],wtr_h@data@max,w[3]))} else {wtr_h<-wtr_h}
    if(length(t)>2){ wtr_h <- reclassify(wtr_h, cbind( t[2],wtr_h@data@max,w[2]))} else {wtr_h<-wtr_h}
    if(length(t)>1){ wtr_h <- reclassify(wtr_h, cbind( t[1],wtr_h@data@max,w[1]))} else {wtr_h<-wtr_h}
    
    wtr_d<-wtr_h*24
    
    #turnover map###
    max=(round(wtr_d@data@max,digits=0)+10)
    min=ifelse((round((min(allout$x[allout$airT<=tempwtr@data@max])*24),digits=0)-5)<0,0,round((min(allout$x[allout$airT<=tempwtr@data@max])*24),digits=0)-5)
    
    breakpoints<-round(seq(min,max,length=10),digits = 0)
    my.at=breakpoints
    my.c.key <- list(at=breakpoints, labels=list(at=breakpoints, labels=my.at))
    myTheme=rasterTheme(region=(brewer.pal('Blues', n=9)),panel.background = list(col='white'))
    #myTheme=rasterTheme(region=rev(brewer.pal('RdBu', n=6)),panel.background = list(col='white'))
    
    turn.wtrmap<-levelplot(wtr_d, maxpixels=1e5, margin=F,par.settings=myTheme,at=breakpoints,colorkey=my.c.key,cex.axis=2, 
                           main=list("Water turnover (ml/day)",cex=1.5), 
                           pretty=TRUE)+layer(sp.polygons(mask), data=list(mask=mask))
    
    
    # water maps summer
    plotlist.wtr<-list(tnzplot,turn.wtrmap)
    
    #plot list####
    selection<-as.numeric(input$output)
    
    
    sum.plot<-plotlist.wtr[[selection]]
    
    
    if(input$lepishpInput==F){
      grid.arrange(sum.plot,ncol=2)}
    else{
      sum.plot<-sum.plot+ layer(sp.polygons(lepishp, col = lepishp@data$binomial),data=list(lepishp=lepishp))+
        layer(sp.text(coordinates(lepishp), txt = lepishp@data$labels, pos = 1, cex=1.3, font=4),data=list(lepishp=lepishp))
      
      grid.arrange(sum.plot,ncol=2)}
    
  })
  
  #Winterplot####
  output$winterplot<-renderPlot({ 
    
    category=0+as.numeric(input$sexInput) #summer is 3 and 4 (f and m) and dry is 1 and 2 f/m
    
    source("source_proto.R",keep.source = T,local = TRUE)
    
    #TNZ map####
    minimummlh=min(allout$mlO2ph)
    
    LCT=(min(allout$airT[allout$mlO2phL==minimummlh],na.rm=T))
    if(is.infinite(LCT)){LCT=5} else {LCT=LCT}
    if(LCT<5){LCT=5} else {LCT=LCT}
    UCT=(max(allout$airT[allout$mlO2phL==minimummlh],na.rm=T))
    if(is.infinite(UCT)){UCT=44} else {UCT=UCT}
    if(UCT>44){UCT=44} else {UCT=UCT}
    cold=max(allout$airT[allout$airT<=LCT& allout$mlO2phL>=minimummlh*1.25]) 
    if(is.infinite(cold)){cold=0} else {cold=cold}
    if(cold<0){cold=0} else {cold=cold}
    hot=max(allout$airT[allout$airT>UCT& allout$mlO2phL<=minimummlh*1.25])
    if(is.infinite(hot)){hot=46} else {hot=hot}
    if(hot>46){hot=46} else {hot=hot}
    warm=(((LCT+UCT)/2)+UCT)/2
    
    
    #this base layer stays the same throughout 3 and 4 for winter (avg and max)
    layer.win=2+as.numeric(input$templayer)
    temp <- subset(temp.brick,layer.win)
    temp<-temp+input$tempInput
    
    list<-c(cold,LCT,warm,UCT,hot)
    list=list[is.finite(list)]
    
    min=ifelse(temp@data@min<min(list),((round(temp@data@min,digits=0))-1),min(list)-1)
    max=ifelse(temp@data@max>max(list),((round(temp@data@max,digits=0))+1),max(list)+1)
    
    #tnzbreaks<-c(temp@data@min,cold,LCT,warm,UCT,hot,temp@data@max)
    tnzbreaks<-c(min,cold,LCT,warm,UCT,hot,max)
    tnznames=c(min, "Cold stress","Lower Critical Temp.","Warm","Upper Critical Temp.","Heat stress",max)
    num<-seq(1,7,by=1)
    tnzdata<-cbind.data.frame(num,tnznames,tnzbreaks)
    dat<-tnzdata
    dat$tnzbreaks[!is.finite(dat$tnzbreaks)] <- 0
    ###dat<-subset(dat,dat$tnzbreaks>0)
    dat<-subset(dat, !ave(dat$tnzbreaks, FUN = duplicated))
    dat <- dat[order(dat$tnzbreaks),]
    
    tnzbreak=as.numeric(dat$tnzbreaks)
    tnzmapTheme <- rasterTheme(region=c('#2166ac','#67a9cf','white','#fddbc7','#ef8a62','#b2182b')) # white ,'#f7f7f7' 
    tnz.at=as.character(tnzdata$tnznames)
    tnz.c.key <- list(at=tnzbreak, labels=list(at=tnzbreak, labels=tnz.at))
    
    tnzplot<-levelplot(temp,margin=F,maxpixels=1e5, cex.axis=2,par.settings=tnzmapTheme,at=tnzbreak,colorkey=tnz.c.key, 
                       main=list("L. leucopus Thermal Niche Map",cex=1.5))+ 
      layer(sp.polygons(mask),data=list(mask=mask))
    
    #water maps####
    
    tempwtr <- temp
    
    allout$x<-allout$wtr
    
    w<-seq((min(allout$x[allout$x>0])),(max(allout$x)+0.1),length=40)
    
    t=(c(min(allout$airT[allout$x<=w[1]],na.rm=T),(max(allout$airT[allout$x<w[2]],na.rm=T))
         ,(max(allout$airT[allout$x<w[3]],na.rm=T)),(max(allout$airT[allout$x<w[4]],na.rm=T))
         ,(max(allout$airT[allout$x<w[5]],na.rm=T)),(max(allout$airT[allout$x<w[6]],na.rm=T))
         ,(max(allout$airT[allout$x<w[7]],na.rm=T)),(max(allout$airT[allout$x<w[8]],na.rm=T))
         ,(max(allout$airT[allout$x<w[9]],na.rm=T)),(max(allout$airT[allout$x<w[10]],na.rm=T))
         ,(max(allout$airT[allout$x<w[11]],na.rm=T)),(max(allout$airT[allout$x<w[12]],na.rm=T))
         ,(max(allout$airT[allout$x<w[13]],na.rm=T)),(max(allout$airT[allout$x<w[14]],na.rm=T))
         ,(max(allout$airT[allout$x<w[15]],na.rm=T)),(max(allout$airT[allout$x<w[16]],na.rm=T))
         ,(max(allout$airT[allout$x<w[17]],na.rm=T)),(max(allout$airT[allout$x<w[18]],na.rm=T))
         ,(max(allout$airT[allout$x<w[19]],na.rm=T)),(max(allout$airT[allout$x<w[20]],na.rm=T))
         ,(max(allout$airT[allout$x<w[21]],na.rm=T)),(max(allout$airT[allout$x<w[22]],na.rm=T))
         ,(max(allout$airT[allout$x<w[23]],na.rm=T)),(max(allout$airT[allout$x<w[24]],na.rm=T))
         ,(max(allout$airT[allout$x<w[25]],na.rm=T)),(max(allout$airT[allout$x<w[26]],na.rm=T))
         ,(max(allout$airT[allout$x<w[27]],na.rm=T)),(max(allout$airT[allout$x<w[28]],na.rm=T))
         ,(max(allout$airT[allout$x<w[29]],na.rm=T)),(max(allout$airT[allout$x<w[30]],na.rm=T))
         ,(max(allout$airT[allout$x<w[31]],na.rm=T)),(max(allout$airT[allout$x<w[32]],na.rm=T))    
         ,(max(allout$airT[allout$x<w[33]],na.rm=T)),(max(allout$airT[allout$x<w[34]],na.rm=T))
         ,(max(allout$airT[allout$x<w[35]],na.rm=T)),(max(allout$airT[allout$x<w[36]],na.rm=T))
         ,(max(allout$airT[allout$x<w[37]],na.rm=T)),(max(allout$airT[allout$x<w[38]],na.rm=T))
         ,(max(allout$airT[allout$x<w[39]],na.rm=T)),(max(allout$airT[allout$x<w[40]],na.rm=T))
    ))
    wtr_dat<-cbind.data.frame(t,w)
    wtr_dat<-wtr_dat[is.finite(wtr_dat$t),]
    wtr_dat<-wtr_dat[!duplicated(wtr_dat$t),]
    
    wtr_dat1<-subset(wtr_dat,wtr_dat$t>tempwtr@data@min)
    wtr_dat1<-subset(wtr_dat1,wtr_dat1$t<=tempwtr@data@max)
    
    t<-wtr_dat1$t
    w<-wtr_dat1$w
    
    wtr_h <- reclassify(tempwtr, cbind(0, t[1], (min(allout$x[allout$x>0])))) #from min temp make minimum water loss
    if(length(t)>40){ wtr_h <- reclassify(wtr_h, cbind( t[40],wtr_h@data@max,w[40]))} else {wtr_h<-wtr_h}
    if(length(t)>39){ wtr_h <- reclassify(wtr_h, cbind( t[39],wtr_h@data@max,w[39]))} else {wtr_h<-wtr_h}
    if(length(t)>38){ wtr_h <- reclassify(wtr_h, cbind( t[38],wtr_h@data@max,w[38]))} else {wtr_h<-wtr_h}
    if(length(t)>37){ wtr_h <- reclassify(wtr_h, cbind( t[37],wtr_h@data@max,w[37]))} else {wtr_h<-wtr_h}
    if(length(t)>36){ wtr_h <- reclassify(wtr_h, cbind( t[36],wtr_h@data@max,w[36]))} else {wtr_h<-wtr_h}
    if(length(t)>35){ wtr_h <- reclassify(wtr_h, cbind( t[35],wtr_h@data@max,w[35]))} else {wtr_h<-wtr_h}
    if(length(t)>34){ wtr_h <- reclassify(wtr_h, cbind( t[34],wtr_h@data@max,w[34]))} else {wtr_h<-wtr_h}
    if(length(t)>33){ wtr_h <- reclassify(wtr_h, cbind( t[33],wtr_h@data@max,w[33]))} else {wtr_h<-wtr_h}
    if(length(t)>32){ wtr_h <- reclassify(wtr_h, cbind( t[32],wtr_h@data@max,w[32]))} else {wtr_h<-wtr_h}
    if(length(t)>31){ wtr_h <- reclassify(wtr_h, cbind( t[31],wtr_h@data@max,w[31]))} else {wtr_h<-wtr_h}
    if(length(t)>30){ wtr_h <- reclassify(wtr_h, cbind( t[30],wtr_h@data@max,w[30]))} else {wtr_h<-wtr_h}
    if(length(t)>29){ wtr_h <- reclassify(wtr_h, cbind( t[29],wtr_h@data@max,w[29]))} else {wtr_h<-wtr_h}
    if(length(t)>28){ wtr_h <- reclassify(wtr_h, cbind( t[28],wtr_h@data@max,w[28]))} else {wtr_h<-wtr_h}
    if(length(t)>27){ wtr_h <- reclassify(wtr_h, cbind( t[27],wtr_h@data@max,w[27]))} else {wtr_h<-wtr_h}
    if(length(t)>26){ wtr_h <- reclassify(wtr_h, cbind( t[26],wtr_h@data@max,w[26]))} else {wtr_h<-wtr_h}
    if(length(t)>25){ wtr_h <- reclassify(wtr_h, cbind( t[25],wtr_h@data@max,w[25]))} else {wtr_h<-wtr_h}
    if(length(t)>24){ wtr_h <- reclassify(wtr_h, cbind( t[24],wtr_h@data@max,w[24]))} else {wtr_h<-wtr_h}
    if(length(t)>23){ wtr_h <- reclassify(wtr_h, cbind( t[23],wtr_h@data@max,w[23]))} else {wtr_h<-wtr_h}
    if(length(t)>22){ wtr_h <- reclassify(wtr_h, cbind( t[22],wtr_h@data@max,w[22]))} else {wtr_h<-wtr_h}
    if(length(t)>21){ wtr_h <- reclassify(wtr_h, cbind( t[21],wtr_h@data@max,w[21]))} else {wtr_h<-wtr_h}
    if(length(t)>20){ wtr_h <- reclassify(wtr_h, cbind( t[20],wtr_h@data@max,w[20]))} else {wtr_h<-wtr_h}
    if(length(t)>19){ wtr_h <- reclassify(wtr_h, cbind( t[19],wtr_h@data@max,w[19]))} else {wtr_h<-wtr_h}
    if(length(t)>18){ wtr_h <- reclassify(wtr_h, cbind( t[18],wtr_h@data@max,w[18]))} else {wtr_h<-wtr_h}
    if(length(t)>17){ wtr_h <- reclassify(wtr_h, cbind( t[17],wtr_h@data@max,w[17]))} else {wtr_h<-wtr_h}
    if(length(t)>16){ wtr_h <- reclassify(wtr_h, cbind( t[16],wtr_h@data@max,w[16]))} else {wtr_h<-wtr_h}
    if(length(t)>15){ wtr_h <- reclassify(wtr_h, cbind( t[15],wtr_h@data@max,w[15]))} else {wtr_h<-wtr_h}
    if(length(t)>14){ wtr_h <- reclassify(wtr_h, cbind( t[14],wtr_h@data@max,w[14]))} else {wtr_h<-wtr_h}
    if(length(t)>13){ wtr_h <- reclassify(wtr_h, cbind( t[13],wtr_h@data@max,w[13]))} else {wtr_h<-wtr_h}
    if(length(t)>12){ wtr_h <- reclassify(wtr_h, cbind( t[12],wtr_h@data@max,w[12]))} else {wtr_h<-wtr_h}
    if(length(t)>11){ wtr_h <- reclassify(wtr_h, cbind( t[11],wtr_h@data@max,w[11]))} else {wtr_h<-wtr_h}
    if(length(t)>10){ wtr_h <- reclassify(wtr_h, cbind( t[10],wtr_h@data@max,w[10]))} else {wtr_h<-wtr_h}
    if(length(t)>9){ wtr_h <- reclassify(wtr_h, cbind( t[9],wtr_h@data@max,w[9]))} else {wtr_h<-wtr_h}
    if(length(t)>8){ wtr_h <- reclassify(wtr_h, cbind( t[8],wtr_h@data@max,w[8]))} else {wtr_h<-wtr_h}
    if(length(t)>7){ wtr_h <- reclassify(wtr_h, cbind( t[7],wtr_h@data@max,w[7]))} else {wtr_h<-wtr_h}
    if(length(t)>6){ wtr_h <- reclassify(wtr_h, cbind( t[6],wtr_h@data@max,w[6]))} else {wtr_h<-wtr_h}
    if(length(t)>5){ wtr_h <- reclassify(wtr_h, cbind( t[5],wtr_h@data@max,w[5]))} else {wtr_h<-wtr_h}
    if(length(t)>4){ wtr_h <- reclassify(wtr_h, cbind( t[4],wtr_h@data@max,w[4]))} else {wtr_h<-wtr_h}
    if(length(t)>3){ wtr_h <- reclassify(wtr_h, cbind( t[3],wtr_h@data@max,w[3]))} else {wtr_h<-wtr_h}
    if(length(t)>2){ wtr_h <- reclassify(wtr_h, cbind( t[2],wtr_h@data@max,w[2]))} else {wtr_h<-wtr_h}
    if(length(t)>1){ wtr_h <- reclassify(wtr_h, cbind( t[1],wtr_h@data@max,w[1]))} else {wtr_h<-wtr_h}
    
    wtr_d<-wtr_h*24
    
    #turnover map###
    max=(round(wtr_d@data@max,digits=0)+10)
    min=ifelse((round((min(allout$x[allout$airT<=tempwtr@data@max])*24),digits=0)-5)<0,0,round((min(allout$x[allout$airT<=tempwtr@data@max])*24),digits=0)-5)
    
    breakpoints<-round(seq(min,max,length=10),digits = 0)
    my.at=breakpoints
    my.c.key <- list(at=breakpoints, labels=list(at=breakpoints, labels=my.at))
    myTheme=rasterTheme(region=(brewer.pal('Blues', n=9)),panel.background = list(col='white'))
    
    turn.wtrmap<-levelplot(wtr_d, maxpixels=1e5, margin=F,par.settings=myTheme,at=breakpoints,colorkey=my.c.key,cex.axis=2, 
                           main=list("Water turnover (ml/day)",cex=1.5), 
                           pretty=TRUE)+layer(sp.polygons(mask), data=list(mask=mask))
    
    # water maps summer
    plotlist.wtr<-list(tnzplot,turn.wtrmap)
    
    #plot list####
    selection<-as.numeric(input$output)
    
    
    win.plot<-plotlist.wtr[[selection]]
    
    if(input$lepishpInput==F){
      grid.arrange(win.plot,ncol=2)}
    else{
      win.plot<-win.plot+ layer(sp.polygons(lepishp, col = lepishp@data$binomial),data=list(lepishp=lepishp))+
        layer(sp.text(coordinates(lepishp),txt = lepishp@data$labels, pos = 1, cex=1.3, font=4),data=list(lepishp=lepishp))
      
      grid.arrange(win.plot,ncol=2)}
    
    
  })
  
}


#end####
shinyApp(ui = ui, server = server)



