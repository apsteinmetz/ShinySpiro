
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  #http://www.r-bloggers.com/ggplot2-version-of-figures-in-%E2%80%9Clattice-multivariate-data-visualization-with-r%E2%80%9D-part-12/
  
  library(plotrix)
  library(ggplot2)
  library(shape)
  
  debug=TRUE
  # Spirograph
  
  # angle in radians
  # draw 2*pi cycles with a point every 1/density radians
  calcCycles=100
  density =100
  defaultTheta <- 2 * pi * seq(0, calcCycles, by = 1/density)
  
circle <- function(cx=0,cy=0,radius = 1, npoints = 100){
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- cx + radius * cos(tt)
    yy <- cy + radius * sin(tt)
    return(data.frame(x = xx, y = yy))
}  
ggoverlayCircles<-function(radius1,radius2,penLoc){
    #uses Plotrix
    c1<-ggplot(data=circle(0,0,radius1),aes(x,y))+geom_path()
    c2<-ggplot(data=circle(radius1+radius2,0,abs(radius2)),aes(x,y))+geom_path()
    # l1<- c(x=c(radius1+radius2,radius1+radius2-penLoc*sign(radius2)),y=c(0,0))
    l1<- geom_segment(aes(x = radius1+radius2, y = 0, xend = radius1+radius2-penLoc*sign(radius2), yend = 0), arrow = arrow(length = unit(0.5, "cm")))
    return (c1+c2+l1)
}

  shapeoverlayCircles<-function(radius1,radius2,penLoc){
    #uses Plotrix
    draw.circle(0,0,radius1,lwd=2,border="red")
    draw.circle(radius1+radius2,0,abs(radius2),lwd=2,border="red")
    # lines(x=c(radius1+radius2,radius1+radius2-penLoc*sign(radius2)),y=c(0,0),lwd=2,col="green")
    Arrows(x0=radius1+radius2,x1=radius1+radius2-penLoc*sign(radius2),
           y0=0,y1=0,lwd=2,col="blue",arr.adj=1)  
  }

  hypoTrochoid <-function(r1=100,r2=-10,penLoc=10,theta=defaultTheta)  {
    #cyclic Trochoid
    #http://en.wikipedia.org/wiki/Hypotrochoid
    # in this convention we denote hypo- byt a negative r2 so flip this sign for the usual formulation
    r2 = -r2
    
    x <-(r1-r2)*cos(theta) + penLoc*cos((r1-r2)/r2*theta)
    y <-(r1-r2)*sin(theta) - penLoc*sin((r1-r2)/r2*theta)
    return(data.frame(x,y))
  }
  
  epiTrochoid <-function(r1=100,r2=10,penLoc=10,theta=defaultTheta)  {
    #cyclic Trochoid
    #http://en.wikipedia.org/wiki/Hypotrochoid

    x <-(r1+r2)*cos(theta)-penLoc*cos((r1+r2)/r2*theta)
    y <-(r1+r2)*sin(theta)-penLoc*sin((r1+r2)/r2*theta)
    return(data.frame(x,y))
  }
  
  plotRandomcTrochoid<-function() {
    radius1=round(runif(1,1,100))
    radius2=round(runif(1,-50,50))
    penLoc=round(radius2*runif(1,0,2))
    dat<-cTrochoid(radius1,radius2,penLoc,defaultTheta)
    plot(dat$x,dat$y,type='l',xlab='x',ylab='y')
    sub=paste('Big Circle=',radius1,'Small Circle=',radius2,'Pen Point=',penLoc)
    if (radius2>0)
      main='Epitrochoid'
    else
      main='Hypotrochoid'
    
    title(main=main,sub=sub)
  }
  
  
  plotcTrochoid<-function(radius1=100,radius2=10,penLoc=5,overlay=TRUE) {
    if (radius2>0)
      dat<-epiTrochoid(radius1,radius2,penLoc,defaultTheta)
    else
      dat<-hypoTrochoid(radius1,radius2,penLoc,defaultTheta)
    plotRange=c(-max(abs(dat)),max(abs(dat)))
    plot(dat$x,dat$y,type='l',xlab='x',ylab='y',asp=1,xlim=plotRange,ylim=plotRange)
    sub=paste('Big Circle=',radius1,'Small Circle=',radius2,'Pen Point=',penLoc)
    if (radius2>0)
      main='Epitrochoid'
    else
      main='Hypotrochoid'
    
    title(main=main,sub=sub)
    if (overlay) overlayCircles(radius1,radius2,penLoc)
  }
  
  ggplotcTrochoid<-function(radius1=100,radius2=10,penLoc=5,overlay=TRUE) {
    if (radius2>0)
      dat<-epiTrochoid(radius1,radius2,penLoc,defaultTheta)
    else
      dat<-hypoTrochoid(radius1,radius2,penLoc,defaultTheta)
    plotRange=c(-max(abs(dat)),max(abs(dat)))
    t.plot <- ggplot(data=dat,aes(x,y))+geom_path()+xlim(plotRange)+ylim(plotRange)
    #,xlab='x',ylab='y',asp=1,xlim=plotRange,ylim=plotRange)
    sub=paste('Big Circle=',radius1,'Small Circle=',radius2,'Pen Point=',penLoc)
    if (radius2>0)
      main='Epitrochoid'
    else
      main='Hypotrochoid'
    
    title(main=main,sub=sub)
    if (overlay) ggoverlayCircles(radius1,radius2,penLoc)
    # now plot it
    print(t.plot)
  }
  
  

animatecTrochoid<-function(radius1=100,radius2=20,penLoc=5,point=length(defaultTheta),overlay=TRUE) {

    
    if (radius2>0)
      dat<-epiTrochoid(radius1,radius2,penLoc,defaultTheta)
    else
      dat<-hypoTrochoid(radius1,radius2,penLoc,defaultTheta)
    if (radius2>0)
      main='Epitrochoid'
    else
      main='Hypotrochoid'
    plotRange=c(-max(abs(dat)),max(abs(dat)))
    # plot(dat[1,],type='l',xlab='x',ylab='y',asp=1,xlim=plotRange,ylim=plotRange)
    sub=paste('Big Circle=',radius1,'Small Circle=',radius2,'Pen Location=',penLoc)
    # title(main=main,sub=sub)
    if (debug) print(paste(point))
    t.plot<-ggplot(data=dat[1:point,],aes(x=x,y=y))+geom_path()+xlim(plotRange)+ylim(plotRange)
    print(t.plot)
    #plot(dat[1:point,],type='l',xlab='x',ylab='y',asp=1,xlim=plotRange,ylim=plotRange)
    if (overlay) 
      print(t.plot+ggoverlayCircles(radius1,radius2,penLoc))
    else
      print(t.plot)
  } 
  
output$distPlot <- renderPlot({
 
    animatecTrochoid(radius1=input$ring1,
                     radius2=input$ring2,
                     penLoc=input$penLoc,
                     point=input$point,
                     overlay=input$showRings)
  })

})
