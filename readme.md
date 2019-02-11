Untitled
================

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

``` r
  ggplotcTrochoid_A<-function(radius1=100,radius2=20,
                            penLoc=5,point=defaultPoints,
                            overlay=TRUE,penColor="black",zoom=FALSE) {
    
    
    if (radius2>0) {
      # rotating ring goes outside stationary ring
      figureType ='Epitrochoid'
      if (radius1==radius2)  figureType="Epicycloid (stationary Ring=Rotating Ring)"
      if (penLoc==radius2)  figureType="Limaçon (Pen Size = Rotating Ring)"      
      if (penLoc==0)  figureType="Circle (Pen Size = 0)"      
      dat<-epiTrochoid(radius1,radius2,penLoc,defaultTheta)
    }
    else {
      # rotating ring goes inside stationary ring
      figureType = 'Hypotrochoid'
      if (radius1==radius2)  figureType="Ellipse (stationary Ring = 2 * Rotating Ring)"
      if (penLoc==radius2)  figureType="Hypocycloid (Pen Size = Rotating Ring)"      
      
      dat<-hypoTrochoid(radius1,radius2,penLoc,defaultTheta)
    } 
    # are these special cases?
    
    
    # zoom might clip ring visibility but make figure fill plot area
    if (zoom)
      maxRange= max(abs(dat))
    else
      maxRange=radius1+abs(radius2)+penLoc
    
    plotRange=c(-maxRange,maxRange)
    
    t.plot<-ggplot(data=dat[1:point,],aes(x=x,y=y))+geom_path(color=penColor)+xlim(plotRange)+ylim(plotRange)
    #plot rotating ring and 'pen' for last point
    if (overlay) t.plot <- ggoverlayCircles(t.plot,radius1,radius2,
                                            px =dat[point,]$x,
                                            py =dat[point,]$y,
                                            cx =dat[point,]$cx,
                                            cy =dat[point,]$cy,
                                            penColor)
    print(t.plot)
  } 
```
