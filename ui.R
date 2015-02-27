
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

points=6000

shinyUI(fluidPage(

  # Application title
  titlePanel("Spirograph for Max and Nathan"),
  
  #Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("ring1",
                  "Unit Size of Stationary Ring:",
                  min = 1,
                  max = 120,
                  value = 100),
      sliderInput("ring2",
                  "Unit Size of Rotating Ring:",
                  min = -100,
                  max = 120,
                  value = -30),
      sliderInput("penLoc",
                  "Pen relative to radius of ring 2:",
                  min = 0,
                  max = 50,
                  value = 20),
      radioButtons("penColor","Pen Color:",
                   c("Red"="red",
                     "Green" = "darkgreen",
                     "Blue" = "blue")),


      checkboxInput("showRings","Show Rings?",TRUE),      
      checkboxInput("zoom","Zoom? (won't always make a difference)",FALSE),

      sliderInput("point","Number of points drawn",min=2,max=points,value=2,step=5,
                  animate=animationOptions(interval=points/60))
      
#       selectInput("speed", "Animation Speed", c("slow", "medium", "fast")),
#       
#       conditionalPanel(
#         condition="input.speed == slow",
#           sliderInput("point","Number of points drawn",min=2,max=points,value=2,step=5,
#                   animate=animationOptions(interval=points/20))
#       )
#        conditionalPanel(
#         condition= "input.speed == medium",
#         sliderInput("point","Number of points drawn",min=2,max=points,value=2,step=5,
#                     animate=animationOptions(interval=points/50))
#       )
#       conditionalPanel(
#         condition= "input.speed == fast",
#         sliderInput("point","Number of points drawn",min=2,max=points,value=2,step=5,
#                     animate=animationOptions(interval=points/100))
#       )        
    ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("speed"),
      plotOutput("distPlot",width="400px",height="400px")
    )
  )
))
