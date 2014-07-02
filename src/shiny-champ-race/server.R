library(shiny)
library(ggplot2)
library(reshape)
 
# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  points=data.frame(pos=1:11,val=c(25,18,15,12,10,8,6,4,2,1,0))
  points[[1,2]]
  a=245
  v=255
   
  pospoints=function(a,v,pdiff,points){
    pp=matrix(ncol = nrow(points), nrow = nrow(points))
    for (i in 1:nrow(points)){
      for (j in 1:nrow(points))
        pp[[i,j]]=v-a+pdiff[[i,j]]
    }
    pp
  }
   
  pdiff=matrix(ncol = nrow(points), nrow = nrow(points))
  for (i in 1:nrow(points)){
    for (j in 1:nrow(points))
      pdiff[[i,j]]=points[[i,2]]-points[[j,2]]
  }
   
  ppx=pospoints(a,v,pdiff,points)
   
  winmdiff=function(vadiff,pdiff,points){
    win=matrix(ncol = nrow(points), nrow = nrow(points))
    for (i in 1:nrow(points)){
      for (j in 1:nrow(points))
        if (i==j) win[[i,j]]=''
        else if ((vadiff+pdiff[[i,j]])>=0) win[[i,j]]='VET'
        else win[[i,j]]='ALO'
    }
    win
  }
   
  # Function that generates a plot of the distribution. The function
  # is wrapped in a call to reactivePlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  output$distPlot <- renderPlot({
    wmd=winmdiff(ppx[[input$vet,input$alo]],pdiff,points)
    wmdm=melt(wmd)
    g=ggplot(wmdm)+geom_text(aes(X1,X2,label=value,col=value))
    g=g+xlab('VET position in Brazil')+ ylab('ALO position in Brazil')
    g=g+labs(title="Championship outcomes in Brazil")
    g=g+ theme(legend.position="none")
    g=g+scale_x_continuous(breaks=seq(1, 11, 1))+scale_y_continuous(breaks=seq(1, 11, 1))
    print(g)
  })
})