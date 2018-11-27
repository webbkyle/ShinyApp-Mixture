#install.packages('shiny')
#install.packages('ggplot2')

library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram

split_comp<-function(N,m){
  t<-vector(mode="numeric")
  t=rep(floor(N/m),m)
  while(sum(t)!=N){
    r=sample(1:m,1)
    t[r]=t[r]+1
  }
  for(i in 1:N){
    r=sample(1:m,2)
    g=sample(1:m,2)
    q=runif(1)
    if(t[r[1]]>2 & t[r[2]]>2){
      t[r[1]]=t[r[1]]+1
      t[r[2]]=t[r[2]]-1
    }
    if(t[g[1]]>3 & t[g[2]]>3 & q>.5){
      t[g[1]]=t[g[1]]+2
      t[g[2]]=t[g[2]]-2
    }
  }
  return(t)
}

dataset <- function (n,mu,dev){
  comp <- length(mu)
  Comp <- 1:comp
  db <- vector(mode="numeric")
  obs <- 1:n
  comps <- vector(mode="numeric")
  t <- split_comp(n,comp)
  for(i in 1:comp){
    db=append(db,rnorm(t[i],mu[i],dev[i]))
    comps=append(comps,rep(Comp[i],t[i]))
  }
  db=data.frame(db,comps)
  db=db[sample(nrow(db),n),]
  return(data.frame(obs,db))
}


shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  rand_data <- eventReactive(input$rand, {
    Mu=sample(-2E6:2E7,input$components)
    Dev=runif(input$components,.5,1E6)
    cbind(Mu,Dev)
  })
  
  output$Mixture <- renderPlot({
    n <- as.numeric(input$N)
    current_data <- dataset(n,rand_data()[,1],rand_data()[,2])
    K <- ggplot(current_data,aes(1:n,current_data$db))+geom_point(aes(1:n,current_data$db,color=as.factor(current_data$comps)))+
      expand_limits(y=c(-2E6,2E7)) + ylab('Value') + scale_color_discrete(name = "Components")
    K
  })
  
  output$Table <- renderTable({
    Components <- 1:input$components 
    data.frame(Components,rand_data())
  });
  
})