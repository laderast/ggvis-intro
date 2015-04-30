#iris_example
#server.R
require(shiny)
require(dplyr)
require(ggvis)
require(reshape2)

data(iris)
iris <- data.frame(ID=rownames(iris), iris)
iris2 <- melt(iris, ID=ID)

iris_tooltip <- function(x){
  if(is.null(x)){return(NULL)}
  if(is.null(x$ID)){return(NULL)}
  g <- iris[as.numeric(x$ID),]
  paste0("<b>Information for data point </b>", g$ID, "<br>",
         "<b>Species: </b>", g$Species
  )
}  
  
if(0){
iris2 %>% ggvis(x=~variable, y=~ID, fill=~value) %>%
  layer_rects(width=band(), height=band()) %>%
  scale_ordinal("x", padding = 0, points=FALSE) %>%
  scale_ordinal("y", padding = 0, points=FALSE, domain = c(1:nrow(iris))) %>%
  add_tooltip(iris_tooltip, "hover")
}

shinyServer(function(input, output) {
  
  clusters <- reactive({
    #select distance metric
    if(input$distance == "euclidean"){
      distMat <- dist(iris[,-c(1,6)])
    }
    
    if(input$distance == "correlation"){
      distMat <- as.dist(1-cor(t(iris[,-c(1,6)])))
    }
    
    #run clustering algorithm and cut tree to number of clusters
    clusts <- cutree(hclust(distMat), k = input$clusters)    
    clusts 
  })
  
  output$table <- renderDataTable({
    #count the number of flowers in each cluster
    data.frame(table(clusters()))
  })
  
  clustPlot <- reactive({
    
    colPal <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
    #make cluster assignments compatible with melted iris data frame
    clusts <- as.character(rep(clusters(),4))
    
    #actual plot
    data.frame(iris2, clusts) %>% 
      #map properties
      ggvis(x=~variable, y=~value) %>%
      #group by ID (so each flower is represented by a line)
      group_by(ID) %>%
      #add tooltip functionality (lets us see the species for a line)
      add_tooltip(iris_tooltip, "hover") %>%
      #specify that we want a line and color by cluster assingment
      layer_lines(stroke=~clusts) %>%
      #specify the mapping
      scale_ordinal("stroke", domain=as.character(c(1:9)), range=colPal)
      
  }) 
  
  clustPlot %>% bind_shiny("plot2")
  
  
})