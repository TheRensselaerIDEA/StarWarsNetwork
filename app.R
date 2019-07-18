
if (!require("httpuv")) {
  install.packages("https://cran.r-project.org/src/contrib/httpuv_1.5.1.tar.gz", repo=NULL, type="source")
  library(httpuv)
}

if (!require("shiny")) {
  install.packages("https://cran.r-project.org/src/contrib/shiny_1.3.2.tar.gz", repo=NULL, type="source")
  library(shiny)
}

if (!require("mwshiny")) {
  install.packages("https://cran.r-project.org/src/contrib/mwshiny_0.1.0.tar.gz", repo=NULL, type="source")
  library(mwshiny)
}

if (!require("visNetwork")) {
  devtools::install_git("https://github.com/datastorm-open/visNetwork")
  library(tidyverse)
}

if (!require("readr")) {
  install.packages("readr", dependencies = TRUE)
  library(readr)
}

if (!require("htmlwidgets")) {
  install.packages("htmlwidgets", dependencies = TRUE)
  library(htmlwidgets)
}
if (!require("tidyverse")) {
  install.packages("tidyverse", dependencies = TRUE)
  library(tidyverse)
}
if (!require("plyr")) {
  install.packages("plyr", dependencies = TRUE)
  library(plyr)
}
if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE)
  library(ggplot2)
}
if (!require("htmlwidgets")) {
  install.packages("htmlwidgets", dependencies = TRUE)
  library(htmlwidgets)
}
if (!require("igraph")) {
  install.packages("igraph", dependencies = TRUE)
  library(igraph)
}
if (!require("leaflet")) {
  install.packages("leaflet", dependencies = TRUE)
  library(leaflet)
}

# Hannah's dependancy fixer
# Alloctate our named dependency list
depend <- list()

# names of the list correspond to the package we want to import
# we give each of them the value of a vector of strings corresponding to the specific scripts we want to import
depend[["htmlwidgets"]] <- c("www/htmlwidgets.js")
depend[["visNetwork"]] <- c("htmlwidgets/lib/vis/vis.css", "htmlwidgets/lib/vis/vis.min.js", "htmlwidgets/visNetwork.js")

# vector of strings that are the names of my windows
win_titles <- c("Controller","Floor", "Wall")

ui_win <- list()

# first we add what we want to see in the controller to the list
ui_win[[1]] <- fluidPage(
  titlePanel("StarWars Network Explorer: Controller")
)

# then we add what we want to see in the Floor section
ui_win[[2]] <- fillPage(
#  titlePanel("Visnetwork Explorer: Network"),
  visNetworkOutput(outputId="network", width = "100%", height = "900px")
)

ui_win[[3]] <- fluidPage(
  fluidRow(
    # In a plotOutput, passing values for click, dblclick, hover, or brush
    # will enable those interactions. (Only watch brush for StarWars)
    plotOutput("plot1", height = 800,
               # Equivalent to: click = clickOpts(id = "plot_click")
               click = "plot_click",
               dblclick = dblclickOpts(
                 id = "plot_dblclick"
               ),
               hover = hoverOpts(
                 id = "plot_hover"
               ), 
               brush = brushOpts(
                 id = "plot_brush"
               )
    )
  )
)

# setting up the list of calculations I want to do
serv_calc <- list()

# Not sure what goes here for this example
serv_calc[[1]] <- function(input,calc){

}

serv_out <- list()

# Here we render our network
# note the name is the same as the outputid
serv_out[["network"]] <- function(input, calc){
  renderVisNetwork({
    
    # filter the data based on the selection
    # NEW: If user hasn't made a selection, choose Episode 4 (the original Star Wars)
    if(!is.null(input$plot_brush)) {
         new_timeline <- sw_timeline %>% filter(between(timecode, input$plot_brush$ymin, input$plot_brush$ymax))
      } else {
         new_timeline <- sw_timeline %>% filter(between(timecode, 3, 4))
      }
    
    new_timeline$timecode <- round(1000*new_timeline$timecode)
    new_timeline <- new_timeline %>% distinct()
    
    # determine all the nodes in the selection
    nodes <- new_timeline %>% select(-timecode) %>% distinct()
    all_nodes <- sw_timeline  %>% select(-timecode) %>% distinct() %>% arrange(id)
    
    all_nodes <- cbind(all_nodes, color= c("#f8766d","#de8c00","#b79f00","#7cae00","#00ba38","#00c08b",
                                           "#00bfc4","#00b4f0","#619cff","#c77cff","#f564e3","#ff64b0"))
    
    # TODO: Not elegant; try to do as function!
    edges_df <- data.frame(NULL)
    for (i in 1:length(new_timeline$timecode)) {
      tc_edge <- new_timeline %>% filter(timecode==new_timeline$timecode[i])
      if (nrow(tc_edge) > 1) {
        edges_df <- rbind(edges_df,t(combn(as.numeric(tc_edge$id), 2)))
      }
    }
    colnames(edges_df) <- c("from","to")
    
    # edges_df now has all the edges. Now figure out counts (weights) and figure out final list
    # creates new column `n`
    edges_df <- edges_df %>% add_count(from, to) 
    colnames(edges_df) <- c("from","to", "value")
    
    # Set node positions
    setNodeLocations <- function(nodes, numNodes, radius, center_x, center_y) {
      slice <- 2 * pi / numNodes
      # The +2 is needed to align Node 1 with 6 O'Clock
      angle <- slice * seq(1+2, numNodes+2, by = 1) 
      
      # Need to rotate by 1/2 slice further
      angle <- angle + slice/2
      
      nodes$x <- center_x + radius * cos(angle)
      nodes$y <- center_y + radius * sin(angle)
      
      return(nodes)
    }
    all_nodes <- setNodeLocations(all_nodes,nrow(all_nodes),1000,0,0)
    
    visNetwork(all_nodes, edges_df, background="black") %>%
      visEdges(color = list(color = "orange")) %>%
      visNodes(fixed = TRUE)
  })  
}

serv_out[["plot1"]] <- function(input, calc){
  renderPlot({
    
    #sw_timeline <- read_csv("timelines.csv")
    sw_timeline <<- readRDS("sw_timeline.rds")
    sw_timeline$id <- factor(sw_timeline$character, labels=as.character(1:length(unique(sw_timeline$character))))
    # sw_timeline should now include title
    
    # Basic violin plot
    ggplot(sw_timeline, aes(x=character, y=timecode)) + 
      geom_violin(adjust = .1, scale = "count", aes(fill = factor(id))) +
      theme(
        panel.background = element_rect(fill = "black",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) +
      theme(plot.background = element_rect(fill = "black")) +
      theme(legend.position = "none") +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "white",face="bold"),
            axis.text.x = element_text(colour = "white",face="bold"),
            axis.text.y = element_text(colour = "white")) + 
      scale_y_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7)) + 
      ylab("EPISODE") # for the y axis label
    
  })  
}

serv_out[["brush_info"]] <- function(input, calc){
  renderPrint({
    cat("input$plot_brush$ymin: ")
    str(input$plot_brush$ymin)
    cat("input$plot_brush$ymax: ")
    str(input$plot_brush$ymax)
  })
}

# NEW: Run with dependencies 
mwsApp(win_titles, ui_win, serv_calc, serv_out, depend)
