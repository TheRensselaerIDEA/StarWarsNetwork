if (!require("rlang")) {
  install.packages("rlang", dependencies = TRUE)
  library(rlang)
}

if (!require("httpuv")) {
  install.packages("httpuv", dependencies = TRUE)
  library(httpuv)
}

if (!require("shiny")) {
  install.packages("shiny", dependencies = TRUE)
  library(shiny)
}

if (!require("devtools")) {
  install.packages("devtools", dependencies = TRUE)
  library(devtools)
}

if (!require("mwshiny")) {
  devtools::install_git("https://github.com/delosh653/mwshiny")
  library(mwshiny)
}

if (!require("visNetwork")) {
  devtools::install_git("https://github.com/datastorm-open/visNetwork")
  library(visNetwork)
}

if (!require("readr")) {
  install.packages("readr", dependencies = TRUE)
  library(readr)
}

if (!require("htmlwidgets")) {
  install.packages("htmlwidgets", dependencies = TRUE)
  library(htmlwidgets)
}

if (!require("plyr")) {
  install.packages("plyr", dependencies = TRUE)
  library(plyr)
}

if (!require("dplyr")) {
  install.packages("dplyr", dependencies = TRUE)
  library(dplyr)
}

if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE)
  library(ggplot2)
}
if (!require("igraph")) {
  install.packages("igraph", dependencies = TRUE)
  library(igraph)
}
# Note sure what function we're using from Leaflet...
if (!require("leaflet")) {
  install.packages("leaflet", dependencies = TRUE)
  library(leaflet)
}

# Disable scientific notation
#options(scipen = 999)

# First-things first: load the data!
#sw_timeline <- read_csv("timelines.csv")  # Load from csv
sw_timeline <<- readRDS("sw_timeline.rds") # Load from rds

# Hannah's dependancy fixer
# Alloctate our named dependency list
#depend <- list()

# names of the list correspond to the package we want to import
# we give each of them the value of a vector of strings corresponding to the specific scripts we want to import
#depend[["htmlwidgets"]] <- c("www/htmlwidgets.js")
#depend[["visNetwork"]] <- c("htmlwidgets/lib/vis/vis.css", "htmlwidgets/lib/vis/vis.min.js", "htmlwidgets/visNetwork.js")

# vector of strings that are the names of my windows
win_titles <- c("Script","Floor", "Wall", "Character")

ui_win <- list()

# First we add the script view
ui_win[["Script"]] <-   fluidPage(
  htmlOutput("scriptzoom")
)    

# Then the Floor (Character networks)
ui_win[["Floor"]] <- fillPage(
#  titlePanel("Visnetwork Explorer: Network"),
  visNetworkOutput(outputId="network", width = "100%", height = "1000px")
)

# Then the Wall (the character timelines)
ui_win[["Wall"]] <- fluidPage(
  fluidRow(
    # In a plotOutput, passing values for click, dblclick, hover, or brush
    # will enable those interactions. (Only watch brush for StarWars)
    plotOutput("plot1", #height = 800,
               # Equivalent to: click = clickOpts(id = "plot_click")
               click = "plot_click",
               dblclick = dblclickOpts(
                 id = "plot_dblclick"
               ),
               hover = hoverOpts(
                 id = "plot_hover"
               ), 
               brush = brushOpts(
                 id = "plot_brush", fill = "#50DC14", direction = "y"
               )
    )
  )
)

# Finally the Charater detail
ui_win[["Character"]] <-   fluidPage(
  htmlOutput("character_info")
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
      new_timeline <- sw_timeline %>% dplyr::filter(between(timecode, input$plot_brush$ymin, input$plot_brush$ymax))
    } else {
      new_timeline <- sw_timeline %>% dplyr::filter(between(timecode, 3, 4))
    }

    new_timeline$timecode <- round(1000*new_timeline$timecode)
    new_timeline <- new_timeline %>% distinct()
    
    # determine all the nodes in the selection

    nodes <- new_timeline %>% dplyr::select(-timecode) %>% distinct()
    all_nodes <- sw_timeline  %>% dplyr::select(-timecode) %>% distinct() %>% arrange(id)
    
    all_nodes <- cbind(all_nodes, color= c("#f8766d","#de8c00","#b79f00","#7cae00","#00ba38","#00c08b",
                                           "#00bfc4","#00b4f0","#619cff","#c77cff","#f564e3","#ff64b0"))
    
    # Character images
    image <- as.character(c("http://orion.tw.rpi.edu/~olyerickson/swimages/node_01.png",  # ANAKIN
               "http://orion.tw.rpi.edu/~olyerickson/swimages/node_02.png",  # C-3PO
               "http://orion.tw.rpi.edu/~olyerickson/swimages/node_03.png",  # CHEWBACCA
               "http://orion.tw.rpi.edu/~olyerickson/swimages/node_04.png",  # DARTH VADER
               "http://orion.tw.rpi.edu/~olyerickson/swimages/node_05.png",  # EMPEROR
               "http://orion.tw.rpi.edu/~olyerickson/swimages/node_06.png",  # HAN
               "http://orion.tw.rpi.edu/~olyerickson/swimages/node_07.png",  # LEIA
               "http://orion.tw.rpi.edu/~olyerickson/swimages/node_08.png",  # LUKE
               "http://orion.tw.rpi.edu/~olyerickson/swimages/node_09.png",  # OBI-WAN
               "http://orion.tw.rpi.edu/~olyerickson/swimages/node_10.png",  # PADME
               "http://orion.tw.rpi.edu/~olyerickson/swimages/node_11.png",  # R2-D2
               "http://orion.tw.rpi.edu/~olyerickson/swimages/node_12.png"))  # YODA
    
    shape <- as.character(c("circularImage","circularImage","circularImage","circularImage",
                         "circularImage","circularImage","circularImage","circularImage",
                         "circularImage","circularImage","circularImage","circularImage") )
    
    # add image column to all_nodes
    all_nodes <- cbind(all_nodes, image)
    all_nodes <- cbind(all_nodes, shape)
    
    # TODO: Not elegant; try to do as function!
    edges_df <- data.frame(NULL)
    for (i in 1:length(new_timeline$timecode)) {
      tc_edge <- new_timeline %>% dplyr::filter(timecode==new_timeline$timecode[i])
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
      visNodes(fixed = TRUE) %>%
      visNodes(size = 50) %>%  # Default is 25
      visNodes(shapeProperties = list(useBorderWithImage = TRUE))  %>% 
      visEvents(selectNode = "function(nodes) {
        Shiny.onInputChange('current_node_id', nodes);
      ;}")
  })  
}

serv_out[["plot1"]] <- function(input, calc){
  renderPlot({
    
    #sw_timeline <- read_csv("timelines.csv")
    #sw_timeline <<- readRDS("sw_timeline.rds")
    sw_timeline$id <- factor(sw_timeline$character, labels=as.character(1:length(unique(sw_timeline$character))))
    # sw_timeline should now include title
    
    if(!is.null(input$plot_brush)) {
      intercept1 <- input$plot_brush$ymin
      intercept2 <- input$plot_brush$ymax
    } else {
      # Default selection region is Episode 4
      intercept1 <- 3
      intercept2 <- 4
    }
    
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
      geom_hline(aes(yintercept=intercept1), color = "yellow") + # Select region (min)
      geom_hline(aes(yintercept=intercept2), color = "yellow") + # Select region (max)
      ylab("EPISODE") # for the y axis label
    
  }, width="auto", height="auto")  
}

serv_out[["character_info"]] <- function(input, calc){
  renderUI({
    # This extracts the node number from the list returned by Shiny
    theNode <- as.integer(input$current_node_id[[1]])
    # Determine the character name    browser() 
    all_nodes <- sw_timeline  %>% dplyr::select(-timecode) %>% distinct() %>% arrange(id)
    character_url <- c("https://en.wikipedia.org/wiki/Anakin_Skywalker",   # ANAKIN
                       "https://en.wikipedia.org/wiki/C-3PO",              # C-3PO
                       "https://en.wikipedia.org/wiki/Chewbacca",          # CHEWBACCA
                       "https://en.wikipedia.org/wiki/Darth_Vader",        # DARTH VADER
                       "https://en.wikipedia.org/wiki/Palpatine",          # EMPEROR
                       "https://en.wikipedia.org/wiki/Han_Solo",           # HAN
                       "https://en.wikipedia.org/wiki/Princess_Leia",      # LEIA
                       "https://en.wikipedia.org/wiki/Luke_Skywalker",     # LUKE
                       "https://en.wikipedia.org/wiki/Obi-Wan_Kenobi",     # OBI-WAN
                       "https://en.wikipedia.org/wiki/Padm%C3%A9_Amidala", # PADME
                       "https://en.wikipedia.org/wiki/R2-D2",              # R2-D2
                       "https://en.wikipedia.org/wiki/Yoda")               # YODA
    all_nodes <- cbind(all_nodes, character_url)
    theUrl <- toString(all_nodes[theNode,]$character_url)
    # browser()
    if(theUrl != "") {
      redirectScript <- paste0("window = window.open('", theUrl, "');")
      tags$script(HTML(redirectScript))
    } else {
      # At startup we display a poster
      redirectScript <- paste0("window = window.open('", "https://docs.google.com/presentation/d/1EheLBbPsD0oTMSF-gRn9ulLzvJENClX9YOzZ9C08Cpc/present", "');")
      tags$script(HTML(redirectScript))
    }
    
  })
}

serv_out[["scriptzoom"]] <- function(input, calc){
  # Renders the script
  renderUI({
    scriptURL <- paste0("http://orion.tw.rpi.edu/~olyerickson/starwars_param.html?position=", 
                        format(toString(as.integer(input$plot_brush$ymin * 100000)), scientific=FALSE))
    
    redirectScript <- paste0("window = window.open('", scriptURL, "');")
    tags$script(HTML(redirectScript))
  })
  
} 

# NEW: Run with dependencies 
#mwsApp(win_titles, ui_win, serv_calc, serv_out, depend)
mwsApp(ui_win, serv_calc, serv_out)
