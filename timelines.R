# Testing the occurances data structure
# Note required by app.R itself!

library(tidyverse)
library(ggplot2)
library(plotly)


# read in timelines test file
sw_timeline <- read_csv("timelines.csv")

# Basic violin plot
p <- ggplot(sw_timeline, aes(x=character, y=timecode)) + 
  geom_violin(adjust = .1, scale = "count", aes(fill = factor(character))) +
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

p
