#### git clone git@github.com:Kolpashnikova/paperCAnD3.git ####

#### open the project files ####

#### Load the data ####
# data for 2015 GSS can be downloaded as PUMF from Statistics Canada
# through your institutional access

library(haven)

# load main data
data_gss_main <- read_dta("data/gss-89M0034-E-2015-c-29-main_F2.dta")

# load episode data
data_gss_episode <- read_dta("data/gss-89M0034-E-2015-c-29-episode_F2.dta")

## load packages that we will be working with
library(ggplot2)
library(tidyverse)
library(paletteer)
library(grid)
library(gridExtra)
library(graphics)
library(jpeg)

## load
library(devtools)
library(pak)
library(transformr)
library(gganimate)
library(RColorBrewer)
library(magick)
library(dplyr)
library(ggpubr)
#library(renv)
library(ggimage)

library(geojsonio)
library(rmapshaper)
library(rgdal)
library(sf)
library(ggtext)
library(ggrepel)
library(ggforce)

## let's find out which color palette we will use
palettes_d_names
palettes_d_names %>% distinct(package)

source("R/gss_longtempo.R")

## define time labels
t_intervals_labels <-  seq.POSIXt(as.POSIXct("2022-12-03 04:00:00 GMT"),
                                  as.POSIXct("2022-12-04 03:59:00 GMT"), by = "1 min")



groups <- c("15 to 24 years", "25 to 34 years", "35 to 44 years", "45 to 54 years",
            "55 to 64 years", "65 to 74 years", "75 years and over")

#### graph for women ####

df <- data.frame(Housework = numeric(),
                 t_interval = as.POSIXct(character()),
                 group = character())

for(gr in groups){
  n <- which(groups == gr)
  women <- data_gss_main[data_gss_main$AGEGR10 == n & data_gss_main$SEX == 2, ]
  episode <- data_gss_episode[data_gss_episode$PUMFID %in% women$PUMFID, ]

  women <- merge(episode, women, by = "PUMFID", all.x = TRUE)
  tem_women <- gss_longtempo(women)

  df_women <- as.data.frame(tem_women$values)
  names(df_women) <- tem_women$key

  df_women <- df_women / n_distinct(women$PUMFID) * 100

  ## add positions and time labels
  df_women$time <- 1:1440
  df_women$t_intervals_labels <- t_intervals_labels

  df_women <- subset(df_women, select = c(Housework,t_intervals_labels))

  df_women$group <- gr

  df <- rbind(df, df_women)

}

df_women <- df

#### non-animated plots combined for women ####

# Create an empty list to store plots
plot_list <- list()

# Loop through each group
for (i in seq_along(groups)) {
  current_group <- groups[i]

  # Filter the data for the current group
  current_data <- df_women %>%
    filter(group == current_group)

  # Create the plot for the current group
  current_plot <- ggplot(current_data, aes(x = t_intervals_labels, y = `Housework`)) +
    geom_area(fill = "#FF6666") +
    scale_x_datetime(labels = scales::date_format("%H:%M", tz = "EST"), position = "top") +
    scale_y_continuous(limits = c(0, 35)) +
    coord_flip() +
    labs(title = paste("Women:", current_group),
         x = "",
         y = "Percent") +
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey20", linetype = "dashed"),
          panel.grid.minor = element_line(color = "grey10", linetype = "dotted"),
          plot.title = element_text(hjust = 0, face = "bold", size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          plot.caption = element_text(hjust = 0, size = 14),
          legend.position = "none")

  # Save the current plot in the list
  plot_list[[paste0("plot", i)]] <- current_plot
}

#### graph for men ####

df <- data.frame(Housework = numeric(),
                 t_interval = as.POSIXct(character()),
                 group = character())

for(gr in groups){
  n <- which(groups == gr)
  men <- data_gss_main[data_gss_main$AGEGR10 == n & data_gss_main$SEX == 1, ]
  episode <- data_gss_episode[data_gss_episode$PUMFID %in% men$PUMFID, ]

  men <- merge(episode, men, by = "PUMFID", all.x = TRUE)
  tem_men <- gss_longtempo(men)

  df_men <- as.data.frame(tem_men$values)
  names(df_men) <- tem_men$key

  df_men <- df_men / n_distinct(men$PUMFID) * 100

  ## add positions and time labels
  df_men$time <- 1:1440
  df_men$t_intervals_labels <- t_intervals_labels

  df_men <- subset(df_men, select = c(Housework,t_intervals_labels))

  df_men$group <- gr

  df <- rbind(df, df_men)

}

df_men <- df

#### combined plots for men ####

# Create an empty list to store plots
plot_list_men <- list()

# Loop through each group
for (i in seq_along(groups)) {
  current_group <- groups[i]

  # Filter the data for the current group
  current_data <- df_men %>%
    filter(group == current_group)

  # Create the plot for the current group
  current_plot <- ggplot(current_data, aes(x = t_intervals_labels, y = `Housework`)) +
    geom_area(fill = paletteer_d("rcartocolor::Pastel",1)) +
    scale_x_datetime(labels = scales::date_format("%H:%M", tz="EST"))  +
    coord_flip() +
    scale_y_reverse(limits = c(35, 0)) + #puts y reverse
    labs(title = paste("Men   &"),
         x = "Time of Day",
         y = "Percent") +
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey20", linetype = "dashed"),
          panel.grid.minor = element_line(color = "grey10", linetype = "dotted"),
          plot.title = element_text(hjust = 1, face="bold", size = 14),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          plot.caption = element_text(hjust = 0, size = 14),
          legend.position = "none")

  # Save the current plot in the list
  plot_list_men[[paste0("plot", i)]] <- current_plot
}

# specify where to save the file
jpeg(filename = "output/combined_plot.jpg", width = 940, height = 1000, quality = 300)

## plot flipped graphs together
grid.arrange(plot_list_men$plot1, plot_list$plot1, plot_list_men$plot2, plot_list$plot2,
             plot_list_men$plot3, plot_list$plot3, plot_list_men$plot4, plot_list$plot4,
             plot_list_men$plot5, plot_list$plot5, plot_list_men$plot6, plot_list$plot6,
             plot_list_men$plot7, plot_list$plot7,
             ncol=4, top = textGrob("Housework Time",gp=gpar(fontsize=14,fontface = "bold")))


dev.off()
