#### set up git with ssh ####

## checking updates, Git not working

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

#### animation ####

groups <- c("15 to 24 years", "25 to 34 years", "35 to 44 years", "45 to 54 years",
            "55 to 64 years", "65 to 74 years", "75 years and over")

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

p <- df %>%
  ggplot(aes(x = t_intervals_labels, y = `Housework`, group = group)) +
  geom_area(fill = "#FF6666") +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz="EST"), position = "top")  +
  scale_y_continuous(limits = c(0, 35)) +
  coord_flip() + # flips x and y axes
  labs(title = "",
       x = "Time of Day",
       y = "Percent") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey20", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey10", linetype = "dotted"),
        plot.title = element_text(hjust = 0, face="bold", size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.caption = element_text(hjust = 0, size = 14),
        legend.position = "none") +
  # Here comes the gganimate code
  transition_states(
    group
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')+
  labs(title = "Women: {closest_state}", x = "Time of Day", y = "Percent")


a <- animate(p, width = 240, height = 480,
             duration = 10)
f = "output/first.gif"
anim_save(f, animation = a)

#### animation for men ####

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

p <- df %>%
  ggplot(aes(x = t_intervals_labels, y = `Housework`, group = group)) +
  geom_area(fill = paletteer_d("rcartocolor::Pastel",1)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz="EST"))  +
  coord_flip() +
  scale_y_reverse(limits = c(35, 0)) + #puts y reverse
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey20", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey10", linetype = "dotted"),
        plot.title = element_text(hjust = 1, face="bold", size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.caption = element_text(hjust = 0, size = 14),
        legend.position = "none")+
  # Here comes the gganimate code
  transition_states(
    group
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')+
  labs(title = "{closest_state}: Men", x = "Time of Day", y = "Percent")

b <- animate(p, width = 240, height = 480,
             duration = 10)
f = "output/second.gif"
anim_save(f, animation = b)

#### combine animations ####

new_gif <- image_append(c(b[1], a[1]))
for(i in 2:100){
  combined <- image_append(c(b[i], a[i]))
  new_gif <- c(new_gif, combined)
}

new_gif

anim_save("output/combined.gif", animation = new_gif)
