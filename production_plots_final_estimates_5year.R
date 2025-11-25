## Code for producing the plots used in the cereals first estimates publication.
## Adapted from code used for the final ests. publication.

##NB: The svg plots will likely have some overlapping text issues; these are easier to sort manually in Inkscape than by
## fickering with the x/y coordinates in the code.

library(tidyverse)
library(readxl)
library(ggrepel)
library(ggplot2)
library(scales)
library(styler)
library(extrafont)

#Define the harvest year here
CurrentYear = 2025
#Set up graph parameters based on harvest year
# TenYearsAgo = CurrentYear-10
xlimits = c(CurrentYear-4, CurrentYear)
xbreaks = c((CurrentYear-4):CurrentYear)
xaxislabels = c(CurrentYear-4,
                CurrentYear-3,
                CurrentYear-2,
                CurrentYear-1,
                CurrentYear)


SVGWidth <- 225
SVGHeight <- 141

#Use the resas theme (modified to remove some grid lines)
resas_theme <- source("resas_theme_modified.R")
# setwd("")
ch_data <- read_csv("CH_data_final.csv")%>%
  filter(Year != 0)

production <- ch_data %>%
  select(c(Year, contains("production"))) %>%
  filter(Year > (CurrentYear-5))

df <- data.frame(production$Year, production$Cereals_Production) %>%
  setNames(c("Year", "Cereals_Production"))

df_mean <- df %>%
  select(c(contains("Production"))) %>%
  lapply(mean) %>%
  as.data.frame()

# Define start and end years for the 5-year average
start_year <- CurrentYear - 5
end_year   <- CurrentYear - 1

Crop <- ggplot(df, aes(Year)) +
  geom_hline(
    yintercept = df_mean$Cereals_Production, color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = CurrentYear - 3, y = 3300000, 
    label = paste0("Five year average (", start_year, ":", end_year, ")"),
    size = 6, color = "#575756"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000000, 
                                 prefix = "", 
                                 suffix = "", 
                                 accuracy = 1, 
                                 big.mark = ","), 
    limits = c(0, 3500000), breaks = c(0, 500000, 1500000, 2500000, 3500000)
  ) +
  scale_x_continuous(
    limits = xlimits, breaks = xbreaks, labels = xaxislabels
  ) + 
  geom_line(data = df,
            aes(y = Cereals_Production),
            color = "#00833E", size = 1.5, linetype = "solid"
  ) +
  geom_point(data = subset(df, Year == CurrentYear),
             aes(y = Cereals_Production),
             color = "#00833E", size = 5
  ) +
  geom_text(
    data = subset(df, Year == CurrentYear),
    aes(y = Cereals_Production, label = format((round(Cereals_Production, -3) / 1000), big.mark = ",")),
    size = 6, color = "#00833E", vjust = -1
  ) +
  annotate(
    "text",
    x = CurrentYear - 1, y = 2750000, label = "Total cereals production", size = 6, color = "#00833E"
  ) +
  labs(
    title = "", y = "Thousand tonnes", x = "Year"
  ) +
  theme_set(
    theme_resas
  ) +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title = element_text(size = 19),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


Crop

ggsave(filename = paste0("CH_",CurrentYear,"_cereals_production_5year.svg"), plot = Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = "retina", bg = "white")


####################################################################################################
# Alex - Barley Crop

df <- ch_data %>%
  select(c(Year, contains("Barley_Production"))) %>%
  filter(Year > (CurrentYear-5)) %>%
  as.data.frame()

df_mean <- df %>%
  select(c(contains("Production"))) %>%
  lapply(mean) %>%
  as.data.frame()

Crop <- ggplot(df, aes(Year)) +
  geom_hline(
    yintercept = df_mean$S_Barley_Production, color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = CurrentYear-2.75, y = 1750000, label = paste0("Five year average (", start_year, ":", end_year, ")"), size = 6, color = "#575756"
  ) +
  geom_hline(
    yintercept = df_mean$W_Barley_Production, color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = CurrentYear-3.5, y = 465000, label = paste0("Five year average (", start_year, ":", end_year, ")"), size = 6, color = "#575756"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 2000000), breaks = c(0, 500000, 1000000, 1500000, 2000000)
  ) +
  scale_x_continuous(
    limits = xlimits, breaks = xbreaks, labels = xaxislabels
  ) +
  geom_line(data=df,
            aes(y = S_Barley_Production),
            color = "#00833E", size = 1.75, linetype = "solid"
  ) +
  geom_point(data=subset(df, Year==CurrentYear),
             aes(y = S_Barley_Production),
             color = "#00833E", size = 5
  )+
  annotate(
    "text",
    x = CurrentYear, y = df$S_Barley_Production[df$Year==CurrentYear]+120000, 
    label = format((round(df$S_Barley_Production[df$Year==CurrentYear],-3)/1000), big.mark=","), 
    size = 6, color = "#00833E"
  )+
  annotate(
    "text",
    x = CurrentYear-1, y = 1500000, label = "Spring barley production", size = 6, color = "#00833E"
  ) +
  geom_line(data=df,
            aes(y = W_Barley_Production),
            color = "#00833E", size = 1.75, linetype = "solid"
  ) +  
  geom_point(data=subset(df, Year==CurrentYear),
             aes(y = W_Barley_Production),
             color = "#00833E", size = 5
  )+
  annotate(
    "text",
    x = CurrentYear, y = df$W_Barley_Production[df$Year==CurrentYear]+140000, 
    label = paste0(round(df$W_Barley_Production[df$Year==CurrentYear],-3)/1000), 
    size = 6, color = "#00833E"
  )+
  annotate(
    "text",
    x = CurrentYear-1.25, y =  260000, label = "Winter barley production", size = 6, color = "#00833E"
  ) +
  labs(
    title = "",  y = "Thousand tonnes", x = "Year"
  ) +
  theme_set(
    theme_resas
  ) +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title = element_text(size = 19),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

Crop

ggsave(filename = paste0("CH_",CurrentYear,"_barley_production_5year.svg"), plot = Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = 320, bg = "white")


####################################################################################################
# Alex - Oats Crop

df <- ch_data %>%
  select(c(Year, contains("Oats_Production"))) %>%
  filter(Year > (CurrentYear-5)) %>%
  as.data.frame()

df_mean <- df %>%
  select(c(contains("Production"))) %>%
  lapply(mean) %>%
  as.data.frame()

Crop <- ggplot(df, aes(Year)) +
  geom_hline(
    yintercept = df_mean$Oats_Production, color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = CurrentYear-3.5, y = 170000, label = paste0("Five year average (", start_year, ":", end_year, ")"), size = 6, color = "#575756"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 250000), breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
  ) +
  scale_x_continuous(
    limits = xlimits, breaks = xbreaks, labels = xaxislabels
  ) +
  geom_line(data=df,
            aes(y = Oats_Production),
            color = "#00833E", size = 1.75, linetype = "solid"
  ) +
  geom_point(data=subset(df, Year==CurrentYear),
             aes(y = Oats_Production),
             color = "#00833E", size = 5
  )+
  annotate(
    "text",
    x = CurrentYear, y = df$Oats_Production[df$Year==CurrentYear]-20000, 
    label = format((round(df$Oats_Production[df$Year==CurrentYear],-3)/1000), big.mark=","), 
    size = 6, color = "#00833E"
  )+
  annotate(
    "text",
    x = CurrentYear-1, y = 145000, label = "Oats production", size = 6, color = "#00833E"
  ) +
  labs(
    title = "",  y = "Thousand tonnes", x = "Year"
  ) +
  theme_set(
    theme_resas
  ) +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title = element_text(size = 19),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

Crop

ggsave(filename = paste0("CH_",CurrentYear,"_oats_production_5year.svg"), Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = "retina", bg = "white")

####################################################################################################
# Alex - Wheat Crop

df <- ch_data %>%
  select(c(Year, contains("Wheat_Production"))) %>%
  filter(Year > CurrentYear-5) %>%
  as.data.frame()

df_mean <- df %>%
  select(c(contains("Production"))) %>%
  lapply(mean) %>%
  as.data.frame()

Crop <- ggplot(df, aes(Year)) +
  geom_hline(
    yintercept = df_mean$Wheat_Production, color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = CurrentYear-3.5, y = 970000, label = paste0("Five year average (", start_year, ":", end_year, ")"),, size = 6, color = "#575756"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 1250000), breaks = c(0, 250000, 500000, 750000, 1000000, 1250000)
  ) +
  scale_x_continuous(
    limits = xlimits, breaks = xbreaks, labels = xaxislabels
  ) +
  geom_line(data=df,
            aes(y = Wheat_Production), 
            color = "#00833E", size = 1.75, linetype = "solid",
  ) +
  geom_point(data=subset(df, Year==CurrentYear),
             aes(y = Wheat_Production),
             color = "#00833E", size = 5
  )+
  annotate(
    "text",
    x = CurrentYear, y = df$Wheat_Production[df$Year==CurrentYear]-70000, 
    label = format((round(df$Wheat_Production[df$Year==CurrentYear],-3)/1000), big.mark=","), 
    size = 6, color = "#00833E"
  )+
  annotate(
    "text",
    x = CurrentYear-1.5, y = 1085000, label = "Wheat production", size = 6, color = "#00833E"
  ) +
  labs(
    title = "",  y = "Thousand tonnes", x = "Year"
  ) +
  theme_set(
    theme_resas
  ) +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title = element_text(size = 19),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

Crop

ggsave(filename = paste0("CH_",CurrentYear,"_wheat_production_5year.svg"), plot = Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = "retina", bg = "white")


####################################################################################################
# Alex - OSR Crop

df <- ch_data %>%
  select(c(Year, contains("OSR_Production"))) %>%
  filter(Year > CurrentYear-5) %>%
  as.data.frame()

df_mean <- df %>%
  select(c(contains("Production"))) %>%
  lapply(mean) %>%
  as.data.frame()


Crop <- ggplot(df, aes(Year)) +
  geom_hline(
    yintercept = df_mean$OSR_Production, color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = CurrentYear-3.5, y = 150000, label = paste0("Five year average (", start_year, ":", end_year, ")"), size = 6, color = "#575756"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 200000)
  ) +
  scale_x_continuous(
    limits = xlimits , breaks = xbreaks, labels = xaxislabels
  ) +
  geom_line(data=df,
            aes(y = OSR_Production),
            color = "#00833E", size = 1.75, linetype = "solid"
  ) +
  geom_point(data=subset(df, Year==CurrentYear),
             aes(y = OSR_Production),
             color = "#00833E", size = 5
  )+
  annotate(
    "text",
    x = CurrentYear, y = df$OSR_Production[df$Year==CurrentYear]+14000, 
    label = format((round(df$OSR_Production[df$Year==CurrentYear],-3)/1000), big.mark=","), 
    size = 6, color = "#00833E"
  )+
  annotate(
    "text",
    x = CurrentYear-1, y = 180000, label = "Oilseed rape production", size = 6, color = "#00833E"
  ) +
  labs(
    title = "", y = "Thousand tonnes", x = "Year"
  ) +
  theme_set(
    theme_resas
  ) +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title = element_text(size = 19),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

Crop

ggsave(filename = paste0("CH_",CurrentYear,"_osr_production_5year.svg"), plot = Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = "retina", bg = "white")



