CurrentYear <- 2025

# Define start and end years for the 5-year average
start_year <- CurrentYear - 5
end_year   <- CurrentYear - 1

df_wheat <- ch_data %>%
  select(Year, contains("Wheat_Production")) %>%
  filter(Year >= CurrentYear - 10 & Year <= CurrentYear) %>%
  as.data.frame()

df_wheat_mean_5yr <- df_wheat %>%
  filter(Year >= start_year & Year <= end_year) %>%
  summarise(Wheat_Production = mean(Wheat_Production, na.rm = TRUE))

Crop <- ggplot(df_wheat, aes(Year)) +
  # 5-year average line (2020–2024)
  geom_segment(
    data = df_wheat_mean_5yr,
    aes(y = Wheat_Production, yend = Wheat_Production,
        color = avg_label),
    x = start_year, xend = end_year, 
    size = 0.75
  )+
  # Wheat production
  geom_line(
    data = df_wheat,
    aes(y = Wheat_Production, color = "Wheat production"),
    size = 2, linetype = "solid"
  ) +
  
  # Current year point (excluded from legend)
  geom_point(
    data = subset(df_wheat, Year == CurrentYear),
    aes(y = Wheat_Production, color = "Wheat production"),
    size = 5,
    show.legend = FALSE
  ) +
  
  # Current year label (excluded from legend)
  geom_text(
    data = subset(df_wheat, Year == CurrentYear),
    aes(
      y = Wheat_Production,
      label = format((round(Wheat_Production, -3) / 1000), big.mark = ","),
      color = "Wheat production"
    ),
    size = 6, vjust = -1,
    show.legend = FALSE
  ) +
  
  # Make legend text same color as lines
  guides(
    color = guide_legend(
      override.aes = list(
        color = c("#00833E", "black")   # match legend key colors
      )
    )
  ) +
  # Manual legend colors
  scale_color_manual(
    name = NULL,
    values = c(
      "Wheat production" = "#00833E",
      "Five-year average (2020:2024)" = "black"
    ),
    breaks = c(
      "Wheat production",
      "Five-year average (2020:2024)"
    )
  ) +
  # Axis and theme
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 1250000), breaks = c(0, 250000, 500000, 750000, 1000000, 1250000)
  ) +
  scale_x_continuous(
    limits = xlimits, breaks = xbreaks, labels = xaxislabels
  ) +
  labs(y = "Thousand tonnes", x = "Year") +
  theme_set(theme_resas) +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title = element_text(size = 19),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    legend.box = "horizontal"
  )

Crop

ggsave(filename = paste0("CH_",CurrentYear,"_wheat_production.svg"), plot = Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = "retina", bg = "white")
