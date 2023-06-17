# oscilloscope theme
theme_oscilloscope <- function(){
  theme(
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    plot.background = element_rect(fill = "#000000"),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5),
    panel.background = element_rect(fill = "#000000"),
    panel.grid.major = element_line(color = "#00FF00",
                                    size = 0.2, linetype = "dashed"),
    panel.grid.minor = element_line(color = "#00FF00",
                                    size = 0.1, linetype = "dashed"),
    legend.position = "none"
  )
}

# Generate example plot
# data <- data.frame(
#   time = seq(0, 1, length.out = 100),
#   voltage = sin(2 * pi * seq(0, 1, length.out = 100))
# )
#
# p <- ggplot(data, aes(x = time, y = voltage)) +
#   geom_line(color = "#00FF00", linewidth = 1) +
#   labs(title = "Oscilloscope-style Plot", subtitle = "Sinusoidal Waveform") +
#   oscilloscope_theme
#
# # Display the plot
# print(p)
