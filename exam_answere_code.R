
# Required packages
required_packages <- c("tidyverse", "readxl", "viridisLite", "scales", "ggplot2")

# Install required packages if not already installed
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


# Set the file path to get data 
file_path <- "Data/Dataset_S1.csv"

# Read the CSV file
data <- read_delim(file_path, col_types = cols(.default = "character"), delim = ",")

####I want to create the Figure 3, with biomass and eveness, using dissimilarity. 

#Converting variables to numeric so that it works for the plot 
data$evesimpson <- as.numeric(data$evesimpson)
data$jac <- as.numeric(data$jac)
data$biomass <- as.numeric(data$biomass) 

#Plotting the scatterplot 
plot <- ggplot(data, aes(x = evesimpson, y = biomass/1000000, color = jac)) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(0.05, 0.1, 0.2)) +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.00, 0.05, 0.5, 2.5),
    labels = c(0.00, 0.05, 0.5, 2.5)
  ) +
  labs(color = "Dissimilarity") +
  scale_color_gradientn(
    colors = colorRampPalette(c("blue", "beige", "brown"))(6),
    breaks = levels(data$jac),
    labels = levels(data$jac)
  ) +
  xlab("evenness") +
  ylab("biomass") +
  theme(    # Axis labels and theme customization
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 1),
    axis.line.x = element_line(arrow = arrow(type = "open", ends = "last", length = unit(0.5, "cm"), angle = 20)),
    axis.line.y = element_line(arrow = arrow(type = "open", ends = "last", length = unit(0.5, "cm"), angle = 20)),
    axis.text = element_text(size = 12),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "bottom",
    legend.box = "horizontal"
  ) +
  guides(color = guide_colorbar(title = "Dissimilarity",     # Customize color legend guide
                                direction = "horizontal",
                                barwidth = 10, 
                                barheight = 1))


# Display the plot
print(plot)
