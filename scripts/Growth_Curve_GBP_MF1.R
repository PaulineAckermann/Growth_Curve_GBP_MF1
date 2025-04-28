# Load packages 
library(ggplot2)
library(here)

# Determine the directory of the script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Construct the path to the data file
data_file_path <- file.path(script_dir, "..", "data", "Growth and Induction.csv")

# Load data
data <- read.csv(data_file_path, sep = ";", dec = ",", header = TRUE)

# Subset rows 1 to 6 for GBP and 1 to 7 for MF1
df_gbp <- data[1:6, ]
df_mf1 <- data[1:7, ]

# Find the y-value (OD.MF1) at t = 3 for MF1
y_mf1_at_3 <- df_mf1$`OD.MF1`[df_mf1$`t.MF1` == 3]

# Create the Plot
p <- ggplot() +
  # First dataset (GBP) - Blue
  geom_point(data = df_gbp, aes(x = `t.GBP`, y = `OD.GBP`, color = "GBP"), size = 3) +
  geom_line(data = df_gbp, aes(x = `t.GBP`, y = `OD.GBP`, color = "GBP"), linewidth = 1) +
  # Second dataset (MF1) - Red
  geom_point(data = df_mf1, aes(x = `t.MF1`, y = `OD.MF1`, color = "MF 1"), size = 3) +
  geom_line(data = df_mf1, aes(x = `t.MF1`, y = `OD.MF1`, color = "MF 1"), linewidth = 1) +  
  # Manually defined colors and legend
  scale_color_manual(name = "Sample", values = c("GBP" = "blue", "MF 1" = "red")) +
  # horizontal lines for target range
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black") +
  annotate("text", x = max(c(df_gbp$`t.GBP`, df_mf1$`t.MF1`)), y = 2.6, 
           label = "Target range", hjust = 1, vjust = 9.0, size = 5) +
  annotate("text", x = 3, y = min(c(df_gbp$`OD.GBP`, df_mf1$`OD.MF1`)), 
           label = "MF 1 inoculation +60 mL", vjust = 0, hjust = -0.05, size = 5, color = "black") +
  geom_segment(aes(x = 3, xend = 3, y = 0, yend = y_mf1_at_3), color = "red", linetype = "dotted") +
  # IPTG induction labels
  annotate("text", x = 4.5, y = 1.244, 
           label = "Induction (IPTG)", hjust = -0.1, vjust = -0.8, size = 5, color = "blue") +
  annotate("text", x = 6.5, y = 0.619, 
           label = "Induction (IPTG)", hjust = 0.8, vjust = -0.8, size = 5, color = "red") +
  # Axis labels
  labs(x = "Time t [h]", y = "OD600") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.text = element_text(size = 14)
  )
# show plot
print(p)