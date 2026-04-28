
library(ggplot2)
library(tidyverse)
library(here)
library(writexl)


data <- tibble(
  package_name = c("Weak Package", "Nesta's Package", "Prevention Package", "Treatment Package"),
  cost = c(1.9, 1, 1.7, 5),
  impact = c(1, 5, 5, 5),
  size = c(3, 3, 3, 3))

mid_x <- 3
mid_y <- 3

# Create the bubble plot
ggplot(data, aes(x = impact, y = cost, size = size, color = package_name, label = str_wrap(package_name, width = 12) )) +
  annotate("rect", xmin = mid_x, xmax = 6, ymin = 0, ymax = mid_y, alpha = 0.2, fill = "green") +
  annotate("rect", xmin = mid_x, xmax = 6, ymin = mid_y, ymax = 6, alpha = 0.2, fill = "orange") +
  annotate("rect", xmin = 0, xmax = mid_x, ymin = 0, ymax = mid_y, alpha = 0.2, fill = "red") +
  geom_vline(xintercept = mid_x, linetype = "dashed", color = "black") +
  geom_hline(yintercept = mid_y, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 5,  linetype = "solid", color = "green",size = 0.9) +
  geom_hline(yintercept = 5,  linetype = "solid", color = "darkgrey",size = 0.1) +
  geom_point(alpha = 1) +
  geom_text(nudge_x = 0.5, nudge_y = -0.25, size = 4.5) +
  # geom_text(nudge_x = 0.1, nudge_y = 0.1, size = 3) +
  
   scale_x_continuous(limits = c(0, 6)) +
  # scale_y_continuous(limits = c(1, 6)) +
  theme_minimal() +
  labs(title = "Impact vs Cost",
       x = "Impact - Reduction in obesity prevalence (5 years)",
       y = "Cost to government (5 years) ") +
  theme(plot.title = element_text(hjust = 0.1), legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  annotate("text", x = 0, y = 0, vjust = 1, label = "Low") +  # Annotation for low
  annotate("text", x = 0, y = 6.2, hjust = 0.3, label = str_wrap("High", width = 9) ) +
  annotate("text", x = 0, y = 5, hjust = 0.3, vjust = 1.2, label = str_wrap("~£50 bil)", width = 10), size = 3.5) +
  annotate("text", x = 5.9, y = 0, vjust = 1, label = ("High")) +
  annotate("text", x = 5, y = 0, hjust = 0, vjust = 1, label = "50%", size = 3.5) 






data_2 <- tibble(
  package_name = c("Low impact package", "Nesta's recommendation for halving obesity", "Tax, regulation & treatment intervention package", "Treatment package"),
  cost = c(14.4, 3.2, 11.6, 51),
  impact = c(9.3, 50, 50, 50),
  size = c(3, 3, 3, 3))

mid_x <- 30
mid_y <- 30

# Create the bubble plot
ggplot(data_2, aes(x = impact, y = cost, size = size, color = package_name, label = str_wrap(package_name, width = 20), hjust = 0, fontface = "bold" )) +
  annotate("rect", xmin = mid_x, xmax = 60, ymin = 0, ymax = mid_y, alpha = 0.2, fill = "green") +
  annotate("rect", xmin = mid_x, xmax = 60, ymin = mid_y, ymax = 60, alpha = 0.2, fill = "orange") +
  annotate("rect", xmin = 0, xmax = mid_x, ymin = 0, ymax = mid_y, alpha = 0.2, fill = "red") +
  geom_vline(xintercept = mid_x, linetype = "dashed", color = "black") +
  geom_hline(yintercept = mid_y, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 50,  linetype = "solid", color = "green",size = 0.9) +
  geom_hline(yintercept = 50,  linetype = "solid", color = "darkgrey",size = 0.1) +
  geom_point(alpha = 1) +
  geom_text(nudge_x = 1, nudge_y = 3.5, size = 5) +
  # geom_text(nudge_x = 0.1, nudge_y = 0.1, size = 3) +
  
  # scale_x_continuous(limits = c(0, 6)) +
  # scale_y_continuous(limits = c(1, 6)) +
  theme_minimal() +
  labs(title = "Impact vs Cost",
       x = "Impact - % Reduction in obesity prevalence (5 years)",
       y = "Cost to government (5 years, in £'s) ") +
  theme(plot.title = element_text(size = 20, hjust = 0.1), legend.position = "none",
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)
        ) +
  annotate("text", x = 0, y = 0, vjust = 1, label = "Low") +  # Annotation for low
  annotate("text", x = 0, y = 62, hjust = 0.3, label = str_wrap("High", width = 9) ) +
  annotate("text", x = 0, y = 50, hjust = 0.3, vjust = 1.2, label = str_wrap("~£50 bil)", width = 10), size = 5) +
  annotate("text", x = 59, y = 0, vjust = 1, label = ("High")) +
  annotate("text", x = 50, y = 0, hjust = 0, vjust = 1, label = "50%", size = 5) 






# Create the bubble plot
ggplot(data_2, aes(x = impact, y = cost, size = size, color = package_name)) +
  annotate("rect", xmin = mid_x, xmax = 60, ymin = 0, ymax = mid_y, alpha = 0.2, fill = "green") +
  annotate("rect", xmin = mid_x, xmax = 60, ymin = mid_y, ymax = 60, alpha = 0.2, fill = "orange") +
  annotate("rect", xmin = 0, xmax = mid_x, ymin = 0, ymax = mid_y, alpha = 0.2, fill = "red") +
  geom_vline(xintercept = mid_x, linetype = "dashed", color = "black") +
  geom_hline(yintercept = mid_y, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 50, linetype = "solid", color = "green", size = 0.9) +
  geom_hline(yintercept = 50, linetype = "solid", color = "darkgrey", size = 0.1) +
  geom_point(alpha = 1) +
  geom_text(aes(label = str_wrap(package_name, width = 20)), nudge_x = 5, nudge_y = 4, size = 5, fontface = "bold", data = filter(data_2, package_name != "Nesta's recommendation for halving obesity")) +
  geom_label(aes(label = str_wrap(package_name, width = 20)), nudge_x = 5, nudge_y = 4, size = 5, label.size = 0.5, label.padding = unit(0.25, "lines"), fontface = "bold", data = filter(data_2, package_name == "Nesta's recommendation for halving obesity"),  colour = "#0000ff") +
  scale_color_manual(values = c("Low impact package" = "red",
                                "Nesta's recommendation for halving obesity" = "#0000ff",
                                "Tax, regulation & treatment intervention package" = "black",
                                "Treatment package" = "purple")) +
  theme_minimal() +
  labs(title = "Impact vs Cost",
       x = "Impact - % Reduction in obesity prevalence (5 years)",
       y = "Cost to government (5 years, in £'s)") +
  theme(plot.title = element_text(size = 20, hjust = 0.1), legend.position = "none",
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  annotate("text", x = 0, y = 0, vjust = 1, label = "Low") +
  annotate("text", x = 0, y = 62, hjust = 0.3, label = str_wrap("High", width = 9)) +
  annotate("text", x = 0, y = 50, hjust = 0.3, vjust = 1.2, label = str_wrap("~£50 bil)", width = 10), size = 5) +
  annotate("text", x = 59, y = 0, vjust = 1, label = "High") +
  annotate("text", x = 50, y = 0, hjust = 0, vjust = 1, label = "50%", size = 5)




