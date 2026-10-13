setwd("~/week9/be623_assignment5")
library(dplyr)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
#create data frame circ_df
circ_df <- data.frame(
  Sample = paste0("S", 1:12),
  
  Gene = c(rep("PER2", 6), rep("BMAL1", 6) ),
  ZT = rep(c(0, 4, 8, 12, 16, 20), 2),
  Expression = c(
    1.8, 3.2, 6.5, 9.4, 7.1, 3.6,
    8.6, 5.9, 2.7, 1.5, 3.4, 6.8
  )
)

str(circ_df)
#Select six PER2 rows
per2_rows <- circ_df[circ_df$Gene == "PER2",] #take row per2 and keep all columns
#Display the per2 row
per2_rows
#Calculate statistics for per2
mean(per2_rows$Expression)
median(per2_rows$Expression)
sd(per2_rows$Expression)

# Save the data frame without row numbers
write.csv(circ_df, "circadian_expression.csv", row.names = FALSE)
# Read the CSV file back into R
circ_reloaded <- read.csv("circadian_expression.csv")
# Check whether both files same
identical(circ_df$Expression, circ_reloaded$Expression)

# PER2: sort from highest to lowest expression
per2_peak <- circ_df %>%
  filter(Gene == "PER2") %>%
  arrange(desc(Expression))

# BMAL1: sort from highest to lowest expression
bmal1_peak <- circ_df %>%
  filter(Gene == "BMAL1") %>%
  arrange(desc(Expression))

# Display the sorted data frames
per2_peak
bmal1_peak

# Extract the ZT of each peak
per2_ZT <- per2_peak$ZT[1]
per2_ZT
bmal1_ZT <- bmal1_peak$ZT[1]
bmal1_ZT

# Calculate the hours between the peaks
hours_apart <- abs(per2_ZT - bmal1_ZT)
hours_apart

circ_df2 <- circ_df %>%
  mutate(dark = ZT >= 12)   #mutate is used to add column

circ_df2

dark_summary <- circ_df2 %>%
  filter(dark == TRUE) %>%
  group_by(Gene) %>%
  summarise(
    mean_expression = mean(Expression),
    n_samples = n()
  )

dark_summary

light_summary <- circ_df2 %>%
  filter(dark == FALSE) %>%
  group_by(Gene) %>%
  summarise(
    mean_expression = mean(Expression),
    n_samples = n()
  )

light_summary

#Scatter plot
rhythm_plot <- ggplot(circ_df, aes(x = ZT, y = Expression, color = Gene)) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("PER2" = "blue", "BMAL1" = "red")  ) +
  labs( title = "Circadian Gene Expression", x = "Zeitgeber time (h)", y = "Expression", color = "Gene") +
  theme_minimal()

# Display the plot
rhythm_plot

# Save the plot as a PNG file
ggsave("rhythm_plot.png", plot = rhythm_plot, width = 6, height = 4, dpi = 300)

#Box plot
expression_boxplot <- ggplot(circ_df, aes(x = Gene, y = Expression, fill = Gene)) +
  geom_boxplot() +
  scale_fill_manual(values = c("PER2" = "steelblue", "BMAL1" = "firebrick")) +
  labs(title = "Expression distribution by Gene", x = "Gene", y = "Expression") +
  theme_minimal()

#Display the box plot
expression_boxplot

#Save the plot
ggsave("expression_boxplot.png", width = 6, height = 4, dpi = 300)

getwd()
list.files()
list.files(pattern = "assignment5\\.R$", ignore.case = TRUE)
