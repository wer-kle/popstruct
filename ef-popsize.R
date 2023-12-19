data<-read_excel("results-main.xlsx")

# Create custom labels for Born classes
year_breaks <- seq(min(data$Born), max(data$Born) + 11, by = 11)
year_labels <- paste(year_breaks[-length(year_breaks)], year_breaks[-1], sep = "-")

# Assign labels to Born classes
data$BornClass <- cut(data$Born, breaks = year_breaks, labels = year_labels)

# Calculate effective population size for each class
effective_population <- tapply(data$Sex, data$BornClass, function(x) {
  Nm <- sum(x == "male")
  Nf <- sum(x == "female")
  (4 * Nm * Nf) / (Nm + Nf)
})

# Create a data frame with class, labels, and effective population size
effective_pop_df <- data.frame(Class = names(effective_population), Label = levels(data$BornClass), EffectivePopulation = effective_population)

# View the resulting data frame
print(effective_pop_df)

library(ggplot2)

# Create the plot
ggplot(effective_pop_df, aes(x = Class, y = EffectivePopulation)) +
  geom_bar(stat = "identity") +
  labs(x = "Born Class", y = "Effective Population Size", title = "Effective Population Size by Born Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

