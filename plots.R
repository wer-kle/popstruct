#Libraries
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(reshape2)
library(gridExtra)
library(readxl)

#Sheet with all the results
ped<-read_excel("results-main.xlsx")

#only reference population
ref<-ped[ped$Reference == TRUE, ]

#without the individuals who aren't ancestors of RP nor RP itself
total <- ped[ped$Reference | (ped$Reference == FALSE & ped$Offspring), ]

#total without reference population
anc<- ped[(ped$Reference == FALSE & ped$Offspring), ]

#Create the "Year of Birth" groups - every 50 years
ped$Born_group <- cut(ped$Born, breaks = seq(1630, 2020, by = 50), labels = FALSE, include.lowest = TRUE)

# Create custom labels for the x-axis
born_labels <- seq(1630, 2020, by = 50)
born_labels <- sprintf("%d-%d", born_labels, born_labels + 49)

#DECRIPTIVE STATISTICS
names(ped)
# Histogram of TP by yob with division by sex
TP_yob<-ggplot(ped, aes(x = as.factor(Born_group), fill = Sex)) +
  geom_histogram(stat = "count", position = "dodge", color = "black", bins = 10) +
  labs(x = "Year of Birth", y = "Frequency") +
  theme_minimal() +
  scale_x_discrete(labels = born_labels)

# Histogram of RP by yob with division by sex
RP_yob<-ggplot(ref, aes(x = Born, fill = Sex)) +
  geom_histogram(stat = "count", position = "dodge", color = "black", bins = 10) +
  labs(x = "Year of Birth", y = "Frequency") +
  theme_minimal()

# only born after 1980

new <- ped[ped$Born >= 1980, ]

library(ggplot2)

# Calculate average Born for both sexes
average_count_male <- mean(new$Sex[new$Sex == "male"])
average_count_female <- mean(new$Sex[new$Sex == "female"])

# Get the fill colors from your existing plot's scale
fill_colors <- scales::hue_pal()(2)

# Create the histogram plot
ggplot(new, aes(x = Born, fill = Sex)) +
  geom_histogram(stat = "count", position = "dodge", color = "black", bins = 10) +
  labs(x = "Year of Birth", y = "Frequency") +
  theme_minimal() +
  geom_hline(xintercept = average_count_male, color = fill_colors[1], linetype = "dashed", size = 1) +
  geom_hline(xintercept = average_count_female, color = fill_colors[2], linetype = "dashed", size = 1)

# Print the histogram plot
print(histogram_plot)



library(gridExtra)
inbredplot<-grid.arrange(plot1_label, plot2_label, ncol=1)



#Distribution of Demographic Parameters -> number of generations and Pedigree Completeness
hist(optisel_summary$equiGen, main="Number of Equivalent Complete Generations")
hist(optisel_summary$fullGen, main="Number of Fully Traced Generations")
hist(optisel_summary$maxGen, main="Number of Maximum Geneerations Traced")
hist(optisel_summary$PCI, xlim=c(0,0.7), main="Pedigree Completeness Index")
hist(optisel_summary$Inbreeding, xlim=c(0,0.4), main="Inbreeding")

# mating ratio by yob
sex_counts <- ped %>%
  group_by(Born, Sex) %>%
  summarise(Count = n()) %>%
  spread(Sex, Count, fill = 0) %>%
  mutate(Ratio = (female + 1e-6) / (male + 1e-6))

ped<-read_excel("results-main.xlsx")
# Create the plot
ggplot(sex_counts, aes(x = Born, y = Ratio, group = 1)) +
  geom_line() +
  labs(title = "Female to Male Ratio by Year of Birth",
       x = "Year of Birth", y = "Female to Male Ratio") +
  theme_minimal()

#Number of horses by generation - Total Population
ggplot(ped, aes(x = maxGen, fill = Sex)) +
  geom_histogram(stat = "count", position = "dodge", color = "black", bins = 10) +
  labs(title = "Number of Horses by Maximum Generation Traced", 
       x = "Maximum Generation Traced", y = "Frequency") +
  theme_minimal()

#Number of horses by generation - Reference Population
ggplot(ref, aes(x = maxGen, fill = Sex)) +
  geom_histogram(stat = "count", position = "dodge", color = "black", bins = 10) +
  labs(title = "Number of Horses by Maximum Generation Traced", 
       x = "Maximum Generation Traced", y = "Frequency") +
  theme_minimal()

#Number of Equivalent Generations by year of birth (in groups)
##boxplot
ggplot(ref, aes (x = Sex, y = equiGen, fill = Sex)) +
  geom_boxplot() +
  labs(title = "Equivalent Complete Generations by Year of Birth", 
       x = "Year of Birth", 
       y = "Equivalent Complete Generation") +
  theme_minimal()

#Number of Maximum Generations Traced by year of birth (in groups)
ggplot(ped, aes(x = as.factor(Born_group), y = maxGen, fill = stat(count))) +
  stat_bin2d(binwidth = c(1, 1)) +
  scale_fill_viridis(name = "Frequency") +  # You can use other color palettes as well
  labs(title = "Maximum Generation Traced by Year of Birth", 
       x = "Year of Birth", 
       y = "Maximum Generation Traced") +
  theme_minimal() +
  scale_x_discrete(labels = born_labels)

library(ggplot2)

library(ggplot2)


heatmap(ped, Colv = "sireline", Rowv = "damline", scale = "column")

count_data <- ref %>%
  group_by(Born, maxGen) %>%
  summarize(count = n())

ggplot(count_data, aes(x = Born, y = maxGen, fill = count)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Count") +
  labs(title = "Heatmap of Count by Born and maxGen",
       x = "Born", y = "maxGen") +
  theme_minimal()

# Print the heatmap
print(heatmap_plot)


# Group by "damline" and "sex" and count the number of occurrences
count_data <- ref %>%
  group_by(damline, Sex) %>%
  summarize(count = n())

females <- count_data %>%
  filter(Sex == "female")

males <- count_data %>%
  filter(Sex == "male")

# Calculate the top N damline types for each sex
top_n_damlines <- count_data %>%
  group_by(Sex) %>%
  top_n(15, count)

top_n_damlines_f <- females %>%
  group_by(Sex) %>%
  top_n(15, count)

top_n_damlines_m <- males %>%
  group_by(Sex) %>%
  top_n(15, count)


# Create a grouped bar plot
#for both sexes
ggplot(top_n_damlines, aes(x = reorder(damline, -count), y = count, fill = damline)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Sex, ncol = 1) +
  scale_fill_discrete() +
  labs(title = "Top 15 damline types by sex",
       x = "Damline", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

#for females
ggplot(top_n_damlines_f, aes(x = reorder(damline, -count), y = count, fill = damline)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Sex, ncol = 1) +
  scale_fill_discrete() +
  labs(title = "Top 15 damline types by sex",
       x = "Damline", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

#for males
ggplot(top_n_damlines_m, aes(x = reorder(damline, -count), y = count, fill = damline)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Sex, ncol = 1) +
  scale_fill_discrete() +
  labs(title = "Top 15 damline types by sex",
       x = "Damline", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

#sirelines
count_data <- ref %>%
  group_by(sireline, Sex) %>%
  summarize(count = n())

females <- count_data %>%
  filter(Sex == "female")

males <- count_data %>%
  filter(Sex == "male")

# Calculate the top N damline types for each sex
top_n_sirelines <- count_data %>%
  group_by(Sex) %>%
  top_n(8, count)

top_n_sirelines_f <- females %>%
  group_by(Sex) %>%
  top_n(8, count)

top_n_sirelines_m <- males %>%
  group_by(Sex) %>%
  top_n(8, count)


# Create a grouped bar plot
#for both sexes
ggplot(top_n_sirelines, aes(x = reorder(sireline, -count), y = count, fill = sireline)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Sex, ncol = 1) +
  scale_fill_discrete() +
  labs(title = "Top sirelines by sex",
       x = "Sireline", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

#for females
ggplot(top_n_sirelines_f, aes(x = reorder(sireline, -count), y = count, fill = sireline)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Sex, ncol = 1) +
  scale_fill_discrete() +
  labs(title = "Top 8 sirelines by sex",
       x = "Sireline", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

#for males
ggplot(top_n_sirelines_m, aes(x = reorder(sireline, -count), y = count, fill = sireline)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Sex, ncol = 1) +
  scale_fill_discrete() +
  labs(title = "Top 8 sirelines",
       x = "Sireline", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

#plot the Saklawi line!

library(dplyr)
install.packages("ggraph")
install.packages("igraph")
library(ggraph)
library(igraph)

ped<-read_excel("results-main.xlsx")

library(dplyr)
library(ggraph)
library(igraph)

# Assuming your dataset is named "ped"
# Filter the dataset to start with Saklawi_I (numIndiv = 706)
current_generation <- ped %>%
  filter(numIndiv == 706)

# Create an empty graph to store the pedigree
pedigree_graph <- graph.empty()

library(dplyr)
library(ggraph)
library(igraph)


# INBREEDING
##average inbreeding by year of birth
library(dplyr)
ped<-read_excel("results-main.xlsx")
avgI_by_yob<-ped %>%
  group_by(Born) %>%
  summarise(avg_calcInbr = mean(Inbreeding))
write.csv(avgI_by_yob,"ICA.csv")
names(avgI_by_yob)

avFyob<-ggplot(avgI_by_yob, aes(x=Born, y=avg_calcInbr))+
  geom_line(color="lightgrey") + geom_smooth(method = "gam", se = FALSE, 
                                            color = "red", linetype = "solid") + 
  theme_minimal()+
  labs(x = "", y = "Average Inbreeding Coefficient")
plot(avFyob)
#Average rate of Inbred Animals by yob
percentage_inbreeding_gt_0 <- ped %>%
  group_by(Born) %>%
  summarize(Percentage_Inbreeding_GT_0 = mean(Inbreeding > 0) * 100)

avratFyob<-ggplot(percentage_inbreeding_gt_0, aes(x = Born, y = Percentage_Inbreeding_GT_0)) +
  geom_line(color="lightgrey") + geom_smooth(method = "gam", 
                                            se = FALSE, 
                                            color = "red", linetype = "dashed") +
  theme_minimal()+
  labs(x = "Year of Birth", y = "RIA, %")

cor(avgI_by_yob, percentage_inbreeding_gt_0)

plot1_label <- avFyob + annotate("text", x = Inf, y = -Inf, label = "A", 
                                 hjust = 1, vjust = 0, size = 14)
plot2_label <- avratFyob + annotate("text", x = Inf, y = -Inf, label = "B", 
                                    hjust = 1, vjust = 0, size = 14)
library(gridExtra)
inbredplot<-grid.arrange(plot1_label, plot2_label, ncol=1)

# Inbreeding in RP by YOB
ggplot(ref, aes(x = as.factor(Born), y = Inbreeding, fill = Sex)) +
  geom_boxplot() +
  labs(title = "Boxplot of Inbreeding by Yob and Sex",
       x = "Yob", y = "Inbreeding") +
  theme_minimal() +
  facet_wrap(~ Born, scales = "free_x", ncol = 11)

# Inbreeding in RP

ggplot(ref, aes(x = Inbreeding, fill = Sex)) +
  geom_density(alpha = 0.5) +
  labs(
       x = "Inbreeding",
       y = "Density") +
  theme_minimal()

library(readxl)
ped<-read_excel("results-main.xlsx")

library(dplyr)
## Observed Inbreeding vs Expected Inbreeding (Wright's) in maximum generations traced
average_F <- ped %>%
  group_by(maxGen) %>%
  summarize(avg_F = mean(ip_F, na.rm = TRUE))

library(ggplot2)
ggplot(ped, aes(x = maxGen-1)) +
  geom_point(aes(y = ip_F, color = "ip_F"), alpha = 0.7) +   # Plot all values of 'F' in blue with transparency
  geom_point(aes(y = exp_F, color = "exp_F"), size = 1) +  # Plot 'exp_F' as a constant red point
  geom_line(data = average_F, aes(y = avg_F, color = "avg_F"), linewidth = 1) +  # Plot the average of 'Fi' as a green line
  labs(x = "Generation", y = "Inbreeding Coefficient (F)", 
       title = "Observed and Expected F for each generation") +
  scale_color_manual(values = c("ip_F" = "lightgrey", "exp_F" = "red", "avg_F" = "darkblue"),
                     labels = c("AF", "EF", "F"),
                     name = "Inbreeding Coefficients") +
  guides(color = guide_legend(title = "", 
                              position = "bottom"))+
  theme_minimal()

## Observed Ancestral Inbreeding vs Expected Ancestral Inbreeding in Maximum Generations Traced
average_Fa <- ped %>%
  group_by(maxGen) %>%
  summarize(avg_Fa = mean(ip_Fa, na.rm = TRUE))

ggplot(ped, aes(x = maxGen-1)) +
  geom_point(aes(y = ip_Fa, color = "ip_Fa"), alpha = 0.7) +   # Plot all values of 'F' in blue with transparency
  geom_point(aes(y = exp_Fa, color = "exp_Fa"), size = 1.2) +  # Plot 'exp_F' as a constant red point
  geom_line(data = average_Fa, aes(y = avg_Fa, color = "avg_Fa"), linewidth = 1.2) +  # Plot the average of 'Fi' as a green line
  labs(x = "Generation", y = "Ancestral Inbreeding Coefficient (Fa)", 
       title = "Observed and Expected Fa for each generation") +
  scale_color_manual(values = c("ip_Fa" = "lightgrey", 
                                "exp_Fa" = "red", 
                                "avg_Fa" = "darkblue"),
                     labels = c("AFa", "EFa", "Fa"),
                     name = "Inbreeding Coefficients") +
  guides(color = guide_legend(title = "", 
                              position = "bottom"))+
  theme_minimal()

## Observed Purged Inbreeding vs Expected Purged Inbreeding by Maximum Generations Traced
ped<-read_excel("results-main.xlsx")
average_g <- ped %>%
  group_by(maxGen) %>%
  summarize(avg_g = mean(ip_g, na.rm = TRUE))

ggplot(ped, aes(x = maxGen-1)) +
  geom_point(aes(y = ip_g, color = "ip_g"), alpha = 0.7) +   
  geom_point(aes(y = exp_g, color = "exp_g"), size = 1) +  
  geom_line(data = average_g, aes(y = avg_g, color = "avg_g"), linewidth = 1) +  
  labs(x = "Generation", y = "Purged Inbreeding Coefficient (G)", 
       title = "Observed and Expected G for each generation") +
  scale_color_manual(values = c("ip_g" = "lightblue", 
                                "exp_g" = "red", 
                                "avg_g" = "darkblue"),
                     labels = c("AG", "EG", "G"),
                     name = "Purged Inbreeding Coefficients") +
  guides(color = guide_legend(title = "", 
                              position = "bottom"))+
  theme_minimal()

# GENETIC CONTRIBUTION
##Genetic Contributions of damlines across generations
df <- read.csv("damlines_genecont_generations.csv")
df <- df %>%
  mutate(generation = as.numeric(generation)) %>%
  filter(!is.na(generation))
columns_to_average <- names(df)[2:16]
df[columns_to_average] <- lapply(df[columns_to_average], as.numeric)
df <- df[rowSums(sapply(df[columns_to_average], is.na)) == 0, ]
averages_df <- data.frame(generation = unique(df$generation))

for (col in columns_to_average) {
  averages_df[[col]] <- df %>% 
    group_by(generation) %>% 
    summarize(avg_value = mean(.data[[col]], na.rm = TRUE)) %>% 
    pull(avg_value)
}

averages_melted <- melt(averages_df, id.vars = "generation", 
                        variable.name = "column", 
                        value.name = "average_value")

ggplot(averages_melted, aes(x = generation, 
                                               y = average_value, 
                                               color = column)) +
  geom_line() +
  geom_point() +
  labs(x = "Generation", y = "Average Genetic Contribution", 
       title = "Average Genetic Contribution by Generation") +
  theme(legend.position = "top", legend.title = element_blank())


## combined plots for each damline separately
plot_list <- list()

custom_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", 
                            "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6", "#ffff99", 
                            "#b15928", "#a1c9f4", "#b3e2cd", "#fdb462")
                            
for (i in seq_along(columns_to_average)) {
  col <- columns_to_average[i]
  
  # Filter data for the current column
  df_filtered <- df[complete.cases(df[col]), ]
  
  # Calculate average for the current column
  df_filtered$average_value <- rowMeans(df_filtered[col], na.rm = TRUE)
  
  # Create individual plot for the current column
  plot <- ggplot(df_filtered, aes(x = generation, y = average_value)) +
    geom_line(color = custom_colors[i]) +
    geom_point(color = custom_colors[i]) +
    labs(x = "Generation", y = "Average Value", title = NULL) +
    theme(legend.position = "none",
          axis.title.x = element_text(),
          axis.title.y = element_text())
  
  # Add the plot to the list
  plot_list[[col]] <- plot
}

combined_plot <- grid.arrange(grobs = plot_list, ncol = 3)

ggsave(filename = "averages_combined.pdf", plot = combined_plot, device = "pdf")
print(combined_plot)

dir.create("averages_plots", showWarnings = FALSE)
for (i in seq_along(columns_to_average)) {
  col <- columns_to_average[i]
  
  # Filter data for the current column
  df_filtered <- df[complete.cases(df[col]), ]
  
  # Calculate average for the current column
  df_filtered$average_value <- rowMeans(df_filtered[col], na.rm = TRUE)
  
  # Create individual plot for the current column
  plot <- ggplot(df_filtered, aes(x = generation, y = average_value)) +
    geom_line(color = custom_colors[i]) +
    geom_point(color = custom_colors[i]) +
    labs(x = "Generation", y = "Average Value", title = paste("Average Values for", col, "by Generation")) +
    theme(legend.position = "none",
          axis.title.x = element_text(),
          axis.title.y = element_text())
  
  # Save the plot as a PDF in the "averages_plots" folder
  pdf_filename <- paste("averages_plots/", col, ".pdf", sep = "")
  ggsave(filename = pdf_filename, plot = plot, device = "pdf")
  
  # Print the plot (optional)
  print(plot)
}

#Average Genetic Contributions of Sirelines across generations
df <- read_excel("sirelines_genecont_generations.xlsx")
names(df)
df <- df %>%
  mutate(generation = as.numeric(generation)) %>%
  filter(!is.na(generation))

columns_to_average <- names(df)[2:9]

df[columns_to_average] <- lapply(df[columns_to_average], as.numeric)

df <- df[rowSums(sapply(df[columns_to_average], is.na)) == 0, ]

averages_df <- data.frame(generation = unique(df$generation))

for (col in columns_to_average) {
  averages_df[[col]] <- df %>% 
    group_by(generation) %>% 
    summarize(avg_value = mean(.data[[col]], na.rm = TRUE)) %>% 
    pull(avg_value)
}
print(averages_df)
averages_melted <- melt(averages_df, id.vars = "generation", 
                        variable.name = "column", 
                        value.name = "average_value")

ggplot(averages_melted, aes(x = generation, 
                            y = average_value, 
                            color = column)) +
  geom_line() +
  geom_point() +
  labs(x = "Generation", y = "Average Genetic Contribution", 
       title = "Average Genetic Contribution by Generation") +
  theme(legend.position = "top", legend.title = element_blank())

#Number of migrants - how many stallions has been imported to Polish population
# Calculate the total number of unique stallions (regardless of country) by 'yob'
stallions <- read.csv("FINAL-stallions.csv")
total_stallions_year <- stallions %>%
  group_by(year) %>%
  summarize(total_unique_stallions = n_distinct(`stallion-id`))

# Calculate the percentage of unique foreign stallions by 'yob'
foreign_stallions_percentage <- foreign_stallions_year %>%
  left_join(total_stallions_year, by = "year") %>%
  mutate(percentage = (unique_foreign_stallions / total_unique_stallions) * 100)

# Create the plot
ggplot(foreign_stallions_percentage, aes(x = year, y = percentage)) +
  geom_line() + geom_smooth(method = "gam", se = FALSE, color = "darkred") +
  labs(x = "Year", y = "Percentage of Imported Stallions", 
       title = "Percentage of Imported Stallions by Year") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(1992, 2016, by = 2))

# INDIVIDUAL FITNESS AND SURVIVAL

#number of progeny
library(ggplot2)
library(patchwork)

# Plot for the total dataset
plot_total <- ggplot(total, aes(x = w_off, fill = Sex)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.7) +
  labs(title = "Total Offspring by Sex",
       x = "Number of Offspring",
       y = "Frequency") +
  theme_minimal()

# Plot for the ref subset
plot_ref <- ggplot(ref, aes(x = w_off, fill = Sex)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.7) +
  labs(title = "Offspring for Reference Subset by Sex",
       x = "Number of Offspring",
       y = "Frequency") +
  theme_minimal()

# Plot for the anc subset
plot_anc <- ggplot(anc, aes(x = w_off, fill = Sex)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.7) +
  labs(title = "Offspring for Anc Subset by Sex",
       x = "Number of Offspring",
       y = "Frequency") +
  theme_minimal()

# Combine plots using patchwork
final_plot <- plot_total + plot_ref + plot_anc +
  plot_layout(ncol = 1)

# Display the combined plot
final_plot

#average number of progeny by yob
# Group by Born_group and Sex, calculate average w_off
average_w_off <- ped %>%
  group_by(Born, Sex) %>%
  summarise(avg_w_off = mean(w_off, na.rm = TRUE))

# Create the scatter plot with trendlines
ggplot(average_w_off, aes(x = Born, y = avg_w_off, color = Sex)) +
  geom_point() +
  geom_smooth(method = "gam", se = FALSE, aes(group = Sex), linetype = "solid", size = 1.5) +
  labs(
       x = "Born Group",
       y = "Number of Progeny") +
  theme_minimal()

library(readxl)
library(ggplot2)
ped<-read_excel("results-main.xlsx")
names(ped)

offspring<-ped[ped$Offspring == TRUE, ]

dams<-offspring[offspring$Sex == "female", ]
sires<-offspring[offspring$Sex == "male", ]
#offspring vs inbreeding
ggplot(dams, aes(x = w_off, y = ip_F)) +
  geom_point() +
  geom_smooth(method = "gam", se = FALSE, linetype = "solid", size = 1.5, 
              color = "magenta") +
  labs(
    x = "offspring",
    y = "inbreeding") +
  theme_minimal()

ggplot(sires, aes(x = w_off, y = ip_F)) +
  geom_point() +
  geom_smooth(method = "gam", se = FALSE, linetype = "solid", size = 1.5) +
  labs(
    x = "offspring",
    y = "inbreeding") +
  theme_minimal()


