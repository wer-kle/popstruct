library(readxl)
data<-read_excel("results-main.xlsx")

library(dplyr)

# Rename columns for joining with sires
sire_data <- data %>%
  rename(Temp_Indiv = Indiv, Sire_Born = Born) %>%
  select(Temp_Indiv, Sire_Born) %>%
  rename(Sire = Temp_Indiv)

data <- left_join(data, sire_data, by = "Sire")

#the same for dams
dam_data <- data %>%
  rename(Temp_Indiv = Indiv, Dam_Born = Born) %>%
  select(Temp_Indiv, Dam_Born) %>%
  rename(Dam = Temp_Indiv)

data <- left_join(data, dam_data, by = "Dam")

##TOTAL POPULATION (TP)
#filter NA values
complete_data <- data %>% 
  filter(!is.na(Sire) & !is.na(Dam) & !is.na(Sire_Born) & !is.na(Dam_Born))

#calculte generation intervals for complete data
complete_data$sire_son_GI <- ifelse(complete_data$Sex == "male", 
                                    complete_data$Born - complete_data$Sire_Born, NA)
complete_data$sire_daughter_GI <- ifelse(complete_data$Sex == "female", 
                                         complete_data$Born - complete_data$Sire_Born, NA)
complete_data$dam_son_GI <- ifelse(complete_data$Sex == "male", 
                                   complete_data$Born - complete_data$Dam_Born, NA)
complete_data$dam_daughter_GI <- ifelse(complete_data$Sex == "female", 
                                        complete_data$Born - complete_data$Dam_Born, NA)

#calculate average generation intervals for 4 paths
average_GIs_list <- list(
  sire_son = mean(complete_data$sire_son_GI, na.rm = TRUE),
  sire_daughter = mean(complete_data$sire_daughter_GI, na.rm = TRUE),
  dam_son = mean(complete_data$dam_son_GI, na.rm = TRUE),
  dam_daughter = mean(complete_data$dam_daughter_GI, na.rm = TRUE)
)

print(average_GIs_list)


# Adding averages for both sire paths, both dam paths, and overall average for 4 paths
average_GIs_list$sire_average <- mean(c(average_GIs_list$sire_son, average_GIs_list$sire_daughter))
average_GIs_list$dam_average <- mean(c(average_GIs_list$dam_son, average_GIs_list$dam_daughter))
average_GIs_list$overall_average <- mean(c(average_GIs_list$sire_son, 
                                           average_GIs_list$sire_daughter, 
                                           average_GIs_list$dam_son, 
                                           average_GIs_list$dam_daughter))

print(average_GIs_list)

##REFERENCE POPULATION (RP)
# Filter data for complete and Reference as TRUE
reference_data <- data %>% 
  filter(!is.na(Sire) & !is.na(Dam) & !is.na(Sire_Born) & !is.na(Dam_Born) & Reference == TRUE)

# Calculate generation intervals for reference_data
reference_data$sire_son_GI <- ifelse(reference_data$Sex == "male", 
                                     reference_data$Born - reference_data$Sire_Born, NA)
reference_data$sire_daughter_GI <- ifelse(reference_data$Sex == "female", 
                                          reference_data$Born - reference_data$Sire_Born, NA)
reference_data$dam_son_GI <- ifelse(reference_data$Sex == "male", 
                                    reference_data$Born - reference_data$Dam_Born, NA)
reference_data$dam_daughter_GI <- ifelse(reference_data$Sex == "female", 
                                         reference_data$Born - reference_data$Dam_Born, NA)

# Calculate average generation intervals for 4 paths for reference_data
reference_average_GIs_list <- list(
  sire_son = mean(reference_data$sire_son_GI, na.rm = TRUE),
  sire_daughter = mean(reference_data$sire_daughter_GI, na.rm = TRUE),
  dam_son = mean(reference_data$dam_son_GI, na.rm = TRUE),
  dam_daughter = mean(reference_data$dam_daughter_GI, na.rm = TRUE)
)

# Adding averages for both sire paths, both dam paths, and overall average for 4 paths for reference_data
reference_average_GIs_list$sire_average <- mean(c(reference_average_GIs_list$sire_son, 
                                                  reference_average_GIs_list$sire_daughter))
reference_average_GIs_list$dam_average <- mean(c(reference_average_GIs_list$dam_son, 
                                                 reference_average_GIs_list$dam_daughter))
reference_average_GIs_list$overall_average <- mean(c(reference_average_GIs_list$sire_son, 
                                                     reference_average_GIs_list$sire_daughter, 
                                                     reference_average_GIs_list$dam_son, 
                                                     reference_average_GIs_list$dam_daughter))

print(reference_average_GIs_list)

#plot the changes in GIs across the years
library(ggplot2)

complete_data <- complete_data %>%
  mutate(
    sire_son_GI = ifelse(sire_son_GI < 0, NA, sire_son_GI),
    sire_daughter_GI = ifelse(sire_daughter_GI < 0, NA, sire_daughter_GI),
    dam_son_GI = ifelse(dam_son_GI < 0, NA, dam_son_GI),
    dam_daughter_GI = ifelse(dam_daughter_GI < 0, NA, dam_daughter_GI)
  )

complete_data <- complete_data %>%
  filter(
    (is.na(sire_son_GI) | sire_son_GI <= 25) &
      (is.na(sire_daughter_GI) | sire_daughter_GI <= 25) &
      (is.na(dam_son_GI) | dam_son_GI <= 25) &
      (is.na(dam_daughter_GI) | dam_daughter_GI <= 25)
  )


# Create summary data for each year
summary_data <- complete_data %>%
  group_by(Born) %>%
  summarise(
    avg_sire_son_GI = mean(sire_son_GI, na.rm = TRUE),
    avg_sire_daughter_GI = mean(sire_daughter_GI, na.rm = TRUE),
    avg_dam_son_GI = mean(dam_son_GI, na.rm = TRUE),
    avg_dam_daughter_GI = mean(dam_daughter_GI, na.rm = TRUE)
  )

ss<-ggplot(summary_data, aes(x = Born, y = avg_sire_son_GI)) +
  geom_line(color = "lightblue") +
  geom_smooth(method = "gam", color  = "navy") +
  ggtitle("Sire-Son") +
  xlab("Year of Birth") +
  ylab("GI") +
  theme_minimal()

sd<-ggplot(summary_data, aes(x = Born, y = avg_sire_daughter_GI)) +
  geom_line(color = "pink") +
  geom_smooth(method = "gam", color = "navy") +
  ggtitle("Sire-Daughter") +
  xlab("Year of Birth") +
  ylab("GI") +
  theme_minimal()

ds<-ggplot(summary_data, aes(x = Born, y = avg_dam_son_GI)) +
  geom_line(color = "lightblue") +
  geom_smooth(method = "gam", color = "magenta") +
  ggtitle("Dam-Son") +
  xlab("Year of Birth") +
  ylab("GI") +
  theme_minimal()

dd<-ggplot(summary_data, aes(x = Born, y = avg_dam_daughter_GI)) +
  geom_line(color = "pink") +
  geom_smooth(method = "gam", color = "magenta") +
  ggtitle("Dam-Daughter") +
  xlab("Year of Birth") +
  ylab("GI") +
  theme_minimal()

library(gridExtra)

grid.arrange(ss, sd, ds, dd, ncol = 2)

#ONLY FOR 20TH CENTURY
complete_data <- complete_data %>% filter(Born > 1900)

summary_data <- complete_data %>%
  group_by(Born) %>%
  summarise(
    avg_sire_son_GI = mean(sire_son_GI, na.rm = TRUE),
    avg_sire_daughter_GI = mean(sire_daughter_GI, na.rm = TRUE),
    avg_dam_son_GI = mean(dam_son_GI, na.rm = TRUE),
    avg_dam_daughter_GI = mean(dam_daughter_GI, na.rm = TRUE)
  )

ss<-ggplot(summary_data, aes(x = Born, y = avg_sire_son_GI)) +
  geom_line(color = "lightblue") +
  geom_smooth(method = "gam", color  = "navy") +
  ggtitle("Sire-Son") +
  xlab("Year of Birth") +
  ylab("GI") +
  theme_minimal()

sd<-ggplot(summary_data, aes(x = Born, y = avg_sire_daughter_GI)) +
  geom_line(color = "pink") +
  geom_smooth(method = "gam", color = "navy") +
  ggtitle("Sire-Daughter") +
  xlab("Year of Birth") +
  ylab("GI") +
  theme_minimal()

ds<-ggplot(summary_data, aes(x = Born, y = avg_dam_son_GI)) +
  geom_line(color = "lightblue") +
  geom_smooth(method = "gam", color = "magenta") +
  ggtitle("Dam-Son") +
  xlab("Year of Birth") +
  ylab("GI") +
  theme_minimal()

dd<-ggplot(summary_data, aes(x = Born, y = avg_dam_daughter_GI)) +
  geom_line(color = "pink") +
  geom_smooth(method = "gam", color = "magenta") +
  ggtitle("Dam-Daughter") +
  xlab("Year of Birth") +
  ylab("GI") +
  theme_minimal()

library(gridExtra)

grid.arrange(ss, sd, ds, dd, ncol = 2)



