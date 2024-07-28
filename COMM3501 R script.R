###R Coding Collected 2.0
library(forecast)
library(pheatmap)
library(dplyr)
library(tidyr)
library(readr)
library(hms)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(corrplot)
library(caret)
library(gains)
library(pROC)

# Read data
dataset <- read.csv("A3_Dataset_2023.csv")

# Examine the data structure and the first few rows of data
str(dataset)
head(dataset)

# Convert data format
dataset$Date <- as.Date(dataset$Date, format = "%d/%m/%Y")
dataset$Time <- as.hms(as.POSIXct(dataset$Time, format = "%I:%M:%S %p"))
dataset$ExternalRef <- as.character(dataset$ExternalRef)

# Fill null value to 0
dataset <- dataset %>%
  mutate(Alternative = replace(Alternative, Alternative == "" | is.na(Alternative), 0)) %>%
  mutate(PremiumFrequency = replace(PremiumFrequency, PremiumFrequency == "" | is.na(PremiumFrequency), 0))

# Visual check: Use a box diagram to view outliers
#1 LifeCoverAmount
ggplot(dataset, aes(x = factor(0), y = LifeCoverAmount)) + 
  geom_boxplot() +
  xlab("X") +
  ylab("LifeCoverAmount") +
  ggtitle("Boxplot for Checking Outliers in 'LifeCoverAmount'")
# LifeCoverAmount Although there is an extreme value, it indicates that the policy under the life product will have a large number of cases.
# is a normal and real situation, do not need to remove extreme values.

#2 AnnualIncome
ggplot(dataset, aes(x = factor(0), y = AnnualIncome)) + 
  geom_boxplot() +
  xlab("X") +
  ylab("AnnualIncome") +
  ggtitle("Boxplot for Checking Outliers in 'AnnualIncome'")
# There is a maximum value that needs to be removed

# Extremum removal
Q1 <- quantile(dataset$AnnualIncome, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$AnnualIncome, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
dataset_clean <- subset(dataset, AnnualIncome >= lower_bound & AnnualIncome <= upper_bound)

# View the cleaned box diagram
ggplot(dataset_clean, aes(x = factor(0), y = AnnualIncome)) + 
  geom_boxplot() +
  xlab("X") +
  ylab("Annual Income") +
  ggtitle("Boxplot for Checking Outliers in 'AnnualIncome'")

# Filled NA value
dataset_clean <- dataset_clean %>%
  mutate(across(where(is.character), ~replace_na(., ""))) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))
dataset_row_count <- sum(apply(dataset_clean, 1, function(row) any(is.na(row))))
print(paste("Number of rows with NA values after cleaning:", dataset_row_count))
str(dataset_clean)
dataset_clean_na_count_after_conversion <- sum(is.na(dataset_clean))
print(paste("Number of NA values after conversion:", dataset_clean_na_count_after_conversion))


#Consultant's data set
Adviser_data <- data.frame(
  AdviserID = dataset_clean$AdviserID,
  CommissionStructure = dataset_clean$CommissionStructure)


#Customer data sets
Customer_data <- data.frame(
  AgeNext = dataset_clean$AgeNext,
  Gender = dataset_clean$Gender,
  SmokerStatus = dataset_clean$SmokerStatus,
  HomeState = dataset_clean$HomeState,
  Occupation = dataset_clean$Occupation,
  SelfEmployed = dataset_clean$SelfEmployed,
  AnnualIncome = dataset_clean$AnnualIncome
)

#Product data set
Product_data <- data.frame(RecommendationId = dataset_clean$RecommendationId,
                           RequestId = dataset_clean$RequestId,
                           Date = dataset_clean$Date,
                           Alternative = dataset_clean$Alternative,
                           Underwriter = dataset_clean$Underwriter,
                           Package = dataset_clean$Package,
                           Time = dataset_clean$Time,
                           Super = dataset_clean$Super,
                           RolloverTaxRebate = dataset_clean$RolloverTaxRebate,
                           Life = dataset_clean$Life,
                           TPD = dataset_clean$TPD,
                           Trauma = dataset_clean$Trauma,
                           IP = dataset_clean$IP,
                           BE = dataset_clean$BE,
                           Severity = dataset_clean$Severity,
                           LifeCoverAmount = dataset_clean$LifeCoverAmount,
                           TPDCoverAmount = dataset_clean$TPDCoverAmount,
                           TraumaCoverAmount = dataset_clean$TraumaCoverAmount,
                           IPCoverAmount = dataset_clean$IPCoverAmount,
                           BECoverAmount = dataset_clean$BECoverAmount,
                           SeverityCoverAmount = dataset_clean$SeverityCoverAmount,
                           PremiumFrequency = dataset_clean$PremiumFrequency,
                           Premium = dataset_clean$Premium,
                           AnnualisedPremium = dataset_clean$AnnualisedPremium,
                           InsideSuperPremium = dataset_clean$InsideSuperPremium,
                           OutsideSuperPremium = dataset_clean$OutsideSuperPremium,
                           Indexation = dataset_clean$IndexationRate,
                           ExternalRef = dataset_clean$ExternalRef)


################################################################################


#descriptive statistics
# Create the dataset_neos
dataset_neos <- dataset %>% filter(Underwriter == "NEOS Life")

# Create the dataset_top5
top5_companies <- c("TAL", "AIA Australia", "Zurich", "NEOS Life", "OnePath")
dataset_top5 <- dataset %>% filter(Underwriter %in% top5_companies)

# Filter Gender to keep only Male and Female
dataset <- dataset %>% filter(Gender %in% c("Male", "Female"))

# Filter Premium Frequency to keep only "Monthly", "Yearly", "HalfYearly", "Quarterly"
dataset <- dataset %>% filter(PremiumFrequency %in% c("Monthly", "Yearly", "HalfYearly", "Quarterly"))

# Remove outliers for Annual Income and Premium
remove_outliers <- function(x) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  return(y)
}

dataset$AnnualIncome <- remove_outliers(dataset$AnnualIncome)
dataset$Premium <- remove_outliers(dataset$Premium)

# Age distribution
# Plot age distribution for NEOS and top 5 companies for each product (Life, TPD, Trauma, IP)
plot_age_distribution_life <- ggplot(dataset %>% filter(Life == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), y = AgeNext, fill = Underwriter == "NEOS Life")) +
  geom_violin() +
  ggtitle("Age Distribution for Life Product") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Underwriter", y = "Age Next")

plot_age_distribution_tpd <- ggplot(dataset %>% filter(TPD == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), y = AgeNext, fill = Underwriter == "NEOS Life")) +
  geom_violin() +
  ggtitle("Age Distribution for TPD Product") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Underwriter", y = "Age Next")

plot_age_distribution_trauma <- ggplot(dataset %>% filter(Trauma == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), y = AgeNext, fill = Underwriter == "NEOS Life")) +
  geom_violin() +
  ggtitle("Age Distribution for Trauma Product") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Underwriter", y = "Age Next")

plot_age_distribution_ip <- ggplot(dataset %>% filter(IP == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), y = AgeNext, fill = Underwriter == "NEOS Life")) +
  geom_violin() +
  ggtitle("Age Distribution for IP Product") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Underwriter", y = "Age Next")

# Combine age distribution plots into one graph
grid.arrange(plot_age_distribution_life, plot_age_distribution_tpd, plot_age_distribution_trauma, plot_age_distribution_ip, ncol = 2)

# Annual income distribution
# Plot annual income distribution for NEOS and top 5 companies for each product (Life, TPD, Trauma, IP)
plot_income_distribution_life <- ggplot(dataset %>% filter(Life == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), y = AnnualIncome, fill = Underwriter == "NEOS Life")) +
  geom_boxplot() +
  ggtitle("Annual Income Distribution for Life Product") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Underwriter", y = "Annual Income")

plot_income_distribution_tpd <- ggplot(dataset %>% filter(TPD == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), y = AnnualIncome, fill = Underwriter == "NEOS Life")) +
  geom_boxplot() +
  ggtitle("Annual Income Distribution for TPD Product") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Underwriter", y = "Annual Income")

plot_income_distribution_trauma <- ggplot(dataset %>% filter(Trauma == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), y = AnnualIncome, fill = Underwriter == "NEOS Life")) +
  geom_boxplot() +
  ggtitle("Annual Income Distribution for Trauma Product") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Underwriter", y = "Annual Income")

plot_income_distribution_ip <- ggplot(dataset %>% filter(IP == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), y = AnnualIncome, fill = Underwriter == "NEOS Life")) +
  geom_boxplot() +
  ggtitle("Annual Income Distribution for IP Product") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Underwriter", y = "Annual Income")

# Combine annual income distribution plots into one graph
grid.arrange(plot_income_distribution_life, plot_income_distribution_tpd, plot_income_distribution_trauma, plot_income_distribution_ip, ncol = 2)

# Smoker status distribution
# Plot smoker status distribution for NEOS and top 5 companies for each product (Life, TPD, Trauma, IP)
plot_smoker_status_life <- ggplot(dataset %>% filter(Life == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), fill = SmokerStatus)) +
  geom_bar(position = "dodge") +
  ggtitle("Smoker Status Distribution for Life Product") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Underwriter", fill = "Smoker Status")

plot_smoker_status_tpd <- ggplot(dataset %>% filter(TPD == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), fill = SmokerStatus)) +
  geom_bar(position = "dodge") +
  ggtitle("Smoker Status Distribution for TPD Product") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Underwriter", fill = "Smoker Status")

plot_smoker_status_trauma <- ggplot(dataset %>% filter(Trauma == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), fill = SmokerStatus)) +
  geom_bar(position = "dodge") +
  ggtitle("Smoker Status Distribution for Trauma Product") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Underwriter", fill = "Smoker Status")

plot_smoker_status_ip <- ggplot(dataset %>% filter(IP == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), fill = SmokerStatus)) +
  geom_bar(position = "dodge") +
  ggtitle("Smoker Status Distribution for IP Product") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Underwriter", fill = "Smoker Status")

# Combine smoker status plots into one graph
grid.arrange(plot_smoker_status_life, plot_smoker_status_tpd, plot_smoker_status_trauma, plot_smoker_status_ip, ncol = 2)

# Gender distribution
# Plot gender distribution for NEOS and top 5 companies for each product (Life, TPD, Trauma, IP)
plot_gender_distribution_life <- ggplot(dataset %>% filter(Life == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), fill = Gender)) +
  geom_bar(position = "dodge") +
  ggtitle("Gender Distribution for Life Product") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Underwriter", fill = "Gender")

plot_gender_distribution_tpd <- ggplot(dataset %>% filter(TPD == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), fill = Gender)) +
  geom_bar(position = "dodge") +
  ggtitle("Gender Distribution for TPD Product") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Underwriter", fill = "Gender")

plot_gender_distribution_trauma <- ggplot(dataset %>% filter(Trauma == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), fill = Gender)) +
  geom_bar(position = "dodge") +
  ggtitle("Gender Distribution for Trauma Product") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Underwriter", fill = "Gender")

plot_gender_distribution_ip <- ggplot(dataset %>% filter(IP == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), fill = Gender)) +
  geom_bar(position = "dodge") +
  ggtitle("Gender Distribution for IP Product") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Underwriter", fill = "Gender")

# Combine gender distribution plots into one graph
grid.arrange(plot_gender_distribution_life, plot_gender_distribution_tpd, plot_gender_distribution_trauma, plot_gender_distribution_ip, ncol = 2)

# Premium Frequency distribution
# Plot premium frequency distribution for NEOS and top 5 companies for each product (Life, TPD, Trauma, IP)
plot_premium_frequency_life <- ggplot(dataset %>% filter(Life == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), fill = PremiumFrequency)) +
  geom_bar(position = "dodge") +
  ggtitle("Premium Frequency Distribution for Life Product") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Underwriter", fill = "Premium Frequency")

plot_premium_frequency_tpd <- ggplot(dataset %>% filter(TPD == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), fill = PremiumFrequency)) +
  geom_bar(position = "dodge") +
  ggtitle("Premium Frequency Distribution for TPD Product") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Underwriter", fill = "Premium Frequency")

plot_premium_frequency_trauma <- ggplot(dataset %>% filter(Trauma == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), fill = PremiumFrequency)) +
  geom_bar(position = "dodge") +
  ggtitle("Premium Frequency Distribution for Trauma Product") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Underwriter", fill = "Premium Frequency")

plot_premium_frequency_ip <- ggplot(dataset %>% filter(IP == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), fill = PremiumFrequency)) +
  geom_bar(position = "dodge") +
  ggtitle("Premium Frequency Distribution for IP Product") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Underwriter", fill = "Premium Frequency")

# Combine premium frequency plots into one graph
grid.arrange(plot_premium_frequency_life, plot_premium_frequency_tpd, plot_premium_frequency_trauma, plot_premium_frequency_ip, ncol = 2)

# Premium distribution
# Plot premium distribution for NEOS and top 5 companies for each product (Life, TPD, Trauma, IP)
plot_premium_distribution_life <- ggplot(dataset %>% filter(Life == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), y = Premium, fill = Underwriter == "NEOS Life")) +
  geom_boxplot() +
  ggtitle("Premium Distribution for Life Product") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Underwriter", y = "Premium")

plot_premium_distribution_tpd <- ggplot(dataset %>% filter(TPD == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), y = Premium, fill = Underwriter == "NEOS Life")) +
  geom_boxplot() +
  ggtitle("Premium Distribution for TPD Product") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Underwriter", y = "Premium")

plot_premium_distribution_trauma <- ggplot(dataset %>% filter(Trauma == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), y = Premium, fill = Underwriter == "NEOS Life")) +
  geom_boxplot() +
  ggtitle("Premium Distribution for Trauma Product") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Underwriter", y = "Premium")

plot_premium_distribution_ip <- ggplot(dataset %>% filter(IP == "Yes"), aes(x = factor(Underwriter == "NEOS Life", labels = c("Top 5", "NEOS Life")), y = Premium, fill = Underwriter == "NEOS Life")) +
  geom_boxplot() +
  ggtitle("Premium Distribution for IP Product") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Underwriter", y = "Premium")

# Combine premium distribution plots into one graph
grid.arrange(plot_premium_distribution_life, plot_premium_distribution_tpd, plot_premium_distribution_trauma, plot_premium_distribution_ip, ncol = 2)

# Correlation
# Transform variables into numeric values
dataset <- dataset %>%
  mutate(
    NEOS = ifelse(Underwriter == "NEOS Life", 1, 0),
    Life = ifelse(Life == "Yes", 1, 0),
    TPD = ifelse(TPD == "Yes", 1, 0),
    Trauma = ifelse(Trauma == "Yes", 1, 0),
    IP = ifelse(IP == "Yes", 1, 0),
    Super = ifelse(Super == "Yes", 1, 0)
  )

# Select relevant columns for correlation analysis
correlation_data <- dataset %>% select(NEOS, Life, TPD, Trauma, IP, Super)

# Perform the correlation analysis
correlation_matrix <- cor(correlation_data, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Plot the correlation matrix
corrplot(correlation_matrix, method = "number", type = "upper", tl.col = "black", tl.srt = 45)

################################################################################


# Check data structure
str(dataset)

# Fill Alternative with a null value of NA
# Install and load the dplyr package
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
library(dplyr)
dataset <- dataset %>%
  mutate(Alternative = replace(Alternative, Alternative == "" | is.na(Alternative), NA))
dataset <- dataset %>%
  mutate(PremiumFrequency = replace(PremiumFrequency, PremiumFrequency == "" | is.na(PremiumFrequency), NA))


# Secondary data conversion
dataset_clean$Gender <- as.factor(ifelse(dataset_clean$Gender == "Male",0,1))
dataset_clean$Life <- as.factor(ifelse(dataset_clean$Life == "Yes", 1, 0))
dataset_clean$TPD <- as.factor(ifelse(dataset_clean$TPD == "Yes", 1, 0))
dataset_clean$Trauma <- as.factor(ifelse(dataset_clean$Trauma == "Yes", 1, 0))
dataset_clean$IP <- as.factor(ifelse(dataset_clean$IP == "Yes", 1, 0))
dataset_clean$SelfEmployed <- as.factor(ifelse(dataset_clean$SelfEmployed == "Yes", 1, 0))
dataset_clean$SmokerStatus <- as.factor(ifelse(dataset_clean$SmokerStatus == "Smoker",1,0))

# Converts State to a numeric variable
state_levels <- unique(dataset_clean$HomeState)
print(state_levels)
state_map <- c("VIC" = 1, "NSW" = 2, "QLD" = 3, "WA" = 4, "TAS" = 5, "SA" = 6, "ACT" = 7, "NT" = 8)
dataset_clean$State <- as.numeric(as.character(factor(dataset_clean$HomeState, levels = names(state_map), labels = state_map)))
View(dataset_clean)

#dataset_clean中underwriter=NEOS life的
neos_data <- subset(dataset_clean, Underwriter == "NEOS Life")
str(neos_data)
logistic_model <- glm(Life ~ AgeNext + AnnualIncome + Gender + SmokerStatus + SelfEmployed, 
                      data = neos_data, family = binomial)

summary(logistic_model )

# Find the top 5 insurers that appear most frequently
top_5_underwriters <- dataset_clean %>%
  group_by(Underwriter) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(5) %>%
  pull(Underwriter)

top_5_data <- dataset_clean %>%
  filter(Underwriter %in% top_5_underwriters)
str(top_5_data)
summary(top_5_data)
View(top_5_data)
nrow(top_5_data)

# Customer feature regression--LIfe was performed for the five insurers with the highest frequency of occurrence
top_5_data$Life <- as.numeric(as.character(top_5_data$Life))
modellife <- glm(Life ~ AgeNext + AnnualIncome + Gender + SmokerStatus + SelfEmployed + State, data = top_5_data, family = binomial)
summary(modellife)

model_lpm <- lm(Life ~ AgeNext + AnnualIncome + Gender + SmokerStatus + SelfEmployed + State, data = top_5_data)
summary(model_lpm)

# Customer feature regression- TPD was performed for the five most frequent insurers
modelTPD <- glm(TPD ~ AgeNext + AnnualIncome + Gender + SmokerStatus + SelfEmployed + State, data = top_5_data, family = binomial)
summary(modelTPD)

# Customer feature regression--TPD Customer feature regression--Trauma was performed on the five most frequent underwriters
modelTrauma <- glm(Trauma ~ AgeNext + AnnualIncome + Gender + SmokerStatus + SelfEmployed + State, data = top_5_data, family = binomial)
summary(modelTrauma)
# Customer feature regression--IP was performed for the five most frequent insurers
modelIP <- glm(IP ~ AgeNext + AnnualIncome + Gender + SmokerStatus + SelfEmployed + State, data = top_5_data, family = binomial)
summary(modelIP)
#All the variables were slightly related except that smokerstatus and selfemployed were completely unrelated


# top5 Product features
# Create a new column to indicate whether you are a Top 5 insurer
dataset_clean <- dataset_clean %>%
  mutate(Top5Underwriter = ifelse(Underwriter %in% top_5_underwriters, 1, 0))
dataset_clean <- dataset_clean %>%
  mutate(NEOSLifeUnderwriter = ifelse(Underwriter== "NEOS Life", 1, 0))
View(dataset_clean)


# Change frequency to 1234
frequency_map <- list("Yearly" = 1, "Annualised" = 1, "HalfYearly" = 2, "Quarterly" = 3, "Monthly" = 4)
dataset_clean$PremiumFrequency <- as.numeric(unlist(frequency_map[dataset_clean$PremiumFrequency]))
View(dataset_clean)
dataset_clean <- dataset_clean[!is.na(dataset_clean$PremiumFrequency), ]


# Regression equation
modeltop5product <- glm(Top5Underwriter ~ Premium + PremiumFrequency, data = dataset_clean, family = binomial)
summary(modeltop5product)

modelNEOSproduct<- glm(NEOSLifeUnderwriter ~ Premium + PremiumFrequency, data = dataset_clean, family = binomial)
summary(modelNEOSproduct)

################################################################################

#Regression
# Find the top 5 insurers that appear most frequently
top_5_underwriters <- dataset_clean %>%
  group_by(Underwriter) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(5) %>%
  pull(Underwriter)

top_5_data <- dataset_clean %>%
  filter(Underwriter %in% top_5_underwriters)
str(top_5_data)
summary(top_5_data)
View(top_5_data)
nrow(top_5_data)

selected_data1 <- neos_data %>% select( IP, TPD, Life, Trauma)
selected_data <- selected_data1 %>% mutate(across(everything(), as.numeric))

# Calculate the correlation matrix
correlation_matrix <- cor(selected_data, use = "complete.obs")
# Print the correlation matrix
print(correlation_matrix)

# Create the correlation matrix
correlation_matrix2 <- matrix(c(
  1.00000000, 0.03168622, -0.1159957, 0.02157556,
  0.03168622, 1.00000000,  0.7018884, 0.23334349,
  -0.11599569, 0.70188837,  1.0000000, 0.30056452,
  0.02157556, 0.23334349,  0.3005645, 1.00000000
), nrow = 4, ncol = 4, byrow = TRUE)

# Add row and column names
colnames(correlation_matrix2) <- c("IP", "TPD", "Life", "Trauma")
rownames(correlation_matrix2) <- c("IP", "TPD", "Life", "Trauma")
# Plot the correlation matrix
corrplot(correlation_matrix, method = "circle", type = "upper", 
         tl.col = "green", tl.srt = 45, 
         title = "Correlation Matrix")

# Linear regression for age vs. premiums
lm_age_life <- lm(AnnualisedPremium ~ AgeNext, data=top_5_data)
summary(lm_age_life)
# Linear regression for lifecoveramount vs. income
lm_income_life <- lm(LifeCoverAmount ~ AnnualIncome, data=top_5_data)
summary(lm_income_life)
# Linear regression for premium vs. income
lm_income_life <- lm(AnnualisedPremium ~ AnnualIncome, data=top_5_data)
summary(lm_income_life)
# Linear regression for IPcoveramount vs. income
lm_income_ip <- lm(IPCoverAmount ~ AnnualIncome, data=top_5_data)
summary(lm_income_ip)

# All of the above regression results indicate a positive relationship between the two

# Convert the Time column to a proper time format
data2 <- top_5_data %>%
  mutate(Time = format(strptime(Time, format = "%I:%M %p"), format = "%H:%M:%S")) # Convert Time to HH:MM:SS

# Add a dummy date to treat it as a daily recurring time series
data <- data2 %>%
  mutate(DateTime = as.POSIXct(paste("2024-07-17", Time), format = "%Y-%m-%d %H:%M:%S"))

# Select relevant columns and arrange by DateTime
data_ts <- data %>%
  select(DateTime, AnnualisedPremium) %>%
  arrange(DateTime)

# Create a time series object
# Assuming 24 hours * 60 minutes = 1440 periods in a day
ts_data <- ts(data_ts$AnnualisedPremium, frequency = 1440)

# Plot the time series
plot.ts(ts_data, main = "Time Series of Annualised Premiums", ylab = "Annualised Premium", xlab = "Time")

# Fit a time series model (e.g., ARIMA)
fit <- auto.arima(ts_data)

# Plot the forecast
forecasted <- forecast(fit, h = 1440) # Forecasting for the next day
autoplot(forecasted)

library(forecast)
library(ggplot2)

# Smoothing the data using a moving average
smoothed_data <- ma(ts_data, order = 5) # Adjust the order as needed

# Plot the smoothed data
plot.ts(smoothed_data, main = "Smoothed Time Series of Annualised Premiums", ylab = "Annualised Premium", xlab = "Time")

# Decompose the time series to identify trend, seasonal, and residual components
decomposed_data <- decompose(ts_data, type = "multiplicative")

# Plot the decomposed components
plot(decomposed_data)

################################################################################

#Hierarchical aggregation tree
# Remove unwanted columns
dataset_clean <- dataset_clean %>% select(-RecommendationId, -RequestId, -Package, -Alternative, -Time, -Date, -HomeState, -Occupation, -SelfEmployed, -ExternalRef,-RolloverTaxRebate,-AdviserID, 
                                          -IndexationRate, -Super, -LifeId, -CommissionStructure)

# Fill in the NA value
dataset_clean <- dataset_clean %>%
  mutate(across(where(is.character), ~replace_na(., ""))) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))
dataset_row_count <- sum(apply(dataset_clean, 1, function(row) any(is.na(row))))
print(paste("Number of rows with NA values after cleaning:", dataset_row_count))

# Converts some columns to factors
dataset_clean$Gender <- as.factor(ifelse(dataset_clean$Gender == "Male", 0, 1))
dataset_clean$Life <- as.factor(ifelse(dataset_clean$Life == "Yes", 0, 1))
dataset_clean$TPD <- as.factor(ifelse(dataset_clean$TPD == "Yes", 0, 1))
dataset_clean$Trauma <- as.factor(ifelse(dataset_clean$Trauma == "Yes", 0, 1))
dataset_clean$IP <- as.factor(ifelse(dataset_clean$IP == "Yes", 0, 1))
dataset_clean$BE <- as.factor(ifelse(dataset_clean$BE == "Yes", 0, 1))
dataset_clean$Severity <- as.factor(ifelse(dataset_clean$Severity == "Yes", 0, 1))
dataset_clean$SmokerStatus <- as.factor(ifelse(dataset_clean$SmokerStatus == "Non-Smoker", 0, 1))
dataset_clean$PremiumFrequency <- as.factor(dataset_clean$PremiumFrequency)
str(dataset_clean)

dataset_clean_na_count_after_conversion <- sum(is.na(dataset_clean))
print(paste("Number of NA values after conversion:", dataset_clean_na_count_after_conversion))

#Top 5 categories
# Find the top 5 insurers that appear most frequently
top_5_underwriters <- dataset_clean %>%
  group_by(Underwriter) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(5) %>%
  pull(Underwriter)

print("Top 5 underwriters:")
print(top_5_underwriters)

# Filter data to keep only the top 5 underwriters
top_5_data <- dataset_clean %>%
  filter(Underwriter %in% top_5_underwriters)
top5_na_count <- sum(apply(top_5_data, 1, function(row) any(is.na(row))))
print(paste("Number of rows with NA values in top_5_data:", top5_na_count))

# Maps the top 5 values in the Underwriter column to numeric values
mapping <- c("NA" = 0, "TAL" = 1, "AIA Australia" = 2, "NEOS Life" = 3, "Zurich" = 4, "OnePath" = 5)
top_5_data$Underwriter <- as.numeric(mapping[top_5_data$Underwriter])
str(top_5_data)
head(top_5_data)
top5_na_count <- sum(apply(top_5_data, 1, function(row) any(is.na(row))))
print(paste("Number of rows with NA values in top_5_data after mapping:", top5_na_count))

library(tidyr)

# Delete rows with NA values
top_5_data <- drop_na(top_5_data)
print(paste("Number of rows with NA values after dropping:", sum(is.na(top_5_data))))
nan_count <- sum(is.nan(as.matrix(top_5_data)))
inf_count <- sum(is.infinite(as.matrix(top_5_data)))
print(paste("Number of NaN values:", nan_count))
print(paste("Number of Inf values:", inf_count))
str(top_5_data)

# Converts all columns to numeric type
top_5_data <- top_5_data %>%
  mutate(across(everything(), as.numeric))
na_count_after_conversion <- sum(is.na(top_5_data))
print(paste("Number of NA values after conversion:", na_count_after_conversion))



#NEOS category

# Extract rows where the Underwriter is NEOS
neos_data <- dataset_clean %>% 
  filter(Underwriter == "NEOS Life")

# Display the result
print(neos_data)


# Factor conversion to numeric value
variables_to_convert <- c( "Gender", "SmokerStatus", "Life", "TPD", "Trauma", "IP", "BE", "Severity")

# Loop through each variable and transform it
for (variable in variables_to_convert) {
  neos_data[[variable]] <- as.numeric(as.character(neos_data[[variable]]))
}



#NEOS customer characteristics analysis
neos_data$Gender <- as.numeric(as.character(neos_data$Gender))
neos_data$SmokerStatus <- as.numeric(as.character(neos_data$SmokerStatus))

# Standardize feature columns
scaled_features <- scale(neos_data[, c("AgeNext", "AnnualIncome", "Gender", "SmokerStatus")])

# Randomly sample 100 samples
set.seed(123) 
sample_indices <- sample(nrow(neos_data), 100)
sampled_data <- neos_data[sample_indices, ]
dist_matrix <- dist(scaled_features[sample_indices, ])
hc <- hclust(dist_matrix, method = "ward.D")
cluster_assignments <- cutree(hc, k = 4)
labels <- as.character(cluster_assignments)

plot(hc, labels = labels, main = "Hierarchical Clustering Dendrogram for NEOS Customers",
     xlab = "Customer Characteristic", ylab = "Height", sub = "", cex = 0.9)
rect.hclust(hc, k = 4, border = c("red", "blue", "green", "purple"))

cluster_assignments <- cutree(hc, k = 4)
sampled_data$Cluster <- factor(cluster_assignments)

# Generate heat maps
pheatmap(scaled_features[sample_indices, ],
         cluster_rows = hc,
         cluster_cols = FALSE,
         main = "Heatmap of NEOS Products Clusters",
         scale = "row")

# Generate scatter plots
ggplot(sampled_data, aes(x = AgeNext, y = AnnualIncome, color = Cluster)) +
  geom_point() +
  labs(title = "Scatter Plot of NEOS Customers Clusters",
       x = "Age Next",
       y = "Annual Income") +
  theme_minimal()


#Top5 customer characteristics analysis
# Standardize feature columns
scaled_features <- scale(top_5_data[, c("AgeNext", "AnnualIncome", "Gender", "SmokerStatus")])

# Randomly sample 100 samples
set.seed(123) 
sample_indices <- sample(nrow(top_5_data), 100)
sampled_data <- top_5_data[sample_indices, ]
dist_matrix <- dist(scaled_features[sample_indices, ])
hc <- hclust(dist_matrix, method = "ward.D")

# Draw a clustering tree
plot(hc, labels = top_5_data$Product_Code[sample_indices], main = "Hierarchical Clustering Dendrogram for Top5-Customers",
     xlab = "Product Type", ylab = "Height", sub = "", cex = 0.9)
rect.hclust(hc, k = 4, border = c("red", "blue", "green", "purple")) # 使用不同的颜色
cluster_assignments <- cutree(hc, k = 4)
labels <- as.character(cluster_assignments)
plot(hc, labels = labels, main = "Hierarchical Clustering Dendrogram for Top5-Customers",
     xlab = "Product Type", ylab = "Height", sub = "", cex = 0.9)
rect.hclust(hc, k = 4, border = c("red", "blue", "green", "purple"))
cluster_assignments <- cutree(hc, k = 4)
sampled_data$Cluster <- factor(cluster_assignments)

# Generate heat maps
pheatmap(scaled_features[sample_indices, ],
         cluster_rows = hc,
         cluster_cols = FALSE,
         main = "Heatmap of Top5-Customers Clusters",
         scale = "row")

# Generate scatter plots
ggplot(sampled_data, aes(x = AgeNext, y = AnnualIncome, color = Cluster)) +
  geom_point() +
  labs(title = "Scatter Plot of Top5-Customers Clusters",
       x = "Age Next",
       y = "Annual Income") +
  theme_minimal()



#NEOS product feature analysis
# Create a new column 'Product_Code', initialized to 0
neos_data$Product_Code <- 0

# Assign a value to each product using binary encoding
neos_data$Product_Code <- neos_data$Product_Code + ifelse(neos_data$Life == "1", 1, 0)
neos_data$Product_Code <- neos_data$Product_Code + ifelse(neos_data$TPD == "1", 2, 0)
neos_data$Product_Code <- neos_data$Product_Code + ifelse(neos_data$Trauma == "1", 4, 0)
neos_data$Product_Code <- neos_data$Product_Code + ifelse(neos_data$IP == "1", 8, 0)
neos_data$Product_Code <- neos_data$Product_Code + ifelse(neos_data$BE == "1", 16, 0)
neos_data$Product_Code <- neos_data$Product_Code + ifelse(neos_data$Severity == "1", 32, 0)

# Create a product portfolio name
neos_data$Product_Combination <- paste(
  ifelse(neos_data$Life == "1", "Life", ""),
  ifelse(neos_data$TPD == "1", "TPD", ""),
  ifelse(neos_data$Trauma == "1", "Trauma", ""),
  ifelse(neos_data$IP == "1", "IP", ""),
  ifelse(neos_data$BE == "1", "BE", ""),
  ifelse(neos_data$Severity == "1", "Severity", ""),
  sep = "+"
)
neos_data$Product_Combination <- gsub("\\++", "+", neos_data$Product_Combination)
neos_data$Product_Combination <- gsub("^\\+|\\+$", "", neos_data$Product_Combination)
head(neos_data)

# Randomly sample 100 samples
set.seed(123)
sample_indices <- sample(nrow(neos_data), 100)
sampled_data <- neos_data[sample_indices, ]
dist_matrix <- dist(as.numeric(sampled_data$Product_Code))
hc <- hclust(dist_matrix, method = "ward.D")
cluster_assignments <- cutree(hc, k = 4)
sampled_data$Cluster <- factor(cluster_assignments)

# Define all possible product combinations
possible_combinations <- c(
  "Life", "TPD", "Trauma", "IP", "BE", "Severity",
  "Life+TPD", "Life+Trauma", "Life+IP", "Life+BE", "Life+Severity",
  "TPD+Trauma", "TPD+IP", "TPD+BE", "TPD+Severity",
  "Trauma+IP", "Trauma+BE", "Trauma+Severity",
  "IP+BE", "IP+Severity", "BE+Severity",
  "Life+TPD+Trauma", "Life+TPD+IP", "Life+TPD+BE", "Life+TPD+Severity",
  "Life+Trauma+IP", "Life+Trauma+BE", "Life+Trauma+Severity",
  "Life+IP+BE", "Life+IP+Severity", "Life+BE+Severity",
  "TPD+Trauma+IP", "TPD+Trauma+BE", "TPD+Trauma+Severity",
  "TPD+IP+BE", "TPD+IP+Severity", "TPD+BE+Severity",
  "Trauma+IP+BE", "Trauma+IP+Severity", "Trauma+BE+Severity",
  "IP+BE+Severity",
  "Life+TPD+Trauma+IP", "Life+TPD+Trauma+BE", "Life+TPD+Trauma+Severity",
  "Life+TPD+IP+BE", "Life+TPD+IP+Severity", "Life+TPD+BE+Severity",
  "Life+Trauma+IP+BE", "Life+Trauma+IP+Severity", "Life+Trauma+BE+Severity",
  "Life+IP+BE+Severity",
  "TPD+Trauma+IP+BE", "TPD+Trauma+IP+Severity", "TPD+Trauma+BE+Severity",
  "TPD+IP+BE+Severity",
  "Trauma+IP+BE+Severity",
  "Life+TPD+Trauma+IP+BE", "Life+TPD+Trauma+IP+Severity", "Life+TPD+Trauma+BE+Severity",
  "Life+TPD+IP+BE+Severity",
  "Life+Trauma+IP+BE+Severity",
  "TPD+Trauma+IP+BE+Severity",
  "Life+TPD+Trauma+IP+BE+Severity"
)

# Create a vector of product portfolio names
product_combinations <- tapply(sampled_data$Product_Combination, sampled_data$Cluster, function(x) {
  comb <- paste(unique(x), collapse = ", ")
  if(comb == "") comb <- "No Combination"
  return(comb)
})

product_combinations <- lapply(1:4, function(i) {
  if(i %in% names(product_combinations)) {
    return(product_combinations[[as.character(i)]])
  } else {
    return("No Combination")
  }
})

product_combinations <- unlist(product_combinations)
print(product_combinations)

plot(hc, labels = neos_data$Product_Code[sample_indices], main = "Hierarchical Clustering Dendrogram for NEOS Products",
     xlab = "Product Type", ylab = "Height", sub = "", cex = 0.9)

rect.hclust(hc, k = 4, border = c("red", "blue", "green", "purple"))

legend("topright", legend = paste0(product_combinations), fill = c("green","red", "purple","blue"), bty = "n", cex = 0.8, inset = c(-0.9, 0))

# Generate heat maps
scaled_features <- scale(neos_data[, c("LifeCoverAmount", "TPDCoverAmount", "TraumaCoverAmount", "IPCoverAmount",
                                       "Premium","AnnualisedPremium","InsideSuperPremium","OutsideSuperPremium")])
pheatmap(scaled_features[sample_indices, ],
         cluster_rows = hc,
         cluster_cols = FALSE,
         main = "Heatmap of NEOS Products Clusters",
         scale = "row",
         cellwidth = 50,
)

#Top5 product feature analysis
# Create a new column 'Product_Code', initialized to 0
top_5_data$Product_Code <- 0
top_5_data$Product_Code <- top_5_data$Product_Code + ifelse(top_5_data$Life == "1", 1, 0)
top_5_data$Product_Code <- top_5_data$Product_Code + ifelse(top_5_data$TPD == "1", 2, 0)
top_5_data$Product_Code <- top_5_data$Product_Code + ifelse(top_5_data$Trauma == "1", 4, 0)
top_5_data$Product_Code <- top_5_data$Product_Code + ifelse(top_5_data$IP == "1", 8, 0)

top_5_data$Product_Combination <- paste(
  ifelse(top_5_data$Life == "1", "Life", ""),
  ifelse(top_5_data$TPD == "1", "TPD", ""),
  ifelse(top_5_data$Trauma == "1", "Trauma", ""),
  ifelse(top_5_data$IP == "1", "IP", ""),
  sep = "+"
)
top_5_data$Product_Combination <- gsub("\\++", "+", top_5_data$Product_Combination)
top_5_data$Product_Combination <- gsub("^\\+|\\+$", "", top_5_data$Product_Combination)

head(top_5_data)

set.seed(123) 
sample_indices <- sample(nrow(top_5_data), 100)
sampled_data <- top_5_data[sample_indices, ]

dist_matrix <- dist(as.numeric(sampled_data$Product_Code))
hc <- hclust(dist_matrix, method = "ward.D")
cluster_assignments <- cutree(hc, k = 4)
sampled_data$Cluster <- factor(cluster_assignments)
product_combinations <- tapply(sampled_data$Product_Combination, sampled_data$Cluster, function(x) paste(unique(x), collapse = ", "))
plot(hc, labels = top_5_data$Product_Code[sample_indices], main = "Hierarchical Clustering Dendrogram for Top5-Products",
     xlab = "Product Type", ylab = "Height", sub = "", cex = 0.9)
rect.hclust(hc, k = 4, border = c("purple","green", "blue", "red"))
legend("topright", legend = paste0("Cluster ", 1:4, ": ", product_combinations), fill = c( "red", "blue", "green", "purple" ), bty = "n", cex = 0.8, border = "black", inset = c(-0.4, 0))


# Standardize feature columns
scaled_features <- scale(top_5_data[, c("LifeCoverAmount", "TPDCoverAmount", "TraumaCoverAmount", "IPCoverAmount",
                                        "Premium","AnnualisedPremium","InsideSuperPremium","OutsideSuperPremium")])

# Generate heat maps
pheatmap(scaled_features[sample_indices, ],
         cluster_rows = hc,
         cluster_cols = FALSE,
         main = "Heatmap of Top5-Products Clusters",
         scale = "row",
         cellwidth = 50,
)


################################################################################

# Classification Decision Tree
# Identify and remove extreme values
dataset_clean1 <- subset(dataset, AnnualIncome >= lower_bound & AnnualIncome <= upper_bound)
colnames(dataset_clean1)

#useful variables
dataset_use <- dataset_clean1 %>% 
  select(-RecommendationId, -RequestId, -Date, -Alternative, -Package, -Time, -Super,
         -RolloverTaxRebate, -CommissionStructure, -LifeId, -AdviserID, -Occupation,
         -IndexationRate, -Super, -Occupation, -SelfEmployed,-AnnualisedPremium, 
         -InsideSuperPremium,-OutsideSuperPremium, -Premium, -ExternalRef,-IndexationRate)


dataset_use$Gender <- as.factor(ifelse(dataset_use$Gender == "Male",1,0))
dataset_use$Life <- as.factor(ifelse(dataset_use$Life == "Yes",1,0))
dataset_use$TPD <- as.factor(ifelse(dataset_use$TPD == "Yes",1,0))
dataset_use$Trauma <- as.factor(ifelse(dataset_use$Trauma == "Yes",1,0))
dataset_use$IP <- as.factor(ifelse(dataset_use$IP == "Yes",1,0))
dataset_use$BE <- as.factor(ifelse(dataset_use$BE == "Yes",1,0))
dataset_use$Severity <- as.factor(ifelse(dataset_use$Severity == "Yes",1,0))
dataset_use$SmokerStatus <- as.factor(ifelse(dataset_use$SmokerStatus == "Smoker",1,0))

str(dataset_use)

# Neos life product analysis
dataset_neos_life <- dataset_use %>% 
  filter(Underwriter == "NEOS Life")

colnames(dataset_neos_life)

dataset_neos_life <- dataset_use %>% 
  select(AgeNext, Gender, SmokerStatus, AnnualIncome, Life, LifeCoverAmount)

colnames(dataset_neos_life)
summary(dataset_neos_life)
str(dataset_neos_life)

required_packages <- c("readxl", "tidyverse", "rpart", "rpart.plot", "caret", "gains", "pROC", "adabag", "xgboost", "dplyr")

# Function to check for missing packages and install them if necessary
check_and_install_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Check and install required packages
check_and_install_packages(required_packages)

# Set seed for reproducibility
set.seed(1)

# Partition the data into training (70%) and validation (30%) sets
train_index <- createDataPartition(dataset_neos_life$Life, p = 0.7, list = FALSE)
train_data <- dataset_neos_life[train_index, ]
validation_data <- dataset_neos_life[-train_index, ]

# Train an rpart decision tree to predict the HELOC column
default_tree <- rpart(Life ~ AgeNext + Gender + SmokerStatus + AnnualIncome + LifeCoverAmount , data = train_data, method = "class")

# Display a coloured plot of default_tree using prp
prp(default_tree, 
    type = 2, 
    extra = 2, 
    fallen.leaves = TRUE, 
    box.palette = "Blues", 
    varlen = 0, 
    faclen = 0)

summary(default_tree)

# Neos TPD product analysis
dataset_neos_TPD <- dataset_use %>% 
  select(AgeNext, Gender, SmokerStatus, AnnualIncome, TPD, TPDCoverAmount)

colnames(dataset_neos_TPD)
summary(dataset_neos_TPD)
str(dataset_neos_TPD)

train_index1 <- createDataPartition(dataset_neos_TPD$TPD, p = 0.7, list = FALSE)
train_data1 <- dataset_neos_TPD[train_index, ]
validation_data1 <- dataset_neos_TPD[-train_index, ]

default_tree1 <- rpart(TPD ~ AgeNext + Gender + SmokerStatus + AnnualIncome + TPDCoverAmount, data = train_data1, method = "class")

prp(default_tree1, 
    type = 2, 
    extra = 2, 
    fallen.leaves = TRUE, 
    box.palette = "Blues", 
    varlen = 0, 
    faclen = 0)

summary(default_tree1)

# Neos  product Trauma analysis
dataset_neos_Trauma <- dataset_use %>% 
  select(AgeNext, Gender, SmokerStatus, AnnualIncome, Trauma, TraumaCoverAmount)

colnames(dataset_neos_Trauma)
summary(dataset_neos_Trauma)
str(dataset_neos_Trauma)

train_index2 <- createDataPartition(dataset_neos_Trauma$Trauma, p = 0.7, list = FALSE)
train_data2 <- dataset_neos_Trauma[train_index, ]
validation_data2 <- dataset_neos_Trauma[-train_index, ]

default_tree2 <- rpart(Trauma ~ AgeNext + Gender + SmokerStatus + AnnualIncome + TraumaCoverAmount, data = train_data2, method = "class")

prp(default_tree2, 
    type = 2, 
    extra = 2, 
    fallen.leaves = TRUE, 
    box.palette = "Blues", 
    varlen = 0, 
    faclen = 0)

summary(default_tree2)

# Neos  product IP analysis
dataset_neos_IP <- dataset_use %>% 
  select(AgeNext, Gender, SmokerStatus, AnnualIncome, IP, IPCoverAmount)

colnames(dataset_neos_IP)
summary(dataset_neos_IP)
str(dataset_neos_IP)

train_index3 <- createDataPartition(dataset_neos_IP$IP, p = 0.7, list = FALSE)
train_data3 <- dataset_neos_IP[train_index, ]
validation_data3 <- dataset_neos_IP[-train_index, ]

default_tree3 <- rpart(IP ~ AgeNext + Gender + SmokerStatus + AnnualIncome + IPCoverAmount, data = train_data3, method = "class")

prp(default_tree3, 
    type = 2, 
    extra = 2, 
    fallen.leaves = TRUE, 
    box.palette = "Blues", 
    varlen = 0, 
    faclen = 0)

summary(default_tree3)