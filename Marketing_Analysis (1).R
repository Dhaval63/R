# Importing library packages
library(readr)
library(psych)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(reshape2)


#Loading the marketing dataset
marketing_data <- read_csv("D:/digital_marketing_campaign_dataset.csv")
View(marketing_data)

# Showing the marketing data
head(marketing_data)

# Checking the structure of the dataset to identify data types
str(marketing_data)


# Checking for missing values
missing_values <- colSums(is.na(marketing_data))
print(missing_values)


# Summary of the entire dataset
summary(marketing_data)


# Descriptive Statistics
describe(marketing_data)

#Visualization
# Age Distribution of Customers
ggplot(marketing_data, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution of Customers", x = "Age", y = "Count")



# Income Distribution of Customers
income_summary <- data.frame(
  Income = seq(min(marketing_data$Income), max(marketing_data$Income), by = 5000),
  Count = sapply(seq(min(marketing_data$Income), max(marketing_data$Income), by = 5000),
                 function(x) sum(marketing_data$Income >= x & marketing_data$Income < x + 5000))
)

ggplot(income_summary, aes(x = Income, y = Count)) + 
  geom_line(color = "green", size = 1) +
  geom_text(aes(label = Count), vjust = -0.5, color = "black") +  # Add data values as labels
  labs(title = "Income Distribution of Customers", x = "Income", y = "Count") +
  theme_minimal()



#Campaign Channel vs Click-Through Rate
ggplot(marketing_data, aes(x = CampaignChannel, y = ClickThroughRate, fill = CampaignChannel)) + 
  geom_bar(stat = "summary", fun = "mean", show.legend = FALSE) +
  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 3)), vjust = -0.5) + # Display mean values on bars
  labs(title = "Campaign Channel vs Click-Through Rate", x = "Campaign Channel", y = "Average CTR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Distribution of Gender
marketing_data %>%
  count(Gender) %>%
  ggplot(aes(x = Gender, y = n, fill = Gender)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +  # Add data labels above bars
  labs(title = "Distribution of Gender", x = "Gender", y = "Count") +
  theme_minimal()



#Campaign Channel Usage
channel_summary <- as.data.frame(table(marketing_data$CampaignChannel))
colnames(channel_summary) <- c("CampaignChannel", "Count")

channel_summary$Percentage <- paste0(round((channel_summary$Count / sum(channel_summary$Count)) * 100, 1), "%")

ggplot(channel_summary, aes(x = "", y = Count, fill = CampaignChannel)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5)) +
  labs(title = "Campaign Channel Usage", x = NULL, y = NULL) +
  theme_void() +
  theme(legend.title = element_blank())



# Correlation Matrix
melted_cor_matrix <- melt(cor_matrix)

ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black") +  
  scale_fill_gradient2(midpoint = 0, low = "blue", high = "red", mid = "white") +
  theme_minimal() +
  labs(title = "Correlation Matrix")




# Linear regression model
LR_model <- lm(Conversion + Age ~ AdSpend + Income + ConversionRate + ClickThroughRate + Age, data = marketing_data)

# Display the summary 
summary(LR_model)

predictions <- predict(LR_model, marketing_data)
marketing_data$predictions <- predictions

# Scatter plot of AdSpend vs. Actual Conversion 
ggplot(marketing_data, aes(x = AdSpend, y = Conversion)) +
  geom_point(color = "blue") + 
  geom_smooth(method = "lm", color = "red") +
  labs(title = "AdSpend vs. Conversion with Regression Line",
       x = "AdSpend", y = "Conversion")

# Residual plot
ggplot(marketing_data, aes(x = predictions, y = Conversion - predictions)) +
  geom_point(color = "green") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals")



# Q-Q plot for residuals
residuals <- marketing_data$AdSpend - predictions
qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red")



# One way anova testing
anova_result <- aov(Conversion ~ Age, data = marketing_data)

# summary of the ANOVA testing
summary(anova_result)

# Age vs Conversion with ANOVA Trend Line
ggplot(marketing_data, aes(x = Age, y = Conversion)) +
  geom_point(color = "red") +  # Adds data points
  geom_smooth(method = "loess", color = "blue", se = FALSE) +  # Adds a smooth line
  labs(title = "Age vs Conversion with ANOVA Trend Line",
       x = "Age",
       y = "Conversion Rate") +
  theme_minimal()



# T-Testing
subset_data <- marketing_data[marketing_data$CampaignType %in% c("Awareness", "Conversion"), ]
t_test_income <- t.test(Income ~ CampaignType, data = subset_data)
print(t_test_income)


#Mean Income by Campaign Type
mean_data <- subset_data %>%
  group_by(CampaignType) %>%
  summarise(
    mean_income = mean(Income, na.rm = TRUE),
    sem_income = sd(Income, na.rm = TRUE) / sqrt(n())
  )

ggplot(mean_data, aes(x = CampaignType, y = mean_income, fill = CampaignType)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_errorbar(aes(ymin = mean_income - sem_income, ymax = mean_income + sem_income), 
                width = 0.2, position = position_dodge(0.6)) +
  geom_text(aes(label = round(mean_income, 2)), 
            vjust = -0.5,  
            color = "black", 
            size = 5) +    
  labs(title = "Mean Income by Campaign Type",
       x = "Campaign Type",
       y = "Mean Income") +
  theme_minimal() +
  scale_fill_manual(values = c("Awareness" = "skyblue", "Conversion" = "orange"))




# Chi-square testing
contingency_table <- table(marketing_data$Gender, marketing_data$CampaignChannel)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)







