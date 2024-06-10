
library(dplyr)
library(ggplot2)
library(tidyr)

resolutions <- read.csv('Full_New_Years_Resolutions_Gender_Age_Breakdown-2.csv')
print(head(resolutions))


# Print summary statistics for resolutions dataset.
summary(resolutions)



# Convert to data frame and remove empty rows.
res = as.data.frame(resolutions)
res_cleaned <- res %>% filter_all(all_vars(!is.na(.)))

# Pivot gender columns to rows.
res_long <- pivot_longer(res_cleaned, Male:All.Gender, names_to = "sex", values_to = "percent_by_gender")

# Create plot of resolution percentages for all gender.
p <- ggplot(data = res_long, aes(x = Resolution, y = percent_by_gender, fill = sex)) +
  geom_bar(stat="identity", position = "dodge")

p + coord_flip()


# Pivot age columns to rows.
res_long_age <- pivot_longer(res_long, X18.24:X65., names_to = "age", values_to = "percent_by_age")
# Create plot of resolution percentages for age groups.
p_age <- ggplot(data = res_long_age, aes(x = Resolution, y = percent_by_age, fill = age)) +
  geom_bar(stat="identity", position = "dodge")

p_age + coord_flip()

res_cleaned

# Find average number of people who make each resolution.
mean(res_cleaned$All.Gender)

#Find most popular resolution overall.
max_percent <- max(res_cleaned$All.Gender)
max_position <- (which(res_cleaned$All.Gender == max_percent))
max_res <- res_cleaned$Resolution[1]
print(paste("The most popular resolution is ", max_res))


