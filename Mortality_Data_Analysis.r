
# Import the data
data <- read.delim("Mortality_Data_1.txt")
View(data)

# Let's figure out what kind of variables are contained in this dataset
names(data)

# Check what kind of levels are contained in the following key variables.
unique(data$Ten.Year.Age.Groups)
unique(data$Gender)
unique(data$Census.Region)
unique(data$Year)

# Store the desired levels for each variables
ages=unique(data$Ten.Year.Age.Groups)[1:11]
genders=unique(data$Gender)[1:2]
regions=unique(data$Census.Region)[1:4]
years=unique(data$Year)[1:22]


subdata=subset(data,Year==years[1] & Census.Region==regions[1])
deathrates=matrix(NA,length(ages),length(genders))
colnames(deathrates)=genders
row.names(deathrates)=ages
for (i in 1:length(genders)){
  subdatai=(subset(subdata,Gender==genders[i]))
  for (j in 1:length(ages)){
    subdataij=subset(subdatai,Ten.Year.Age.Groups==ages[j])
    deathrates[j,i]=subdataij$Deaths/as.numeric(subdataij$Population)*100
  }
}
deathrates

# Plot Death Rates by Age and Gender
library(reshape2)
deathrates_long <- melt(deathrates, varnames = c("Age", "Gender"), value.name = "DeathRate")

library(ggplot2)
ggplot(deathrates_long, aes(x = Age, y = DeathRate, color = Gender, group = Gender)) +
  geom_line(size = 1.2) +
  labs(title = paste("Death Rates (diseases of the respiratory system) \n by Age and Gender \n on Year ",years[1],", ",regions[1],sep=""), x = "Age Groups", y = "Death Rates") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Second option could be, we can draw death rates vs year with different lines for genders.
# Set the other two predictor variables (region and age) at some specific levels.
# death rates presented in %.

subdata=subset(data,Ten.Year.Age.Groups==ages[5] & Census.Region==regions[2])
deathrates=matrix(NA,length(years),length(genders))
colnames(deathrates)=genders
row.names(deathrates)=years
for (i in 1:length(genders)){
  subdatai=(subset(subdata,Gender==genders[i]))
  for (j in 1:length(years)){
    subdataij=subset(subdatai,Year==years[j])
    deathrates[j,i]=subdataij$Deaths/as.numeric(subdataij$Population)*100
  }
}
deathrates

# Plot Death Rates by Year and Gender
library(reshape2)
deathrates_long <- melt(deathrates, varnames = c("Year", "Gender"), value.name = "DeathRate")

library(ggplot2)
ggplot(deathrates_long, aes(x = Year, y = DeathRate, color = Gender, group = Gender)) +
  geom_line(size = 1.2) +
  labs(title = paste("Death Rates (diseases of the respiratory system) \n by Year and Gender \n for Age ",ages[5],", ",regions[2],sep=""), x = "Year", y = "Death Rates") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Subset data for a specific year and region
subdata = subset(data, Year == years[1] & Census.Region == regions[1])

# Create matrix for storing death rates
deathrates = matrix(NA, length(ages), length(genders))
colnames(deathrates) = genders
row.names(deathrates) = ages

# Calculate death rates by age and gender
for (i in 1:length(genders)) {
  subdatai = subset(subdata, Gender == genders[i])
  for (j in 1:length(ages)) {
    subdataij = subset(subdatai, Ten.Year.Age.Groups == ages[j])
    deathrates[j, i] = subdataij$Deaths / as.numeric(subdataij$Population) * 100
  }
}

# Convert to long format for ggplot
library(reshape2)
deathrates_long = melt(deathrates, varnames = c("Age", "Gender"), value.name = "DeathRate")

# Plot Death Rates by Age and Gender
library(ggplot2)



ggplot(deathrates_long, aes(x = Age, y = DeathRate, color = Gender, group = Gender)) +
  geom_line(size = 1.2) +
  labs(title = paste("Death Rates by Age and Gender\nYear:", years[1], ", Region:", regions[1]),
       x = "Age Groups", y = "Death Rates (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Subset data for a specific gender and age group
subdata = subset(data, Gender == genders[1] & Ten.Year.Age.Groups == ages[5])

# Create matrix for storing death rates by year and region
deathrates = matrix(NA, length(years), length(regions))
colnames(deathrates) = regions
row.names(deathrates) = years

# Calculate death rates by year and region
for (i in 1:length(regions)) {
  subdatai = subset(subdata, Census.Region == regions[i])
  for (j in 1:length(years)) {
    subdataij = subset(subdatai, Year == years[j])
    deathrates[j, i] = subdataij$Deaths / as.numeric(subdataij$Population) * 100
  }
}

# Convert to long format for ggplot
deathrates_long = melt(deathrates, varnames = c("Year", "Region"), value.name = "DeathRate")

# Line Plot: Death Rates by Year and Region
ggplot(deathrates_long, aes(x = Year, y = DeathRate, color = Region, group = Region)) +
  geom_line(size = 1.2) +
  labs(title = paste("Death Rates by Region and Year\nGender:", genders[1], ", Age Group:", ages[5]),
       x = "Year", y = "Death Rates (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Subset data for a specific gender and age group
subdata = subset(data, Gender == genders[2] & Ten.Year.Age.Groups == ages[5])

# Create matrix for storing death rates by year and region
deathrates = matrix(NA, length(years), length(regions))
colnames(deathrates) = regions
row.names(deathrates) = years

# Calculate death rates by year and region
for (i in 1:length(regions)) {
  subdatai = subset(subdata, Census.Region == regions[i])
  for (j in 1:length(years)) {
    subdataij = subset(subdatai, Year == years[j])
    deathrates[j, i] = subdataij$Deaths / as.numeric(subdataij$Population) * 100
  }
}

# Convert to long format for ggplot
deathrates_long = melt(deathrates, varnames = c("Year", "Region"), value.name = "DeathRate")

# Line Plot: Death Rates by Year and Region
ggplot(deathrates_long, aes(x = Year, y = DeathRate, color = Region, group = Region)) +
  geom_line(size = 1.2) +
  labs(title = paste("Death Rates by Region and Year\nGender:", genders[2], ", Age Group:", ages[5]),
       x = "Year", y = "Death Rates (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







# Subset data for a specific region and age group
subdata = subset(data, Census.Region == regions[2] & Ten.Year.Age.Groups == ages[6])

# Create matrix for storing death rates by gender and year
deathrates = matrix(NA, length(years), length(genders))
colnames(deathrates) = genders
row.names(deathrates) = years

# Calculate death rates by gender and year
for (i in 1:length(genders)) {
  subdatai = subset(subdata, Gender == genders[i])
  for (j in 1:length(years)) {
    subdataij = subset(subdatai, Year == years[j])
    deathrates[j, i] = subdataij$Deaths / as.numeric(subdataij$Population) * 100
  }
}

# Convert to long format for ggplot
deathrates_long = melt(deathrates, varnames = c("Year", "Gender"), value.name = "DeathRate")

# Line Plot: Death Rates by Year and Gender
ggplot(deathrates_long, aes(x = Year, y = DeathRate, color = Gender, group = Gender)) +
  geom_line(size = 1.2) +
  labs(title = paste("Death Rates by Gender and Year\nRegion:", regions[2], ", Age Group:", ages[6]),
       x = "Year", y = "Death Rates (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Subset data for a specific gender and region
subdata = subset(data, Gender == genders[2] & Census.Region == regions[3])

# Create matrix for storing death rates by age and year
deathrates = matrix(NA, length(ages), length(years))
colnames(deathrates) = years
row.names(deathrates) = ages

# Calculate death rates by age and year
for (i in 1:length(years)) {
  subdatai = subset(subdata, Year == years[i])
  for (j in 1:length(ages)) {
    subdataij = subset(subdatai, Ten.Year.Age.Groups == ages[j])
    deathrates[j, i] = subdataij$Deaths / as.numeric(subdataij$Population) * 100
  }
}

# Convert to long format for ggplot
deathrates_long = melt(deathrates, varnames = c("Age", "Year"), value.name = "DeathRate")

# Line Plot: Death Rates by Age and Year
ggplot(deathrates_long, aes(x = Age, y = DeathRate, color = Year, group = Year)) +
  geom_line(size = 1.2) +
  labs(title = paste("Death Rates by Age and Year\nGender:", genders[2], ", Region:", regions[3]),
       x = "Age Groups", y = "Death Rates (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Subset data for a specific year and gender
subdata = subset(data, Year == years[10] & Gender == genders[1])

# Create matrix for storing death rates by age group and region
deathrates = matrix(NA, length(ages), length(regions))
colnames(deathrates) = regions
row.names(deathrates) = ages

# Calculate death rates by age group and region
for (i in 1:length(regions)) {
  subdatai = subset(subdata, Census.Region == regions[i])
  for (j in 1:length(ages)) {
    subdataij = subset(subdatai, Ten.Year.Age.Groups == ages[j])
    deathrates[j, i] = subdataij$Deaths / as.numeric(subdataij$Population) * 100
  }
}

# Convert to long format for ggplot
deathrates_long = melt(deathrates, varnames = c("Age", "Region"), value.name = "DeathRate")

# Line Plot: Death Rates by Age Group and Region
ggplot(deathrates_long, aes(x = Age, y = DeathRate, color = Region, group = Region)) +
  geom_line(size = 1.2) +
  labs(title = paste("Death Rates by Age Group and Region\nYear:", years[10], ", Gender:", genders[1]),
       x = "Age Groups", y = "Death Rates (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Subset data for a specific year and gender
subdata = subset(data, Year == years[10] & Gender == genders[2])

# Create matrix for storing death rates by age group and region
deathrates = matrix(NA, length(ages), length(regions))
colnames(deathrates) = regions
row.names(deathrates) = ages

# Calculate death rates by age group and region
for (i in 1:length(regions)) {
  subdatai = subset(subdata, Census.Region == regions[i])
  for (j in 1:length(ages)) {
    subdataij = subset(subdatai, Ten.Year.Age.Groups == ages[j])
    deathrates[j, i] = subdataij$Deaths / as.numeric(subdataij$Population) * 100
  }
}

# Convert to long format for ggplot
deathrates_long = melt(deathrates, varnames = c("Age", "Region"), value.name = "DeathRate")

# Line Plot: Death Rates by Age Group and Region
ggplot(deathrates_long, aes(x = Age, y = DeathRate, color = Region, group = Region)) +
  geom_line(size = 1.2) +
  labs(title = paste("Death Rates by Age Group and Region\nYear:", years[10], ", Gender:", genders[2]),
       x = "Age Groups", y = "Death Rates (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Subset data for a specific region and gender
subdata = subset(data, Census.Region == regions[3] & Gender == genders[2])

# Create matrix for storing death rates by age group and year
deathrates = matrix(NA, length(ages), length(years))
colnames(deathrates) = years
row.names(deathrates) = ages

# Calculate death rates by age group and year
for (i in 1:length(years)) {
  subdatai = subset(subdata, Year == years[i])
  for (j in 1:length(ages)) {
    subdataij = subset(subdatai, Ten.Year.Age.Groups == ages[j])
    deathrates[j, i] = subdataij$Deaths / as.numeric(subdataij$Population) * 100
  }
}

# Convert to long format for ggplot
deathrates_long = melt(deathrates, varnames = c("Age", "Year"), value.name = "DeathRate")

# Line Plot: Death Rates by Age Group and Year
ggplot(deathrates_long, aes(x = Age, y = DeathRate, color = Year, group = Year)) +
  geom_line(size = 1.2) +
  labs(title = paste("Death Rates by Age Group and Year\nRegion:", regions[3], ", Gender:", genders[2]),
       x = "Age Groups", y = "Death Rates (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









# Subset data for a specific year and age group
subdata = subset(data, Year == years[15] & Ten.Year.Age.Groups == ages[8])

# Create matrix for storing death rates by region and gender
deathrates = matrix(NA, length(regions), length(genders))
colnames(deathrates) = genders
row.names(deathrates) = regions

# Calculate death rates by region and gender
for (i in 1:length(genders)) {
  subdatai = subset(subdata, Gender == genders[i])
  for (j in 1:length(regions)) {
    subdataij = subset(subdatai, Census.Region == regions[j])
    deathrates[j, i] = subdataij$Deaths / as.numeric(subdataij$Population) * 100
  }
}

# Convert to long format for ggplot
deathrates_long = melt(deathrates, varnames = c("Region", "Gender"), value.name = "DeathRate")

# Line Plot: Death Rates by Region and Gender
ggplot(deathrates_long, aes(x = Region, y = DeathRate, color = Gender, group = Gender)) +
  geom_line(size = 1.2) +
  labs(title = paste("Death Rates by Region and Gender\nYear:", years[15], ", Age Group:", ages[8]),
       x = "Census Region", y = "Death Rates (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


