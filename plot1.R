## Plot for Question n.1
## Have total PM2.5 emissions decreased in USA from 1999 to 2008?
## Use base plotting system

# Reading the data from the source files
# Files should be in the working directory
# Due to file size it might take a few seconds. Patience ;-)
if(!exists("NEI")){NEI <- readRDS("summarySCC_PM25.rds")}
if(!exists("SCC")){SCC <- readRDS("Source_Classification_Code.rds")}

# Getting a feel of the data
head(NEI)
str(NEI)
head(SCC)
str(SCC)

# Checking that there are no data from years different from those mentioned
# in the requirements for the submission
unique(NEI$year)

# Loading dplyr for easier data handling
library(dplyr)

# Creating data frame (tibble) with plotting data
yearTotals <- NEI %>% group_by(year) %>% summarize(totalPM25 = sum(Emissions))

# Cheking resulting data frame
str(yearTotals)

# Plotting Total PM2.5 Yearly Emissions in USA to a png file
# Opening the device
png("plot1.png", width = 600, height = 400)
# Actual plotting
par(mar = c(5, 5, 4, 3))
plot(yearTotals$year, yearTotals$totalPM25/1e6, type = "l", lwd = 2,
     xlab = "Year", 
     ylab = "Millions of tons of PM2.5 emissions", 
     main = "Total Tons (in millions) of Fine Particulate Matter (PM2.5) Emissions
     in the USA in the 1999-2008 Period")
# Closing off the device
dev.off()

## Answer: Total emissions have decreased in USA between 1999 and 2008
