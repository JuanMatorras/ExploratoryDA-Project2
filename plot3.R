## Plot for Question n.3
## Of the four types of sources indicated by the type (point, nonpoint, onroad, 
## nonroad) variable, which of these four sources have seen decreases in emissions 
## from 1999 to 2008 for Baltimore City?
## Which have seen increases in emissions from 1999 to 2008?
## Use ggplot2 plotting system

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
baltimoreYearType <- NEI %>% filter(fips == "24510") %>% group_by(type, year) %>% 
        summarize(yearTotal = sum(Emissions))

# Cheking resulting data frame
str(baltimoreYearType)
# View(baltimoreYearType)

# Overriding alphabetical ordering of type variable to suit as desired for plotting
baltimoreYearType$type <- factor(baltimoreYearType$type, 
                                 levels = c("POINT", "NONPOINT", 
                                            "ON-ROAD", "NON-ROAD"))

# Loading ggplot2 for use as required
library(ggplot2)

# Plotting Total PM2.5 Yearly Emissions by Source Type un Baltimore to a png file
# Opening the device
png("plot3.png", width = 600, height = 400)
# Actual plotting
ggplot(baltimoreYearType, aes(x = factor(year), y = yearTotal, fill = type)) + 
        theme_bw() + 
        geom_bar(stat = "identity") + 
        facet_grid(. ~ type) + 
        labs(title = "Total Tons of Fine Particulate Matter (PM2.5) Emissions 
             in Baltimore City by Source Type during the 1999-2008 Period", 
             x = "Year", y = "Number of tons of PM2.5 emissions") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.margin = unit(c(20, 20, 20, 20), "pt")) + 
        scale_y_continuous(labels=function(n){format(n, scientific=FALSE,
                                                     big.mark=",")}) +
        guides(fill = FALSE)
# Closing off the device
dev.off()

## Answer: In Baltimore all source types have seen decreases except for the "Point" type
