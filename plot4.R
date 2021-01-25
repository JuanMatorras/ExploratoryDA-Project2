## Plot for Question n.4
## Across the USA, how have emissions from coal combustion-related sources
## changed from 1999 to 2008?

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
# Getting SCC lines whose Sectors relate to Coal Fuel Combustion
sccCoalComb <- SCC[grep("Fuel Comb.+Coal", SCC$EI.Sector),  ]
# Taking a look at the resulting data
dim(sccCoalComb)
str(sccCoalComb)
head(sccCoalComb)
tail(sccCoalComb)
# Curious about using only "Coal" as the search string
dim(SCC[grep("Coal", SCC$EI.Sector),  ]) # Dimension is exactly the same. All "coal" Sectors are related to "fuel combustion"
# Curios about types of points
unique(sccCoalComb$Data.Category) # Only Point and Nonpoint as expected
# Getting list of SCC codes relating to coal combustion sectors for NEI filtering
sccCoalCombList <- unique(sccCoalComb$SCC)
# Filtering NEI data according to chosen sectors
coalCombUSA <- subset(NEI, SCC %in% sccCoalCombList)
# Getting totals per year by type
coalCombUSA <- coalCombUSA %>% group_by(type, year) %>% 
        summarize(yearTotal = sum(Emissions))
# Getting absolute totals per year
coalCombUSAtotal <- coalCombUSA %>% group_by(year) %>% 
        summarize(yearTotal = sum(yearTotal)) %>% mutate(type = "TOTAL")
# Adding rows with absolute total values to data frame
coalCombUSA <- bind_rows(coalCombUSA, coalCombUSAtotal)
# Checking resulting data frame
str(coalCombUSA)
# View(coalCombUSA)
# Overriding alphabetical ordering of type variable to suit as desired for plotting
coalCombUSA$type <- factor(coalCombUSA$type, levels = c("POINT", "NONPOINT", 
                                                        "TOTAL"))

# Loading ggplot2 for use in plotting
library(ggplot2)

# Plotting Total PM2.5 Yearly Emissions from Coal Combustion to a png file
# Opening the device
png("plot4.png", width = 600, height = 400)
# Actual plotting
ggplot(coalCombUSA, aes(x = factor(year), y = yearTotal, fill = type)) + 
        theme_bw() + 
        geom_bar(stat = "identity") + 
        facet_grid(. ~ type) + 
        labs(title = "Total Tons of Fine Particulate Matter (PM2.5) Emissions 
             from Coal Combustion in USA during the 1999-2008 Period", 
             x = "Year", y = "Number of tons of PM2.5 emissions") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.margin = unit(c(20, 20, 20, 20), "pt")) + 
        scale_y_continuous(labels=function(n){format(n, scientific=FALSE,
                                                     big.mark=",")}) +
        guides(fill = FALSE)
# Closing off the device
dev.off()

## Answer: Emissions from coal combustion-related sources have decreased in USA
