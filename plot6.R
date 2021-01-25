## Plot for Question n.6
## Compare emissions from motor vehicle sources in Baltimore City with emissions 
## from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
## Which city has seen greater changes over time in motor vehicle emissions?

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
# Getting SCC lines with "vehicles" in EI.Sector description
sccVehicles <- SCC[grep("*Vehicles", SCC$EI.Sector),  ]
# Taking a look at the resulting data
dim(sccVehicles)
head(sccVehicles)
str(sccVehicles)
# Looking at unique values for SCC.Level.Two variable
unique(sccVehicles$SCC.Level.Two) # Border Crossings should not appear after filtering Baltimore City and LA County
# Getting the list of SCC codes relating to vehicles for NEI filtering
sccVehiclesList <- unique(sccVehicles$SCC)
# Filtering NEI data according to SCC vehicle codes and Baltimore City and LA County FIPS codes 
vehiBaltLA <- subset(NEI, SCC %in% sccVehiclesList) %>% 
        filter(fips == "24510" | fips == "06037")
# Attaching SCC.Level.Two values
vehiBaltLA <- merge(x = vehiBaltLA, y = sccVehicles[, c("SCC", "SCC.Level.Two")], 
                  by = "SCC")
# Looking at unique values for SCC.Level.Two variable
unique(vehiBaltLA$SCC.Level.Two) # Border Crossings do not appear as expected
# Getting totals per year by type SCC.Level.Two for each city
vehiBaltLA <- vehiBaltLA %>% group_by(fips, year, SCC.Level.Two) %>% 
        summarize(yearTotal = sum(Emissions))
# Checking resulting data frame
# View(vehiBaltLA)
# Getting absolute totals per year
vehiBaltLATotal <- vehiBaltLA %>% group_by(fips, year) %>% 
        summarize(yearTotal = sum(yearTotal)) %>% mutate(SCC.Level.Two = "TOTAL")
# Adding rows with absolute total values to data frame
vehiBaltLA <- bind_rows(vehiBaltLA, vehiBaltLATotal)
# Adding city column
vehiBaltLA <- mutate(vehiBaltLA, city = ifelse(fips =="06037", "LA County", 
                                               "Baltimore"))
# Checking resulting data frame
# View(vehiBaltLA)
# Overriding alphabetical ordering of SCC.Level.Two variable to suit as desired
vehiBaltLA$SCC.Level.Two <- factor(vehiBaltLA$SCC.Level.Two, 
                                   levels = c("Highway Vehicles - Gasoline", 
                                              "Highway Vehicles - Diesel", 
                                              "TOTAL"))

# Loading ggplot2 for use in plotting
library(ggplot2)

# Plotting PM2.5 Yearly Emissions Variation from Vehicles in Baltimore and LA to a png file
# Opening the device
png("plot6.png", width = 600, height = 400)
# Actual plotting
ggplot(vehiBaltLA, aes(x = factor(year), y = yearTotal, fill = SCC.Level.Two)) + 
        theme_bw() + 
        geom_bar(stat = "identity") + 
        facet_grid(city ~ SCC.Level.Two, scales = "free") + 
        labs(title = "Total Tons of Fine Particulate Matter (PM2.5) Emissions 
        from Vehicles in Baltimore City an LA County during the 1999-2008 Period", 
             x = "Year", y = "Number of tons of PM2.5 emissions") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.margin = unit(c(20, 20, 20, 20), "pt")) + 
        scale_y_continuous(labels=function(n){format(n, scientific=FALSE,
                                                     big.mark=",")}) +
        guides(fill = FALSE)

# Closing off the device
dev.off()

## Answer: Emissions from both gasoline and diesel vehicles have decreased 
## in Baltimore City during the period of study while in LA County diesel
## emissions have increased and so have total emissions from vehicles despite
## the decrease for gasoline vehicles
