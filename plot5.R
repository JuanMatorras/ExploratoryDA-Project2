## Plot for Question n.5
## How have emissions from motor vehicle sources changed from 1999–2008
## in Baltimore City?

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
unique(sccVehicles$SCC.Level.Two) # Border Crossings should not appear after filtering Baltimore City
# Getting the list of SCC codes relating to vehicles for NEI filtering
sccVehiclesList <- unique(sccVehicles$SCC)
# Filtering NEI data according to SCC vehicle codes and Baltimore City FIPS code 
vehiclesBalt <- subset(NEI, SCC %in% sccVehiclesList) %>% filter(fips == "24510")
# Attaching SCC.Level.Two values
vehiclesBalt <- merge(x = vehiclesBalt, y = sccVehicles[, c("SCC", "SCC.Level.Two")], 
                  by = "SCC")
# Looking at unique values for SCC.Level.Two variable
unique(vehiclesBalt$SCC.Level.Two) # Border Crossings do not appear as expected
# Getting totals per year by type SCC.Level.Two
vehiclesBalt <- vehiclesBalt %>% group_by(year, SCC.Level.Two) %>% 
        summarize(yearTotal = sum(Emissions))
# Checking resulting data frame
# View(vehiclesBalt)
# Getting absolute totals per year
vehiclesBaltTotal <- vehiclesBalt %>% group_by(year) %>% 
        summarize(yearTotal = sum(yearTotal)) %>% mutate(SCC.Level.Two = "TOTAL")
# Adding rows with absolute total values to data frame
vehiclesBalt <- bind_rows(vehiclesBalt, vehiclesBaltTotal)
# Checking resulting data frame
# View(vehiclesBalt)
# Overriding alphabetical ordering of SCC.Level.Two variable to suit as desired for plotting
vehiclesBalt$SCC.Level.Two <- factor(vehiclesBalt$SCC.Level.Two, 
                                     levels = c("Highway Vehicles - Gasoline", 
                                                "Highway Vehicles - Diesel", 
                                                "TOTAL"))

# Loading ggplot2 for use in plotting
library(ggplot2)

# Plotting Total PM2.5 Yearly Emissions from Vehicles in Baltimore to a png file
# Opening the device
png("plot5.png", width = 600, height = 400)
# Actual plotting
ggplot(vehiclesBalt, aes(x = factor(year), y = yearTotal, fill = SCC.Level.Two)) + 
        theme_bw() + 
        geom_bar(stat = "identity") + 
        facet_grid(. ~ SCC.Level.Two) + 
        labs(title = "Total Tons of Fine Particulate Matter (PM2.5) Emissions 
             from Vehicles in Baltimore City during the 1999-2008 Period", 
             x = "Year", y = "Number of tons of PM2.5 emissions") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.margin = unit(c(20, 20, 20, 20), "pt")) + 
        scale_y_continuous(labels=function(n){format(n, scientific=FALSE,
                                                     big.mark=",")}) +
        guides(fill = FALSE)
# Closing off the device
dev.off()

## Answer: Emissions from both gasoline and diesel vehicles have decreased 
## in Baltimore City during the period of study, and so has the total
