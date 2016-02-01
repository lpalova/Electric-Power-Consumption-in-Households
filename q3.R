library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(scales)
options(digits = 10)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}


household_df <- read.table("../data/household_power_consumption.txt", 
                        header = TRUE,
                        sep = ";",
                        na.strings = c("\\N","","NA","?"),
                        colClasses = c("character",
                                       "character",
                                       "numeric",
                                       "numeric",
                                       "numeric",
                                       "numeric",
                                       "numeric",
                                       "numeric",
                                       "numeric"
                                       ),
                        col.names = c("date",
                                      "time",
                                      "active_power",
                                      "reactive_power",
                                      "voltage",
                                      "intensity",
                                      "sub1",
                                      "sub2",
                                      "sub3"
                                      )
)
## Datetime of the measurement
household_df$datetime <- as.POSIXct( strptime( paste( household_df$date, household_df$time ), "%d/%m/%Y %H:%M:%S" ) )

## There are 25979 observations with NAs; 25979/dim(household_df)[1]*100.0 = 1.25% of NAs
summary(household_df)
missing <- household_df[ !complete.cases(household_df), ]$datetime
weekdays <- weekdays(missing)
sum(weekdays == "Monday")
sum(weekdays == "Tuesday")
sum(weekdays == "Wednesday")
sum(weekdays == "Thursday")
sum(weekdays == "Friday")
sum(weekdays == "Saturday")
sum(weekdays == "Sunday")
# no pattern
evening <- missing[ hour(missing) > 19 | hour(missing) < 8 ]
# no pattern
# Missing data appear to be spread across weekdays and daytime/nightime hours.

## Cleaning Data
# Extracting only nonNAs observations
recorded_df <- household_df[ complete.cases(household_df), ]
# Calculating submetering not measured in sub1, sub2 or sub3 in Watt-hour
recorded_df$subrest <- (recorded_df$active_power*1000.0/60.0) - recorded_df$sub1 - recorded_df$sub2 - recorded_df$sub3
# Quarter of the measurement: Jan-March == 1, Apr-June == 2, July-Sept == 3, Oct-Dec == 4
recorded_df$quarter <- as.factor(quarter(recorded_df$datetime))
# Setting an arbitrary date for Monday in an average week
recorded_df$time2000 <- update( recorded_df$datetime, year = 2016, month = 02, day = 01, tz = "UTC")
# Calculating apparent power from the active and reactive powers
recorded_df$apparent_power <- sqrt(recorded_df$active_power^2+recorded_df$reactive_power^2)
# Calculating shift factor from the active and apparent powers
recorded_df$shift_factor <- recorded_df$active_power/recorded_df$apparent_power
# 
hist( recorded_df$reactive_power )
summary(recorded_df$reactive_power)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000000 0.0480000 0.1000000 0.1237145 0.1940000 1.3900000 
hist( recorded_df$active_power )
summary(recorded_df$active_power)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.076000  0.308000  0.602000  1.091615  1.528000 11.122000 
hist( recorded_df$shift_factor )
summary(recorded_df$shift_factor)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.5558553 0.9519593 0.9934251 0.9636864 0.9997272 1.0000000 

## Dividing data into Mondays - Sundays by a day attribute
## Binding all days into week/s
recorded <- tbl_df(recorded_df)
Mondays <- recorded %>% filter( weekdays(datetime) == "Monday" ) %>%
        mutate( time2000 = update( time2000, day = 01 ) )
Tuesdays <- recorded %>% filter( weekdays(datetime) == "Tuesday" ) %>%
        mutate( time2000 = update( time2000, day = 02 ) )
Wednesdays <- recorded %>% filter( weekdays(datetime) == "Wednesday" ) %>%
        mutate( time2000 = update( time2000, day = 03 ) )
Thursdays <- recorded %>% filter( weekdays(datetime) == "Thursday" ) %>%
        mutate( time2000 = update( time2000, day = 04 ) )
Fridays <- recorded %>% filter( weekdays(datetime) == "Friday" ) %>%
        mutate( time2000 = update( time2000, day = 05 ) )
Saturdays <- recorded %>% filter( weekdays(datetime) == "Saturday" ) %>%
        mutate( time2000 = update( time2000, day = 06 ) )
Sundays <- recorded %>% filter( weekdays(datetime) == "Sunday" ) %>%
        mutate( time2000 = update( time2000, day = 07 ) )
week <- rbind( Mondays, Tuesdays, Wednesdays, Thursdays, Fridays, Saturdays, Sundays )
# tick-marks for date axis: from 00:00 Monday to 24:00 Sunday
datebreaks <- seq(as.POSIXct(min(week$time2000)), as.POSIXct("2016-02-08 00:00:00"), by="12 hours")

## Active energy for an average week
aveweek <- week %>% 
        group_by( time2000 ) %>%
        summarize( 
                active_energy = mean(active_power*1000.0/60.0), 
                sub1 = mean(sub1),
                sub2 = mean(sub2),
                sub3 = mean(sub3),
                subrest = mean(subrest)
        )
names(aveweek) <- c("time2000", "Active Energy", "Kitchen", "Laundry Room", "Water-Heat, AC", "Rest")
aveweekmelt <- melt(aveweek, id="time2000")
p1 <- ggplot(data=aveweekmelt, 
             aes(x=time2000, y=value, colour=variable)) +
        geom_line() +
        scale_colour_manual( name="", values = c("black", "green", "red", "magenta", "blue") ) +
        scale_x_datetime( breaks = datebreaks,
                          labels=date_format("%H:%M"), 
                          limits=c( min(aveweek$time2000), max(aveweek$time2000) ) ) +
        labs( x = "Time [h]: 00:00 Monday through 24:00 Sunday", y = "Active Energy [Wh]", 
              title = "Submetering of Global Active Energy for an Average Week" ) +
        theme(
                plot.title = element_text(size = rel(1.8), face = "bold"),
                axis.text.x = element_text(size = rel(1.4)),
                axis.title.x = element_text(size = rel(1.6)), 
                axis.text.y = element_text(size = rel(1.3), angle = 90),
                axis.title.y = element_text(size = rel(1.6), angle = 90),
                legend.text = element_text(size = rel(1.0), angle = 45)
        )

## Reactive energy for an average week, compared to kitchen submetering
aveweekr <- week %>% 
        group_by( time2000 ) %>%
        summarize( 
                sub1 = mean(sub1),
                reactive_energy = mean(reactive_power*1000.0/60.0)
        )
names(aveweekr) <- c("time2000","Kitchen", "Reactive Energy")
aveweekrmelt <- melt(aveweekr, id="time2000")
p7 <- ggplot(data=aveweekrmelt, 
             aes(x=time2000, y=value, colour = variable)) +
        geom_line() +
        scale_colour_manual( name="", values = c("lightblue", "black") ) +
        scale_x_datetime( breaks = datebreaks,
                          labels=date_format("%H:%M"), 
                          limits=c( min(aveweekr$time2000), max(aveweekr$time2000) ) ) +
        labs( x = "Time [h]: 00:00 Monday through 24:00 Sunday", y = "Global Reactive Energy [Wh]", 
              title = "Global Reactive Energy for an Average Week" ) +
        theme(
                plot.title = element_text(size = rel(2), face = "bold"),
                axis.text.x = element_text(size = rel(1.4)),
                axis.title.x = element_text(size = rel(1.6)), 
                axis.text.y = element_text(size = rel(1.3), angle = 90),
                axis.title.y = element_text(size = rel(1.6), angle = 90),
                legend.text = element_text(size = rel(1.1), angle = 45)
        )
multiplot(p1, p7, cols=1)


## Active and reactive energy by quarters
endweek <- rbind( Thursdays, Fridays, Saturdays, Sundays )
# tick-marks for date axis: from 00:00 Thursday to 24:00 Sunday (UTC vs EST shift)
datebreaks2 <- seq(as.POSIXct("2016-02-03 19:00:00 UTC"), as.POSIXct("2016-02-08 00:00:00 UTC"), by="12 hours")
aveweekq <- endweek %>% 
        group_by( time2000, quarter ) %>%
        summarize( 
                active_energy = mean(active_power*1000.0/60.0), 
                reactive_energy = mean(reactive_power*1000.0/60.0),
                sub1 = mean(sub1),
                sub2 = mean(sub2),
                sub3 = mean(sub3),
                subrest = mean(subrest)
        )
names(aveweekq) <- c("time2000", "quarter", "Active Energy", "Reactive Energy", "Kitchen", "Laundry Room", "Water-Heat, AC", "Rest")
aveweekqmelt <- melt(aveweekq, id=c("time2000","quarter")) 
p3 <- ggplot(data=aveweekqmelt, 
             aes(x=time2000, y=value, colour=variable)) +
        geom_line() +
        facet_grid(quarter~.) +
        scale_colour_manual( name="", values = c("black", "grey", "green", "red", "magenta", "blue") ) +
        scale_x_datetime( breaks = datebreaks2, 
                          labels=date_format("%H:%M"),
                          limits=c( min(aveweekq$time2000), max(aveweekq$time2000) ) ) +
        labs( x = "Time [h]: 00:00 Thursday through 24:00 Sunday", y = "Global Active/Reactive Energy [Wh]", 
              title = "Global Energy by Quarters" ) +
        theme(
                plot.title = element_text(size = rel(2), face = "bold"),
                axis.text.x = element_text(size = rel(1.4)),
                axis.title.x = element_text(size = rel(1.6)), 
                axis.text.y = element_text(size = rel(1.3), angle = 90),
                axis.title.y = element_text(size = rel(1.6), angle = 90),
                legend.text = element_text(size = rel(1.1), angle = 45)
        )

p3
