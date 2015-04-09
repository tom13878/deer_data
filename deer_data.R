# -------------------------------------
# deer collisions in glasgow
# -------------------------------------

# download the data
setwd("M:/deer_data")
# download.file("https://dataservices.open.glasgow.gov.uk/Download/Organisation/be55b88a-a63c-48e9-b991-fcd4a445c8b6/Dataset/be2a7ae7-f359-407d-8a62-cdb1786cb4d7/File/0c078c9e-99ab-45b6-8806-18bed805456f/Version/241d0e39-f77a-4eab-a78a-50840fb4b1b2", destfile='M:/deer_data/deer_dict.csv')
deer_data <- read.csv('deer_data.csv')
deer_dict <- read.csv('deer_dict.csv')

# use eastings and northings to make a spatial points data frame
library(rgdal)
library(zoo)
library(dplyr)

deer_data <- select(deer_data, EASTING, NORTHING, species=DEERSPP, injury=PEOPLEINJY, date=REALINCDAT, roadno=INCROADNO, year=INCYEAR)
deer_data$id <- 1:nrow(deer_data)
deer_data$date <- as.Date(deer_data$date, format="%d/%m/%Y")

# get a month variable
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

x <-as.character(deer_data$date)
split <- strsplit(x, "-") # creates a list
str(split)
digit <- character()
for (i in 1:length(split)){
        digit <- c(digit, split[[i]][2])
}

digit <- gsub("0", "", digit)

month <- character()
for(i in as.numeric(digit)){
        month <- c(month, months[i])
}

deer_data$month <- factor(month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Nov", "Dec"))

# -------------------------------------
# time series basic analysis
# -------------------------------------

# now aggregate dates according to the month using the floor_date function from
# lubridate package
# make a sequence of dates coveing the time period of the data
dates <- data.frame(date=seq.Date(from=min(deer_data$date), to=max(deer_data$date), by=1))
by_day <- group_by(deer_data, date) %>% summarise(freq=as.numeric(length(id)))
nrow(unique(select(deer_data, EASTING, NORTHING))) # watch out for double kills
# merge with the actual data
data <- left_join(dates, by_day)
data$freq[is.na(data$freq)] <- 0

data$month <- floor_date(data$date, "month")
data$year <- floor_date(data$date, "year")

# summarise by individual month and year
by_month <- group_by( data, month ) %>% summarise( month_freq=sum(freq) )
by_year <- group_by( data, year ) %>% summarise( year_freq=sum(freq) )

# make a plot with ggplot
ggplot( data=by_day, aes( x=date, y=freq ) ) + geom_line( )
ggplot( data=by_month, aes( x=month, y=month_freq ) ) + geom_line( ) +
        scale_x_date(breaks=date_breaks("2 month"), labels=date_format("%b")) +
        theme(axis.text.x=element_text(angle=90, hjust=1)) +
        ylab("No. of deer collisions per month") + xlab(" ")
        

ggplot( data=by_year, aes( x=year, y=year_freq ) ) + geom_line( colour = 'red') +
        scale_x_date(breaks=date_breaks("year"), labels=date_format("%Y") ) +
        ylab("No. of deer collisions per year") + xlab(" ") + theme_bw(20)

# -------------------------------------
# spatial analysis of deer collisions
# -------------------------------------

# prepare data for analysis
ukgrid="+init=epsg:27700"
latlong="+init=epsg:4326"

coords <- cbind(Easting=deer$EASTING, Northing=deer$NORTHING)
row.names(coords) <- 1:nrow(coords)

deer_sp <- SpatialPointsDataFrame(coords, data=select(deer, -EASTING, -NORTHING),
                                  proj4string=CRS("+init=epsg:27700"))

head(deer_sp@data)
head(deer_sp@coords)

deer_sp_ll <- spTransform(deer_sp, CRS(latlong))
colnames(deer_sp_ll@coords)[colnames(deer_sp_ll@coords)=="Easting"] <- "longitude"
colnames(deer_sp_ll@coords)[colnames(deer_sp_ll@coords)=="Northing"] <- "latitude"

deer_data <- data.frame(cbind(deer_sp_ll@coords, deer_sp_ll@data))

# use ggmap to get a map of glasgow
glasgow_map <- get_map("Glasgow", zoom=11, source='osm')
glasgow_map2 <- get_map("Glasgow", zoom=11, source="stamen", maptype="toner")
glasgow_map3 <- get_map("Glasgow", zoom=11, source="stamen", maptype="watercolor")

ggmap(glasgow_map) + geom_point(data=deer_data, aes(x=longitude, y=latitude))

# create a month variable - link the data together using fortify
# function which splits up dates and looks at month variable
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

x <-as.character(deer_data$date)
split <- strsplit(x, "/") # creates a list
str(split)
digit <- character()
for (i in 1:length(split)){
        digit <- c(digit, split[[i]][2])
}

digit <- gsub("0", "", digit)

month <- character()
for(i in as.numeric(digit)){
        month <- c(month, months[i])
}

deer_data$month <- factor(month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Nov", "Dec"))

# now make a plot with colour based on month
ggmap(glasgow_map2) + geom_point(data=deer_data, aes(x=longitude, y=latitude, colour=month))
ggmap(glasgow_map2) + geom_point(data=deer_data, aes(x=longitude, y=latitude, colour=month),
                                 size=6, alpha=0.4) + facet_wrap(~month)
