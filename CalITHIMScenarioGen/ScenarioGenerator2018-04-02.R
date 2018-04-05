# ScenarioGenerator2018-03-28.R - N Maizlish - 2018/4/2 update

# Generates and formats scenarios for multiples of baseline active
# travel and bike, walk are arguments to a function that calculates 
# mean and median travel times from baseline travel and the CV of mean
# travel time for 5 California regions and the state overall. 
# Formats pre-established scenarios for optimum AT
# times based on US Surgeon Generals' PA recommendation for adults
# and short car trip substitution. Also, Calculates travel distances for
# walk, cycle, bus, rail, car-drive, car-passenger, motorcycle 
# and truck. 

# set working directory
setwd ("C:/RStatistics/ScenarioGenerator")

options(scipen=999)

# Read Active travel times mean min/person/wk
at  <- read.csv(file= "ATmean_min_week_baseline.csv", head=TRUE, sep=",")
sat <- read.csv(file= "bike_walk_mean_min_week_scs.csv", head=TRUE, sep=",")
cv  <- read.csv(file= "bike_walk_cv.csv", head=TRUE, sep=",")
ussg <- read.csv(file= "USSG_ATtimes.csv", head=TRUE, sep=",")

# x is walk multiplier and y = bicycle multiplier

time.goals <- function(bfile,sfile,x,y) { 
  
  # Reshape baseline file from long to wide
  at_wide <- reshape(bfile,
                     timevar = "Mode",
                     idvar = c("Region"),
                     direction = "wide")
  
  # Reshape scenario file from long to wide
  sat_wide <- reshape(sfile,
                      timevar = "Mode",
                      idvar = c("Region"),
                      direction = "wide")
  
  # Sort so records match in the files
  sat_wide <- sat_wide[order(sat_wide$Region), ] 
  
  
  # Create total
  at_wide$Baseline.Total <- at_wide$Baseline.Walk + at_wide$Baseline.Bike
  
  # Merge with CV
  at_wide <- merge(at_wide,cv, by = "Region", all.x=TRUE)
  
  ############# Function to Calculate Medians Begins ###################
  # Inputs are file used 
  
  # Create placeholder temp scenario
  at_wide_temp <- at_wide
  at_wide_temp["Scenario.Walk"]  <- sat_wide$Baseline.Walk*x
  at_wide_temp["Scenario.Bike"]  <- sat_wide$Baseline.Bike*y
  at_wide_temp$Scenario.Total    <- at_wide_temp$Scenario.Walk + at_wide_temp$Scenario.Bike
  
  # create baseline medians/wk
  # Step 1 - Create sd
  at_wide_temp$sd_at_b  <- at_wide_temp$Baseline.Total * at_wide_temp$CV
  
  # Step 2 - take natural logarithms of mean and SD
  at_wide_temp$ln_mean_at_b <- log(at_wide_temp$Baseline.Total) - 0.5*log(1 + (at_wide_temp$CV)^2)
  at_wide_temp$ln_sd_at_b <- sqrt(log(1+ at_wide_temp$CV^2))
  
  # Step 3 - take normal inverse
  at_wide_temp$ni_ln_mean_at_b <-qnorm(0.5, at_wide_temp$ln_mean_at_b, at_wide_temp$ln_sd_at_b)
  
  # Step 4 - back transform Normal inverse
  at_wide_temp$med_at_b <- exp(at_wide_temp$ni_ln_mean_at_b)
  
  # Step 5 - parse out walking and cycling
  at_wide_temp$med_at_walk_b <- at_wide_temp$med_at_b*(1- at_wide_temp$Baseline.Bike / at_wide_temp$Baseline.Total)
  at_wide_temp$med_at_bike_b <- at_wide_temp$med_at_b - at_wide_temp$med_at_walk_b 
  
  # Create temp scenario
  
  # create baseline medians/wk
  # Step 1 - Create sd
  at_wide_temp$cv_at_s   <- -0.0015429*( at_wide_temp$Scenario.Total - at_wide_temp$Baseline.Total) + at_wide_temp$CV 
  at_wide_temp$sd_at_s  <- at_wide_temp$Scenario.Total * at_wide_temp$cv_at_s
  
  # Step 2 - take natural logarithms of mean and SD
  at_wide_temp$ln_mean_at_s <- log(at_wide_temp$Scenario.Total) - 0.5*log(1 + (at_wide_temp$cv_at_s)^2)
  at_wide_temp$ln_sd_at_s <- sqrt(log(1+ at_wide_temp$cv_at_s^2))
  
  # Step 3 - take normal inverse
  at_wide_temp$ni_ln_mean_at_s <-qnorm(0.5, at_wide_temp$ln_mean_at_s, at_wide_temp$ln_sd_at_s)
  
  # Step 4 - back transform Normal inverse
  at_wide_temp$med_at_s <- exp(at_wide_temp$ni_ln_mean_at_s)
  
  # Step 5 - parse out walking and cycling
  at_wide_temp$med_at_walk_s <- at_wide_temp$med_at_s*(1- at_wide_temp$Scenario.Bike / at_wide_temp$Scenario.Total)
  at_wide_temp$med_at_bike_s <- at_wide_temp$med_at_s - at_wide_temp$med_at_walk_s 
  
  # Save columns of interest
  at_wide_temp <- at_wide_temp[,c(1:4,6:8,13:15,21:23)]
  
  return(at_wide_temp)
  
}

######################### Function Ends #############################

# File for CSMP2020
at3 <- time.goals(bfile=at, sfile=at, x=2,y=3)

# File for AT2030
at4 <- time.goals(bfile=at, sfile=at, x=4,y=9)

# File for SCS2040
scs <- time.goals(bfile=at, sfile=sat, x=1,y=1)

####################### Reshape Output - Long  ########################


###################### Begin Function ###############################

format.time <- function(fat) { 
  
  # Reshape Baseline
  at_long_baseline <- reshape(fat[,c(1:4,8:10)],
                              varying = c("Baseline.Walk", "Baseline.Bike", "Baseline.Total", "med_at_walk_b", "med_at_bike_b", "med_at_b"),
                              v.names = "Baseline",
                              timevar = "Mode",
                              times = c("Baseline.Walk", "Baseline.Bike", "Baseline.Total", "med_at_walk_b", "med_at_bike_b", "med_at_b"),
                              new.row.names = 1:36,
                              direction = "long")
  # Central tendency
  at_long_baseline$Centraltend <- ifelse(substr(at_long_baseline$Mode,1,3)=="med","median","mean")
  
  # rename variables
  
  at_long_baseline$Mode <- gsub("Baseline.Walk", "Walk", at_long_baseline$Mode)
  at_long_baseline$Mode <- gsub("Baseline.Bike", "Bike", at_long_baseline$Mode)
  at_long_baseline$Mode <- gsub("Baseline.Total", "Total", at_long_baseline$Mode)
  at_long_baseline$Mode <- gsub("med_at_walk_b", "Walk", at_long_baseline$Mode)
  at_long_baseline$Mode <- gsub("med_at_bike_b", "Bike", at_long_baseline$Mode)
  at_long_baseline$Mode <- gsub("med_at_b", "Total", at_long_baseline$Mode)
  
  at_long_baseline$ScenarioName <- "temp"
  at_long_baseline$Time <- "weekly"
  
  # Reshape Scenario
  at_long_scenario <- reshape(fat[,c(1,5:7,11:13)],
                              varying = c("Scenario.Walk","Scenario.Bike", "Scenario.Total", "med_at_walk_s", "med_at_bike_s", "med_at_s"),
                              v.names = "Scenario",
                              timevar = "Mode",
                              times = c("Scenario.Walk", "Scenario.Bike", "Scenario.Total", "med_at_walk_s", "med_at_bike_s", "med_at_s"),
                              new.row.names = 1:36,
                              direction = "long")
  
  at_long_scenario$Centraltend <- ifelse(substr(at_long_scenario$Mode,1,3)=="med","median","mean")
  
  # rename variables
  at_long_scenario$Mode <- gsub("Scenario.Walk", "Walk", at_long_scenario$Mode)
  at_long_scenario$Mode <- gsub("Scenario.Bike", "Bike", at_long_scenario$Mode)
  at_long_scenario$Mode <- gsub("Scenario.Total", "Total", at_long_scenario$Mode)
  at_long_scenario$Mode <- gsub("med_at_walk_s", "Walk", at_long_scenario$Mode)
  at_long_scenario$Mode <- gsub("med_at_bike_s", "Bike", at_long_scenario$Mode)
  at_long_scenario$Mode <- gsub("med_at_s", "Total", at_long_scenario$Mode)
  
  
  #combine columns
  at_long <- cbind(at_long_baseline[,c("Region","ScenarioName","Centraltend","Time","Mode","Baseline")],at_long_scenario["Scenario"])
  at_long_day <- cbind(at_long[,1:5],at_long[,c("Baseline","Scenario")]/7)
  at_long_day$Time <- "daily"
  
  # Combine times
  at_long <- rbind(at_long,at_long_day)
  
  return(at_long)
  
}
###################### End Function ###############################

# File for CSMP2020
CSMP2020 <- format.time(fat=at3)
CSMP2020$ScenarioName <- "CSMP2020"

# File for CARB2030
CARB2030 <- format.time(fat=at4)
CARB2030$ScenarioName <- "CARB2030"


# File for SCS
SCS2040 <- format.time(fat=scs)
SCS2040$ScenarioName <- "SCS2040"

# compile into a single file
ATtimes <- rbind(CSMP2020,CARB2030,SCS2040,ussg)
# ATtimes$PercentChange <- 100 * (ATtimes$Scenario/ATtimes$Baseline - 1) 

# Write out file
# write.csv(ATtimes, "ATtimes2.csv", row.names=FALSE)


######################### Distances  ############################


# Generates and formats mean distance by mode for scenarios 
# of multiples of baseline AT 

# Read Travel distances files 
bmiles <- read.csv(file= "Distances_mi_year_baseline.csv", head=TRUE, sep=",")
smiles <- read.csv(file= "Distances_mi_yr_scs.csv", head=TRUE, sep=",")

# ussg <- read.csv(file= "USSG_ATtimes.csv", head=TRUE, sep=",")

# x is walk multiplier and y = bicycle multiplier

distance.goals <- function(bfile,sfile,x,y) { 
  
  p <- 1 
  
  # Reshape baseline file from long to wide
  bmiles_wide <- reshape(bfile,
                         timevar = "Mode",
                         idvar = c("Region"),
                         direction = "wide")
  
  # Sort so records match in the files
  bmiles_wide <- bmiles_wide[order(bmiles_wide$Region), ] 
  
  # Flag for SCS when there are no multiples x = 1 and y = 1
  bmiles_wide$scs_flag <- x + y 
  
  # Reshape scenario file from long to wide
  smiles_wide <- reshape(sfile,
                         timevar = "Mode",
                         idvar = c("Region"),
                         direction = "wide")
  
  # Sort so records match in the files
  smiles_wide <- smiles_wide[order(smiles_wide$Region), ] 
  
  
  # Create total
  bmiles_wide$Baseline.Total <- bmiles_wide$Baseline.Walk + bmiles_wide$Baseline.Bike + bmiles_wide$Baseline.Rail + bmiles_wide$Baseline.Bus + bmiles_wide$Baseline.Truck + bmiles_wide$Baseline.CarDriver + bmiles_wide$Baseline.CarPassenger + bmiles_wide$Baseline.Motorcycle 
  
  
  ############# Function to Calculate Scenarios ###################
  # Inputs are file used 
  
  # Create placeholder temp scenario
  bmiles_wide_temp <- bmiles_wide
  bmiles_wide_temp["Scenario.Walk"]  <- smiles_wide$Baseline.Walk*x
  bmiles_wide_temp["Scenario.Bike"]  <- smiles_wide$Baseline.Bike*y
  bmiles_wide_temp["Scenario.Bus"]   <- smiles_wide$Baseline.Bus*x
  bmiles_wide_temp["Scenario.Rail"]  <- smiles_wide$Baseline.Rail*x
  bmiles_wide_temp["Scenario.Truck"]  <- smiles_wide$Baseline.Truck
  bmiles_wide_temp["Scenario.Motorcycle"]  <- smiles_wide$Baseline.Motorcycle
  
  # Scenario.CarDriver
  bmiles_wide_temp["Scenario.CarDriver"] <- (1 - p) * bmiles_wide_temp$Baseline.CarDriver + p * ((bmiles_wide_temp$Baseline.Walk + bmiles_wide_temp$Baseline.Bike + bmiles_wide_temp$Baseline.Rail + bmiles_wide_temp$Baseline.Bus + bmiles_wide_temp$Baseline.CarDriver + bmiles_wide_temp$Baseline.CarPassenger) -(bmiles_wide_temp$Scenario.Walk + bmiles_wide_temp$Scenario.Bike + bmiles_wide_temp$Scenario.Rail + bmiles_wide_temp$Scenario.Bus)) * (bmiles_wide_temp$Baseline.CarDriver/(bmiles_wide_temp$Baseline.CarDriver + bmiles_wide_temp$Baseline.CarPassenger))
  
  # Scenario.CarPassenger
  bmiles_wide_temp["Scenario.CarPassenger"] <- (1 - p) * bmiles_wide_temp$Baseline.CarPassenger + p * ((bmiles_wide_temp$Baseline.Walk + bmiles_wide_temp$Baseline.Bike + bmiles_wide_temp$Baseline.Rail + bmiles_wide_temp$Baseline.Bus + bmiles_wide_temp$Baseline.CarDriver + bmiles_wide_temp$Baseline.CarPassenger) -(bmiles_wide_temp$Scenario.Walk + bmiles_wide_temp$Scenario.Bike + bmiles_wide_temp$Scenario.Rail + bmiles_wide_temp$Scenario.Bus)) * (bmiles_wide_temp$Baseline.CarPassenger/(bmiles_wide_temp$Baseline.CarDriver + bmiles_wide_temp$Baseline.CarPassenger))
  
  # bmiles_wide_temp["Scenario.CarDriver"]     <- smiles_wide$Baseline.CarDriver
  #  bmiles_wide_temp["Scenario.CarPassenger"]  <- smiles_wide$Baseline.CarPassenger
  
  
  # if not multiples (x=1, y=1) then use scs for scenario
  bmiles_wide_temp$Scenario.CarDriver     <- ifelse (bmiles_wide_temp$scs_flag==2, smiles_wide$Baseline.CarDriver,    bmiles_wide_temp$Scenario.CarDriver)
  bmiles_wide_temp$Scenario.CarPassenger  <- ifelse (bmiles_wide_temp$scs_flag==2, smiles_wide$Baseline.CarPassenger, bmiles_wide_temp$Scenario.CarPassenger)
  
  # Total Distance
  bmiles_wide_temp$Scenario.Total  <- bmiles_wide_temp$Scenario.Walk + bmiles_wide_temp$Scenario.Bike + bmiles_wide_temp$Scenario.Rail + bmiles_wide_temp$Scenario.Bus + bmiles_wide_temp$Scenario.Truck + bmiles_wide_temp$Scenario.CarDriver + bmiles_wide_temp$Scenario.CarPassenger + bmiles_wide_temp$Scenario.Motorcycle
  
  return(bmiles_wide_temp)
  
}

######################### Function Ends #############################

# File for CSMP2020
bmiles3 <- distance.goals(bfile=bmiles, sfile=bmiles, x=2,y=3)

# File for AT2030
bmiles4 <- distance.goals(bfile=bmiles, sfile=bmiles, x=4,y=9)

# File for SCS2040
dscs <- distance.goals(bfile=bmiles, sfile=smiles, x=1, y=1)

# USSG Distances

# Subset file for mean minutes per week
ussg_mean_wk <- subset(ussg, Centraltend=="mean" & Time=="weekly")

# Reshape baseline file from long to wide
ussg_wide <- reshape(ussg_mean_wk,
                     timevar = "Mode",
                     idvar = c("Region"),
                     direction = "wide")
ussg_wide <- ussg_wide[,c("Region","Scenario.Walk","Scenario.Bike")]

# Convert times to distances
# Walk Min/week/60 min/hr x 3  mph x 52 weeks/year = min/week*2.6
# Bike Min/week/60 min/hr x 12 mph x 52 weeks/year = min/week*10.4

ussg_wide$Scenario.Walk <- ussg_wide$Scenario.Walk*2.6
ussg_wide$Scenario.Bike <- ussg_wide$Scenario.Bike*10.4

# Merge with baseline travel distances
ussg_wide <- merge(bmiles3[,c(1:11)],ussg_wide, by="Region")

# P is efficiency of replacement of car miles by walking and cycling (1 = 100% miles substituted)
p = 1

ussg_wide["Scenario.Bus"]   <- ussg_wide$Baseline.Bus
ussg_wide["Scenario.Rail"]  <- ussg_wide$Baseline.Rail
ussg_wide["Scenario.Truck"]  <- ussg_wide$Baseline.Truck
ussg_wide["Scenario.Motorcycle"]  <- ussg_wide$Baseline.Motorcycle

# Scenario.CarDriver
ussg_wide["Scenario.CarDriver"] <- (1 - p) * ussg_wide$Baseline.CarDriver + p * ((ussg_wide$Baseline.Walk + ussg_wide$Baseline.Bike + ussg_wide$Baseline.Rail + ussg_wide$Baseline.Bus + ussg_wide$Baseline.CarDriver + ussg_wide$Baseline.CarPassenger) -(ussg_wide$Scenario.Walk + ussg_wide$Scenario.Bike + ussg_wide$Scenario.Rail + ussg_wide$Scenario.Bus)) * (ussg_wide$Baseline.CarDriver/(ussg_wide$Baseline.CarDriver + ussg_wide$Baseline.CarPassenger))

# Scenario.CarPassenger
ussg_wide["Scenario.CarPassenger"] <- (1 - p) * ussg_wide$Baseline.CarPassenger + p * ((ussg_wide$Baseline.Walk + ussg_wide$Baseline.Bike + ussg_wide$Baseline.Rail + ussg_wide$Baseline.Bus + ussg_wide$Baseline.CarDriver + ussg_wide$Baseline.CarPassenger) -(ussg_wide$Scenario.Walk + ussg_wide$Scenario.Bike + ussg_wide$Scenario.Rail + ussg_wide$Scenario.Bus)) * (ussg_wide$Baseline.CarPassenger/(ussg_wide$Baseline.CarDriver + ussg_wide$Baseline.CarPassenger))

# Total Distance
ussg_wide["Scenario.Total"] = rowSums(ussg_wide[,c(12:19)])


###################### 50% Substitutrion of Short Car Trips by Walking and Cycling  #####################

# Read distance categories
dist_cat  <- read.csv(file= "Distances_pct_cat.csv", head=TRUE, sep=",")

# Reshape distance file from long to wide
dist_cat_wide <- reshape(dist_cat[,c(1:2,4)],
                         timevar = "distance_cat",
                         idvar = c("Region"),
                         direction = "wide")
# Merge with baseline
dist_cat_wide <- merge(bmiles3[,c(1:11)],dist_cat_wide, by="Region")

#Proportion of substituted miles
prop_sub <- 0.5

# Walk
dist_cat_wide$Scenario.Walk <- dist_cat_wide$Baseline.Walk + dist_cat_wide$proportion.lt_one*(dist_cat_wide$Baseline.CarDriver + dist_cat_wide$Baseline.CarPassenger)*prop_sub

# Bike
dist_cat_wide$Scenario.Bike <- dist_cat_wide$Baseline.Bike + dist_cat_wide$proportion.one2five*(dist_cat_wide$Baseline.CarDriver + dist_cat_wide$Baseline.CarPassenger)*prop_sub

# Scenario.CarDriver
dist_cat_wide["Scenario.CarDriver"] <-  ((dist_cat_wide$Baseline.CarDriver + dist_cat_wide$Baseline.CarPassenger) - (dist_cat_wide$Scenario.Walk + dist_cat_wide$Scenario.Bike) -(dist_cat_wide$Baseline.Walk + dist_cat_wide$Baseline.Bike )) * (dist_cat_wide$Baseline.CarDriver/(dist_cat_wide$Baseline.CarDriver + dist_cat_wide$Baseline.CarPassenger))

# Scenario.CarPassenger
dist_cat_wide["Scenario.CarPassenger"] <-  ((dist_cat_wide$Baseline.CarDriver + dist_cat_wide$Baseline.CarPassenger) - (dist_cat_wide$Scenario.Walk + dist_cat_wide$Scenario.Bike) -(dist_cat_wide$Baseline.Walk + dist_cat_wide$Baseline.Bike )) * (dist_cat_wide$Baseline.CarPassenger/(dist_cat_wide$Baseline.CarDriver + dist_cat_wide$Baseline.CarPassenger))

dist_cat_wide["Scenario.Bus"]   <- dist_cat_wide$Baseline.Bus
dist_cat_wide["Scenario.Rail"]  <- dist_cat_wide$Baseline.Rail
dist_cat_wide["Scenario.Truck"]  <- dist_cat_wide$Baseline.Truck
dist_cat_wide["Scenario.Motorcycle"]  <- dist_cat_wide$Baseline.Motorcycle

# Total Distance
# Drop proportions
dist_cat_wide <-  subset(dist_cat_wide, select = -c(12:14)) 
dist_cat_wide["Scenario.Total"] = rowSums(dist_cat_wide[,c(12:19)])


####################### Reshape Output - Long ########################


###################### Begin Function ###############################

format.distance <- function(dist) { 
  
  # Reshape Baseline
  dist_long_baseline <- reshape(dist[,c(1:9,11)],
                                varying = c("Baseline.Walk", "Baseline.Bike", "Baseline.CarDriver", "Baseline.CarPassenger", "Baseline.Bus", "Baseline.Rail",  "Baseline.Motorcycle", "Baseline.Truck", "Baseline.Total"),
                                v.names = "Baseline",
                                timevar = "Mode",
                                times = c("Baseline.Walk", "Baseline.Bike", "Baseline.CarDriver", "Baseline.CarPassenger", "Baseline.Bus", "Baseline.Rail",  "Baseline.Motorcycle", "Baseline.Truck", "Baseline.Total"),
                                new.row.names = 1:54,
                                direction = "long")
  
  # Central tendency
  dist_long_baseline$Centraltend <- "mean"
  
  # rename variables
  
  dist_long_baseline$Mode <- gsub("Baseline.Walk", "Walk", dist_long_baseline$Mode)
  dist_long_baseline$Mode <- gsub("Baseline.Bike", "Bike", dist_long_baseline$Mode)
  dist_long_baseline$Mode <- gsub("Baseline.CarDriver", "CarDriver", dist_long_baseline$Mode)
  dist_long_baseline$Mode <- gsub("Baseline.CarPassenger", "CarPassenger", dist_long_baseline$Mode)
  dist_long_baseline$Mode <- gsub("Baseline.Bus", "Bus", dist_long_baseline$Mode)
  dist_long_baseline$Mode <- gsub("Baseline.Rail", "Rail", dist_long_baseline$Mode)
  dist_long_baseline$Mode <- gsub("Baseline.Truck", "Truck", dist_long_baseline$Mode)
  dist_long_baseline$Mode <- gsub("Baseline.Motorcycle", "Motorcycle", dist_long_baseline$Mode)
  dist_long_baseline$Mode <- gsub("Baseline.Total", "Total", dist_long_baseline$Mode)
  
  # Scenario Name Place Holder
  dist_long_baseline$ScenarioName <- "temp"
  
  # Time basis 
  dist_long_baseline$Time <- "annual"
  
  
  # Reshape Scenario
  dist_long_scenario <- reshape(dist[,c(1,12:20)],
                                varying = c("Scenario.Walk", "Scenario.Bike", "Scenario.CarDriver", "Scenario.CarPassenger", "Scenario.Bus", "Scenario.Rail",  "Scenario.Motorcycle", "Scenario.Truck", "Scenario.Total"),
                                v.names = "Scenario",
                                timevar = "Mode",
                                times = c("Scenario.Walk", "Scenario.Bike", "Scenario.CarDriver", "Scenario.CarPassenger", "Scenario.Bus", "Scenario.Rail",  "Scenario.Motorcycle", "Scenario.Truck", "Scenario.Total"),
                                new.row.names = 1:54,
                                direction = "long")
  
  # Central tendency
  dist_long_scenario$Centraltend <- "mean"
  
  # rename variables
  dist_long_scenario$Mode <- gsub("Scenario.Walk", "Walk", dist_long_scenario$Mode)
  dist_long_scenario$Mode <- gsub("Scenario.Bike", "Bike", dist_long_scenario$Mode)
  dist_long_scenario$Mode <- gsub("Scenario.CarDriver", "CarDriver", dist_long_scenario$Mode)
  dist_long_scenario$Mode <- gsub("Scenario.CarPassenger", "CarPassenger", dist_long_scenario$Mode)
  dist_long_scenario$Mode <- gsub("Scenario.Bus", "Bus", dist_long_scenario$Mode)
  dist_long_scenario$Mode <- gsub("Scenario.Rail", "Rail", dist_long_scenario$Mode)
  dist_long_scenario$Mode <- gsub("Scenario.Truck", "Truck", dist_long_scenario$Mode)
  dist_long_scenario$Mode <- gsub("Scenario.Motorcycle", "Motorcycle", dist_long_scenario$Mode)
  dist_long_scenario$Mode <- gsub("Scenario.Total", "Total", dist_long_scenario$Mode)
  
  #combine columns
  dist_long <- cbind(dist_long_baseline[,c("Region","ScenarioName","Centraltend","Time","Mode","Baseline")],dist_long_scenario["Scenario"])
  
  return(dist_long)
  
}
###################### End Function ###############################

# File for CSMP2020
CSMP2020 <- format.distance(dist=bmiles3)
CSMP2020$ScenarioName <- "CSMP2020"

# File for CARB2030
CARB2030 <- format.distance(dist=bmiles4)
CARB2030$ScenarioName <- "CARB2030"

# File for SCS
SCS2040 <- format.distance(dist=dscs)
SCS2040$ScenarioName <- "SCS2040"

# File for USSG
USSG <- format.distance(dist=ussg_wide)
USSG$ScenarioName <- "USSG"

# File for ShortTrips
ShortTrips <- format.distance(dist=dist_cat_wide)
ShortTrips$ScenarioName <- "ShortTrips"

# compile into a single file

scen_dist <- rbind(CSMP2020,CARB2030,SCS2040,USSG, ShortTrips)

scen_dist$PercentChange <- round(100 * (scen_dist$Scenario/scen_dist$Baseline - 1), 2)

# Write out file
write.csv(scen_dist, "ScenarioDistances.csv", row.names=FALSE)  

# Short Trips active travel times
# Reshape from wide to long

# Convert annual walk distances to weekly minutes (miles y-1 8 52 weeks/y * 3 mph/60min/hr) 
# Convert annual bike distances to weekly minutes (miles y-1 8 52 weeks/y * 12 mph/60min/hr) 
dist_cat_wide$Scenario.Walk <- dist_cat_wide$Scenario.Walk*60/(3*52)
dist_cat_wide$Scenario.Bike <- dist_cat_wide$Scenario.Bike*60/(12*52)

# Reshape Baseline
dist_cat_long <- reshape(dist_cat_wide[,c(1,12:13)],
                            varying = c("Scenario.Walk", "Scenario.Bike"),
                            v.names = "Baseline",
                            timevar = "Mode",
                            times = c("Walk", "Bike"),
                            new.row.names = 1:12,
                            direction = "long")

sat <- dist_cat_long[,c(1:3)]

# Calculate mean and medians by day and week
st_times <- time.goals(bfile=at, sfile=sat, x=1,y=1)

# Format wide
ShortTripsTimes <- format.time(fat=st_times)
ShortTripsTimes$ScenarioName <- "ShortTrips"

# compile into a single file and calculate percent change
ATtimes <- rbind(ATtimes,ShortTripsTimes)
ATtimes$PercentChange <- 100 * (ATtimes$Scenario/ATtimes$Baseline - 1) 

# Write out times file
write.csv(ATtimes, "ATtimes2.csv", row.names=FALSE)
