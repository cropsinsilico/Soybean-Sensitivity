
years <- 2006:2015 # years of weather data

# save filepath to folder that contains the weather files as a variable
filepath <- './'

weather <- list() # Initialize weather as a list
for (i in 1:length(years)) {
  
  # For each year, read in the associated weather file to the ith entry of the list
  # E.g., for i=1, the following command will read the 2006 weather file and store it in weather[[1]]:
  # weather[[1]] <- read.csv('C:/Users/ssimo/OneDrive/Documents/GitHub/DAWN_Summer2021/Weather/2006_Bondville_IL_daylength.csv')
  
  weather[[i]] <- read.csv(paste0(filepath,years[i],'_Bondville_IL_daylength.csv'))
  
} # end for


# The following section creates 100 bootstrapped weather files, where each day is taken from
# a rondom year in the set of weather files.
bootstrap <- list()
seed_number = 1
for (i in 1:1000) {
  
  # Initialize the ith element of the bootstrap list to be a data frame 
  # that is the same size and has the same column names as the weather files
  bootstrap[[i]] <- data.frame(matrix(nrow = 365*24, ncol = ncol(weather[[1]])))
  colnames(bootstrap[[i]]) <- colnames(weather[[1]])
  
  # `yearend` is a flag that is 0 until we reach day 365, at which point it is set to 1,
  # signifying that we have reached the end of a year.
  yearend <- 0 # Initialize yearend to 0
  day <- 1 # Start on the first day of the year
  
  while (!yearend) { # while we haven't reached the end of the year
    set.seed(seed_number)  #set random seed to allow repetition
    rnum <- sample(1:length(years), 1, replace=TRUE) # randomly select a year
    inds <- which(weather[[rnum]]$doy==day) # find the indices for the day of year `day`
    
    bootstrap[[i]][inds,] <- weather[[rnum]][inds,] # store those values in the bootstrap data frame
    
    day <- day + 1 # increment to the next day
    if (day > 365) { # if day is > 365, then we have reached the end of the year
      
      yearend <- 1 # set yearend flag to 1 to exit the while loop
      
    } # end if
    seed_number = seed_number+1 
  } # end while
  if (i%%10==0) print(c("finished",i))
} # end for
print(c("final seed is",seed_number))
saveRDS(bootstrap,"bootstrap_weather_withSeed_s1000.rds")
