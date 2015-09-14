
##########################################################################################
#########################################################
#############################

# Function "rawNetCDF.fut" computes climatic averages from netCDF files (raw outputs from GCMs) for a specified period in the future (e.g., 2080-2100). These climatic averages should be interpolated using functions "kriging.baseline" and/or "kriging.anomaly".

#############################
#########################################################
##########################################################################################

##### Follows a brief description, with examples, for each argument from function "rawNETCDF.fut"


## netCDFfile => name of file (with format .nc and in quotes, for example: "tmin21.nc") with monthly simulations you want compute climatic averages. The netCDFfile is the raw netCDF output downloaded from CMIP5/PMIP3;


## nameVarCDF => the acronym (also in quotes --> "tasmin") of the variable you want extract from fileCDF (e.g., tas: mean temperature - tasmin: minimum temp. - tasmax: maximum temp. - pr: precipitation flux);


## var => the nature of variable; should this variable (nameVarCDF) be "temperature" (tas, tasmin, tasmax) or "precipitation" (pr), written in quotes. This is mandatory to give the correct variable unit (degree Celsius or kg.m-2.mes-1);


## dim.initial & dim.final => the initial and final dimensions, respectivelly, of netCDFfile from which you want compute climatic averages; they delimit the period in the future to compute averages (e.g., 2040-2060; 2080-2100). Actually, because the output have montly simulations, these dimensions should matching "january" from fist year (e.g., 2080) to "december" from last year (e.g., 2100). To find the exact period from the sequence of simulations, you need first explore the structure of each netCDF file (use function 'str()'):

	#example:
	# > library (ncdf)
	# > objectCDF <- open.ncdf (netCDFfile.nc) #indicate the name o netCDF file to open it
	# > objectCDF #the size of dimension "time" correspond to the number of months for which there are simulations (note the netCDF files used in ecoClimate have monthly simulations)
	# > str(objectCDF) #explore the structure of netCDF file. See what information there is inside the netCDF file
	# > objectCDF$dim$time$units #see when time series begins ("days since 2005-01-01 00:00:00")
	# > objectCDF$dim$time$vals[1] # actualy, the first simulation is 380 days after 2005/01/01, so 2006/01/15. Most GCMs starts simulating climates in jan/2006 and ends in dec/2100. So time series have 1140 months or 95 years
	# > varCLIM <- get.var.ncdf (nc = objectCDF, varid = nameVarCDF)
	# > dim(varCLIM) #varCLIM is an array, then its dimension indicate the number os rows, cols and matrices. The third dimension indicate the length of simulated time series (number of months), so the first matrix corresponds to the simulations for jan/2006, second matrix correspond to the feb/2006, ..., untill the last simulated month, dec/2100 (end of time series). You need indicate what is the dimension where starts and ends the period for which you have interest to average climate. E.g., the dim=889 corresponds to jan/2080, and dim=1140 to dec/2100 (means the end of 21th century).
 


## output.file => indicate the name for the file to be saved in your working directory containing averaged climate for specified period (with file extention .txt and in quotes, for example: "tmin21.txt")





rawNetCDF.fut <- function (netCDFfile = "tmin21.nc", nameVarCDF = "tasmin", var = "temperature or precipitation", dim.initial = 889, dim.final = 1140, output.file = "tmin21.txt")
{

  library (ncdf)


  #reading netCDF file and extracting specified variable
  
  objectCDF <- open.ncdf (netCDFfile) #open netCDF

  varCLIM1 <- get.var.ncdf (nc = objectCDF, varid = nameVarCDF) #extract variable 
  varCLIM <- varCLIM1[,, dim.initial:dim.final] # extract just the specified dimensions 
  
  monthly_output <- matrix(NA, nrow(varCLIM)*ncol(varCLIM), 14, dimnames = list(c(), c('long', 'lat', 'jan', 'fev', 'mar', 'abr', 'mai', 'jun', 'jul', 'ago', 'set', 'out', 'nov', 'dez')))


   for(z in c(1:12)){
  	index <- seq(z,dim(varCLIM)[3], 12)
  	varCLIM_month <- varCLIM[,,index]
  	
  	meanVarCLIM_month <- matrix (NA, nrow(varCLIM), ncol(varCLIM)) # will save the monthly climatic averages
  	
  	for (i in 1:nrow(varCLIM)) { # fill the matrix 'meanVarCLIM_month'
    		for (j in 1:ncol(varCLIM)){
    		meanVarCLIM_month[i,j] <- mean(varCLIM_month[i,j, ], na.rm = T)
  	} #fecha for 'j'
    	} #fecha for 'i'

  monthly_output[,z+2] <- as.vector(meanVarCLIM_month) 
      	
  }#fecha for 'z'


  if (var == "precipitation") {monthly_output <- monthly_output*2592000} # changing precipitation unit from kg.m-2.s-1 to kg.m-2.month-1 (a standard month with 30 days have 2592000 sec = 60sec*60min*24hr*30days)
  
  if (var == "temperature") {monthly_output <- monthly_output-273.15} # changing temperature unit from Kelvin (SI) to Celsius


  long1 <- get.var.ncdf (nc = objectCDF, varid = "lon") # extract values of 'longitude' from netCDF file
  lat1 <- get.var.ncdf (nc = objectCDF, varid = "lat") # extract values of 'latitude' from netCDF file


  long2 <- as.vector (long1)
  long3 <- ifelse(long2 > 180, long2-360, long2) # correcting longitude to vary from -180 to 180
  longitude <- rep(long3, length(lat1))

  lat2 <- as.vector(lat1)
  latitude <- sort(rep(lat2, length(long1)), decreasing=F)
  
  monthly_output[,1] <- longitude
  monthly_output[,2] <- latitude

  write.table(monthly_output, output.file, row.names=F)

  }#ends 'function rawNetCDF.fut'

 ################
 



# to start, run function 'rawNetCDF.fut' using a command like follows below. Remember first to paste function to the R-console and to set working directory where file 'netCDFfile' is saved into.

#(examples of a command to run function 'rawNetCDF.fut')
#rawNetCDF.fut(netCDFfile = "pr_Amon_CCSM4_rcp26_r1i1p1_200601-210012.nc"; nameVarCDF = "pr"; var = "precipitation"; dim.initial = 889; dim.final = 1140; output.file = "pr_CCSM_rcp26.txt")
 
