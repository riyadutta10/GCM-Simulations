library(ncdf4)
library(sp)
library(raster) 
library(rgdal) 
library(ggplot2)
library(assertthat)

final_series <- array(dim = c(8,34))

##################################################################################################################


# Extract the data from the nc file_ GCM HadGEM3

filename <- "pr_Amon_HadGEM3-GC31-HM_highresSST-present_r1i1p1f1_gn_198101-198112.nc"
nc_data <- nc_open(filename)
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")

c=1
pr.slice <- array(dim = c(dim(lon)[1],dim(lat)[1],34*12))
for (year in 1981:2014){
  filename <- paste("pr_Amon_HadGEM3-GC31-HM_highresSST-present_r1i1p1f1_gn_",as.character(year),"01-",as.character(year),"12.nc",sep ="")
  nc_data <- nc_open(filename)
  pr.slice[,,c:(c+11)] <- ncvar_get(nc_data, 'pr')
  c <- c+12
}

#convert monthly series (in kg/ m2 s) to annual series (mm)
source("month_to_annual.R")
pr_annual <- month_to_annual(pr.slice)

#Calculate the area for each grid to calculate the global mean
source("GridArea.R")
area_weight <- GridArea(lon, lat, verbose = FALSE)

#Weighted area average to calculate the global mean values
for (year in 1:dim(pr_annual)[3]) {
  a<-pr_annual[,,year]*as.vector(area_weight)
  final_series[1,year]<-Reduce(`+`,a)/Reduce('+',area_weight)
}

plot.ts( final_series[1,],col = 1,ylim = c(1000, 1200))

##################################################################################################################


# Extract the data from the nc file_ GCM EC Earth

filename <- "pr_Amon_EC-Earth3P-HR_highresSST-present_r1i1p1f1_gr_198101-198112.nc"
nc_data <- nc_open(filename)
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")

c=1
pr.slice <- array(dim = c(dim(lon)[1],dim(lat)[1],34*12))
for (year in 1981:2014){
  filename <- paste("pr_Amon_EC-Earth3P-HR_highresSST-present_r1i1p1f1_gr_",as.character(year),"01-",as.character(year),"12.nc",sep ="")
  nc_data <- nc_open(filename)
  pr.slice[,,c:(c+11)] <- ncvar_get(nc_data, 'pr')
  c <- c+12
}

#convert monthly series (in kg/ m2 s) to annual series (mm)
source("month_to_annual.R")
pr_annual <- month_to_annual(pr.slice)

#Calculate the area for each grid to calculate the global mean
source("GridArea.R")
area_weight <- GridArea(lon, lat, verbose = FALSE)

#Weighted area average to calculate the global mean values
for (year in 1:dim(pr_annual)[3]) {
  a<-pr_annual[,,year]*as.vector(area_weight)
  final_series[2,year]<-Reduce(`+`,a)/Reduce('+',area_weight)
}

lines( final_series[2,],col = 2)

##################################################################################################################


# Extract the data from the nc file_ GCM MPI

filename <- "pr_Amon_MPI-ESM1-2-HR_highresSST-present_r1i1p1f1_gn_198101-198112.nc"
nc_data <- nc_open(filename)
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")

c=1
pr.slice <- array(dim = c(dim(lon)[1],dim(lat)[1],34*12))
for (year in 1981:2014){
  filename <- paste("pr_Amon_MPI-ESM1-2-HR_highresSST-present_r1i1p1f1_gn_",as.character(year),"01-",as.character(year),"12.nc",sep ="")
  nc_data <- nc_open(filename)
  pr.slice[,,c:(c+11)] <- ncvar_get(nc_data, 'pr')
  c <- c+12
}

#convert monthly series (in kg/ m2 s) to annual series (mm)
source("month_to_annual.R")
pr_annual <- month_to_annual(pr.slice)

#Calculate the area for each grid to calculate the global mean
source("GridArea.R")
area_weight <- GridArea(lon, lat, verbose = FALSE)

#Weighted area average to calculate the global mean values
for (year in 1:dim(pr_annual)[3]) {
  a<-pr_annual[,,year]*as.vector(area_weight)
  final_series[3,year]<-Reduce(`+`,a)/Reduce('+',area_weight)
}

lines( final_series[3,],col = 3)

##################################################################################################################

# Extract the data from the nc file_ GCM IPSL

nc_data <- nc_open('pr_Amon_IPSL-CM6A-ATM-HR_highresSST-present_r1i1p1f1_gr_195001-201412.nc')
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
pr.array <- ncvar_get(nc_data, 'pr')
pr.slice <- pr.array[, , 373:780] # extract the data from 198101 to 201412

#convert monthly series (in kg/ m2 s) to annual series (mm)
source("month_to_annual.R")
pr_annual <- month_to_annual(pr.slice)

#Calculate the area for each grid to calculate the global mean
source("GridArea.R")
area_weight <- GridArea(lon, lat, verbose = FALSE)

#Weighted area average to calculate the global mean values
for (year in 1:dim(pr_annual)[3]) {
  a<-pr_annual[,,year]*as.vector(area_weight)
  final_series[4,year]<-Reduce(`+`,a)/Reduce('+',area_weight)
}

lines( final_series[4,],col = 4)

##################################################################################################################

# Extract the data from the nc file_ GCM CNRM

nc_data <- nc_open('pr_Amon_CNRM-CM6-1-HR_highresSST-present_r1i1p1f2_gr_198001-198912.nc')
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")

pr.slice <- array(dim = c(dim(lon)[1],dim(lat)[1],34*12))
pr.array <- ncvar_get(nc_data, 'pr')
pr.slice[, , 1:(9*12)] <- pr.array[,,13:dim(pr.array)[3]]

nc_data <- nc_open('pr_Amon_CNRM-CM6-1-HR_highresSST-present_r1i1p1f2_gr_199001-199912.nc')
pr.slice[, , (9*12+1):(9*12+120)] <- ncvar_get(nc_data, 'pr')

nc_data <- nc_open('pr_Amon_CNRM-CM6-1-HR_highresSST-present_r1i1p1f2_gr_200001-200912.nc')
pr.slice[, , (9*12+121):(9*12+240)] <- ncvar_get(nc_data, 'pr')

nc_data <- nc_open('pr_Amon_CNRM-CM6-1-HR_highresSST-present_r1i1p1f2_gr_201001-201412.nc')
pr.slice[, , (9*12+241):(9*12+300)] <- ncvar_get(nc_data, 'pr')


#convert monthly series (in kg/ m2 s) to annual series (mm)
source("month_to_annual.R")
pr_annual <- month_to_annual(pr.slice)

#Calculate the area for each grid to calculate the global mean
source("GridArea.R")
area_weight <- GridArea(lon, lat, verbose = FALSE)

#Weighted area average to calculate the global mean values
for (year in 1:dim(pr_annual)[3]) {
  a<-pr_annual[,,year]*as.vector(area_weight)
  final_series[5,year]<-Reduce(`+`,a)/Reduce('+',area_weight)
}

lines( final_series[5,],col = 5)


##################################################################################################################

# Extract the data from the nc file_ GCM CMCC

nc_data <- nc_open('pr_Amon_CMCC-CM2-HR4_highresSST-present_r1i1p1f1_gn_194801-201412.nc')
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
pr.array <- ncvar_get(nc_data, 'pr')
pr.slice <- pr.array[, , 397:804] # extract the data from 198101 to 201412

#convert monthly series (in kg/ m2 s) to annual series (mm)
source("month_to_annual.R")
pr_annual <- month_to_annual(pr.slice)

#Calculate the area for each grid to calculate the global mean
source("GridArea.R")
area_weight <- GridArea(lon, lat, verbose = FALSE)

#Weighted area average to calculate the global mean values
for (year in 1:dim(pr_annual)[3]) {
  a<-pr_annual[,,year]*as.vector(area_weight)
  final_series[6,year]<-Reduce(`+`,a)/Reduce('+',area_weight)
}

lines( final_series[6,],col = 6)

##################################################################################################################

# Extract the data from the nc file_ GCM ECMWF

filename <- "pr_Amon_ECMWF-IFS-HR_highresSST-present_r1i1p1f1_gr_198101-198112.nc"
nc_data <- nc_open(filename)
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")

c=1
pr.slice <- array(dim = c(dim(lon)[1],dim(lat)[1],34*12))
for (year in 1981:2014){
  filename <- paste("pr_Amon_ECMWF-IFS-HR_highresSST-present_r1i1p1f1_gr_",as.character(year),"01-",as.character(year),"12.nc",sep ="")
  nc_data <- nc_open(filename)
  pr.slice[,,c:(c+11)] <- ncvar_get(nc_data, 'pr')
  c <- c+12
}

#convert monthly series (in kg/ m2 s) to annual series (mm)
source("month_to_annual.R")
pr_annual <- month_to_annual(pr.slice)

#Calculate the area for each grid to calculate the global mean
source("GridArea.R")
area_weight <- GridArea(lon, lat, verbose = FALSE)

#Weighted area average to calculate the global mean values
for (year in 1:dim(pr_annual)[3]) {
  a<-pr_annual[,,year]*as.vector(area_weight)
  final_series[7,year]<-Reduce(`+`,a)/Reduce('+',area_weight)
}

lines( final_series[7,],col = 7)

##################################################################################################################

# Extract the data from the nc file_ GCM MRI

nc_data <- nc_open('pr_Amon_MRI-AGCM3-2-H_highresSST-present_r1i1p1f1_gn_195001-201412.nc')
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
pr.array <- ncvar_get(nc_data, 'pr')
pr.slice <- pr.array[, , 373:780] # extract the data from 198101 to 201412

#convert monthly series (in kg/ m2 s) to annual series (mm)
source("month_to_annual.R")
pr_annual <- month_to_annual(pr.slice)

#Calculate the area for each grid to calculate the global mean
source("GridArea.R")
area_weight <- GridArea(lon, lat, verbose = FALSE)

#Weighted area average to calculate the global mean values
for (year in 1:dim(pr_annual)[3]) {
  a<-pr_annual[,,year]*as.vector(area_weight)
  final_series[8,year]<-Reduce(`+`,a)/Reduce('+',area_weight)
}

lines( final_series[8,],col = 8)

##################################################################################################################

gcm_ensemble_pr <- colMeans(final_series, dims = 1)
lines(gcm_ensemble_pr,col = 10)


