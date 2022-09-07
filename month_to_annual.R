month_to_annual<- function(data_var) {
  
  data_annual <- array(dim = c(dim(data_var)[1],dim(data_var)[2],dim(data_var)[3]/12))
  
  for (lon in 1:dim(data_var)[1]) {
    for (lat in 1:dim(data_var)[2]) {
      c=1
      for (year in 1:((dim(data_var)[3])/12)) {
        s=0
        for (month in 1:12) {
          if (month==1 || month==3 || month==5 || month==7 || month==8 || month==10 || month==12){
            s=s+data_var[lon,lat,c]*31*24*60*60
            c=c+1;}
          else if (month==2){
            s=s+data_var[lon,lat,c]*28*24*60*60
            c=c+1;
          }
          else {
            s=s+data_var[lon,lat,c]*30*24*60*60
            c=c+1;
          }
        }
        data_annual[lon,lat,year]=s
      }
    }
  }
  
 return(data_annual) 
  
}
  