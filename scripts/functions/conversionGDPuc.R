# library(GDPuc)

conversion_gdpuc <- function(ds, var, from, to, var_abbrev, loc, time){
  
  # function to convert variable in ds FROM a certain exchange base (ex PPP 2011 or MER 2015) TO another
  # the function returns the ds with an additional column with the calculated var with the new exchange base
  # var and var_abbrev must be vectors
  
  for (i in seq(1,length(var))){
    
    to_abbrev <- paste0(str_sub(to, -3, -1), str_extract_all(to, "\\d+"))
    label <- paste(var_abbrev[i], to_abbrev, sep="_") # label for the new column
    
    to_convert <- ds |> 
      select(
        iso3c = {{loc}},
        year  = {{time}},
        value = var[i]
      )
    
    converted_tbl <- GDPuc::convertGDP(to_convert, from, to)
    
    col_to_bind <- converted_tbl |> 
      select(value) |> 
      setNames(label)
    
    ds <- bind_cols(ds, col_to_bind)
  }
  
  ds
}


