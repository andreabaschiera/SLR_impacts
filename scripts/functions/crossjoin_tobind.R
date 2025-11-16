crossjoin_tobind <- function(df_units, ds, cols_structure, cols_values, values_to_assume = 0){
  
  structure <- ds |> distinct(across(all_of(cols_structure)))
  
  crossjoin <- df_units |> cross_join(structure)
  
  for (col in cols_values){
    crossjoin <- crossjoin |> mutate("{col}" := values_to_assume)
  }
  
  return(crossjoin)
  
}
