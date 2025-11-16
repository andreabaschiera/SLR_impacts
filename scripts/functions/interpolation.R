lin_interp_ongroups <- function(ds, time, time_new, groupby_vars, interp_vars) {

  ### groupby_vars and interp_vars can be vectors of columns
  
  grouped <- ds |> filter(if_all(all_of(interp_vars),~!is.na(.))) |> 
    group_by(pick(all_of(groupby_vars)))
  
  for (col in interp_vars){
    
    isfirst <- if_else(interp_vars[1] == col, TRUE, FALSE)
    
    result <- grouped |> group_modify( ~ {
      interp <- approx(.x$time, .x[[col]], xout = time_new, rule = 2)
      tibble(time = interp$x, "{col}_yearly" := interp$y)
    }) |>  ungroup()
    
    if(isfirst == TRUE){
      final <- result
    }
    else{
      final <- bind_cols(final, result |> select(last_col()))
    }
  }
  
  return(final)
 
}
