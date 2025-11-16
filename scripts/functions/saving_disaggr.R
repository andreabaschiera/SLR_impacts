saving_disagg <- function(ds, prefix_name, scenarios, adaptation_options, quantiles, additional_cols_todrop, path){
  
  ### rcp, quantile and adaptation must be the unique combinations in the ds
  ### the "rcp", "adaptation" and "quantile" cols can be spelled differently, be careful
  ### columns to drop (string vector) are additional because grouping variables are also dropped
  
  for (scen in scenarios){
    for (quant in quantiles){
      for (ada in adaptation_options){
        
        df_to_write <- ds |> filter(rcp == scen & quantile == quant & adaptation == ada) |> 
          select(!all_of(c("rcp", "quantile", "adaptation", additional_cols_todrop)))
        
        write_excel_csv(df_to_write, paste0(path,"/",prefix_name, scen, "_", quant, "_", ada, ".csv"))
        
      }
    }
  }
  
}
