plot_world_cum <- function(shock_type, ds_map_cum, scen, quant, adapt){
  
  # scen: rcp scenario (126, 245, 370)
  # quant: quantile (0.05, 0.5, 0.95)
  # adapt: adaptation option (Constant dike height, Constant flood protection standards, Optimal protection)
  
  if(shock_type == "capital"){
    title_plot = "Sea level rise impacts (cumulated damages 2018-2070, in %)"
    save_prefix = "assets_"
  } else if (shock_type == "population"){
    title_plot = "Sea level rise impacts (cumulated floodings 2018-2070, in %)"
    save_prefix = "pop"
  } else if (shock_type == "land"){
    title_plot = "Sea level rise impacts (cumulated land loss 2018-2070, in %)"
    save_prefix = "land"
  } else{
    return("Shock type is not a valid argument (valids are: capital, population, land).")
  }
    
  
  ds_toplot <- st_as_sf(ds_map_cum |> filter(
    rcp == scen,
    quantile == quant,
    adaptation == adapt
  ))
  
  plot <- ggplot(ds_toplot) +
    geom_sf(aes(fill = -cumulated_shock)) +
    scale_fill_gradientn(colours = c("lightyellow", "khaki1", "gold", "orange", "orangered2", "red3", "purple4"), 
                         values = rescale(c(0, 2, 5, 10, 20, 40, 100), to = c(0, 1), from = c(0, 100)), 
                         limits = c(0,100), na.value = "lightgrey") +
    theme_light() +
    labs(fill = "Cum shock (%)",
         title = title_plot,
         subtitle = paste0("RCP scenario: ",scen, " / Quantile: ", quant, " / Adaptation Option: ", adapt),
         caption = "Data relaborated from DIVA model")
  
  # asking whether to save the plot
  ans <- tolower(trimws(readline(prompt = "Save plot? (y/n): ")))
  if (ans %in% c("y", "yes")) {
    ggsave(filename = paste0(save_prefix,"cumshock_",scen,"_",quant,"_",trimws(adapt),".png"), path = paste0("outputs/",shock_type,"/maps"))
    #width = 8, height = 6 for smaller PNGs
    plot
  } else if (ans %in% c("n", "no")) {
    print("Map is only shown in the Plots window.")
    plot
  } else {
    "Provided answer is not correct."
  }
  
}
