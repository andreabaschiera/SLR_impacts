AddIso <- function(ds) {
  stopifnot(is.data.frame(ds))
  
  # countrycode library needs to be loaded
  # dataset needs to have a column named iso2 or iso3
  # function adds column with iso2 or iso3 depending on what's missing
  # from iso2 to iso3, function checks whether United Kingdom and Greece are encoded differently in iso2
  
  cols <- colnames(ds)
  pos <- min(which(str_detect(tolower(cols), "iso")))
  col_name <- cols[pos]
  
  if(any(str_detect(tolower(cols), "iso"))){
    if (str_detect(col_name, "2")) {
      iso2c <- pull(unique(ds[col_name]))
      iso2c_new <- iso2c
      if("UK" %in% iso2c_new){
        iso2c_new <- recode(iso2c_new, UK = "GB", .default = iso2c_new)}
      if("EL" %in% iso2c_new){
        iso2c_new <- recode(iso2c_new, EL = "GR", .default = iso2c_new)}
      iso3c <- countrycode(iso2c_new, origin = "iso2c", destination = "iso3c")
      iso_conv <- data.frame(iso2 = iso2c,iso3 = iso3c)
      ds <- left_join(ds,iso_conv, by = setNames(col_name, "iso2")) |>
        relocate(iso3, .after = all_of(col_name))}
    else {
      iso3c <- pull(unique(ds[col_name]))
      iso2c <- countrycode(iso3c, origin="iso3c",destination = "iso2c")
      iso_conv <- data.frame(iso2 = iso2c,iso3 = iso3c)
      ds <- left_join(ds,iso_conv, by = setNames(col_name, "iso3")) |>
        relocate(iso2, .before = all_of(col_name))}
    ds
  }
  else{stop("No columns named iso2 or iso3")}
  
}
