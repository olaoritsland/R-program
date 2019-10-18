gangmed2 <- function(x=0, output_som_melding = TRUE) {
  
  if(is.na(x)) {
    stop(paste("x is NA"))
  } 
  
   if(output_som_melding) {
    return(paste("Det dobbelte av ", x, " er ", 2 * x, "."))
  }
  
  
  x*2
}
gangmed2(NA)


#----------------
fornavn <- c("Robin", "Markus", "Linn")
etternavn <- c("Andersen", "Mortensen", "Gjesdal")
data.frame(fornavn = fornavn,
           etternavn = etternavn)

full_name <- function(fornavn, etternavn) {
  as.data.frame(paste(fornavn, etternavn))
}

full_name2 <- function(fornavn, etternavn) {
  
  df <- data.frame(fornavn = fornavn,
             etternavn = etternavn)
  
  df %>% 
    mutate(navn = str_to_upper(paste(fornavn, etternavn))) %>% 
    select(navn) %>% 
    print()
}

full_name2(fornavn, etternavn)

#-----------------

plot_hist <- function(data, var) {

  data %>% 
    ggplot() +
    aes({{var}}) +
    geom_histogram(fill = 'forestgreen', color = 'black') 
}

purrr::map(.x = mtcars, .f = ~plot_hist(data = mtcars, var = .))


purrr::map_df(.x = mtcars, .f = median)

##-----------------

















