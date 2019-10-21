## vektorer til påfølgende oppgaver
x <- c(1, 2, 3, 10)
y <- c(4, 5, 6, 23)

## 2.1 og 2.2 Summer eller multipliser to tall
compute_vector <- function(x, y, operator) {
  
  if(operator == "+") {
    res = x + y
  }
  
  if(operator == "*") {
    res = x * y
  }
  
  print(res)
}
compute_vector(x, y, operator = "+")
compute_vector(x, y, operator = "*")

## 2.3 Feilmelding
kvadrer_x <- function(x = NA) {
  x^2
}
kvadrer_x()

## 2.4 Default value
### Den første verdien blir default-verdien.
cor(x, y)
cor(x, y, method = "pearson")
cor(x, y, method = "kendall")
cor(x, y, method = "spearman")

## 2.5 Finnes 2 i vektoren?
in_vector <- function(level, vector) {
  
  level %in% vector
}
in_vector(level = 2, vector = x)
in_vector(level = 2, vector = y)

## 2.6 Dårlig idè
### sum, mean og mtcars er base R funksjoner som ligger i global environment. Dersom vi lager nye funksjoner med samme navn overskriver vi de disse.

## 2.7 Partall
er_partall <- function(integer) {
  
dplyr::if_else(integer %% 2 == 0, TRUE, FALSE)
  
}
er_partall(2)

## 2.8 IF ELSE IFELSE
### IF er raskere ettersom den kun evaluerer det første argumentet, mens IFELSE evaluerer begge argumentene

## 2.9 Funksjonen virker
lag_histogram <-  function(.data, variabel) {
  
  bins = .data %>% select({{variabel}}) %>% unique() %>% nrow()
  
  .data %>% 
    ggplot(aes(x = {{variabel}})) + 
    geom_histogram(fill = 'forestgreen', color = 'black', bins = bins)
  
}
lag_histogram(.data = mtcars, variabel = cyl)

## 3.0 God
god_ <- function(date = lubridate::now()) {
  
  hour <- hour(date)
  
  case_when(hour < 3 ~ "God kveld",
            hour < 12 ~ "God formiddag",
            hour < 18 ~ "God ettermiddag",
            hour >= 18 ~ "God kveld")
  
}
god_()

## 4.0 Konverteringer 1
convert_temp <- function(temperature, from_scale, to_scale) {
  
  if(from_scale == "F" & to_scale == "C") {
    
    new_temp = paste(
      round((temperature - 32) * 5/9, 1), 
      "°C")

  }
  
  if(from_scale == "C" & to_scale == "F") {
    
    new_temp = paste(
      round((temperature * 9/5) + 32, 1),
      "F")
        
  }
  
  print(new_temp)
}
convert_temp(temperature = 32, from_scale = "F", to_scale = "C")
convert_temp(temperature = 64, from_scale = "F", to_scale = "C")
convert_temp(temperature = 0, from_scale = "C", to_scale = "F")
convert_temp(temperature = 100, from_scale = "C", to_scale = "F")

# 5.0 DingDong
dingdong <- function(v) {
  
v <- data.frame(v)


v <- v %>%
  mutate(v = case_when(v %% 3 == 0 & v %% 5 == 0 ~ "DingDong",
                       v %% 3 == 0 ~ "Ding",
                       v %% 5 == 0 ~ "Dong",
                       TRUE ~ as.character(v))) 

as.vector(t(v))
  
}
dingdong(c(1:100))








