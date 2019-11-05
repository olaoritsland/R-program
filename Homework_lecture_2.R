


# JP: 
# Veldig bra jobbet, Ola! Jeg har lagt inn noen kommentarer markert med "JP" i scriptet ditt. 


# JP:
# Kommentar til bruk av github: 
# Bra at du har committet underveis! Ta gjerne en titt på best practice for 
# å skrive commit message i slidene fra sesjon 2, jeg tror det vil være veldig 
# nyttig for deg på prosjekt. 



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

# JP:
# To fluer i en smekk, kult! 

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

# JP:
# Bra!
# Tips: "integer %% 2 == 0" returnerer allerede TRUE eller FALSE pga sammenligningen
# "==". Det er derfor ikke nødvendig med if_else() som returnerer akkurat det samme. 
# Da kunne man fått kortere og mer lettlest funksjon. Se løsningsforslag for eksempel. 


## 2.8 IF ELSE IFELSE
### IF er raskere ettersom den kun evaluerer det første argumentet, mens IFELSE evaluerer begge argumentene

# JP:
# Riktig det, men hovedpoenget er at IF - som kun evaluerer det første argumentet - 
# bør brukes som control flow operator til å styre hva som skjer i et script / 
# en funksjon, mens ifelse() passer utmerket til vektorisert transformasjon. 


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

# JP:
# Nydelig! Enkelt og greit :) 

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

# JP:
# Veldig kult! 
# 
# Dersom du ville ha utvidet på denne funksjonen kunne man f.eks. ha hatt logikk
# som sørget for at dersom "from_scale" var satt, men to_scale var missing (eller omvendt),
# så ble to_scale automatisk en annen verdi enn from_scale. 
# Eks: convert_temp(temperature = 32, from_scale = "F") --> "0 °C"
# 
# Husk forresten at characters ikke kan brukes videre til numeriske beregninger.
# Så dersom funksjonen skulle fungere i en pipe kunne det vært bedre å returnere tallet
# uten "F" eller "C" bak. Dette kommer kun an på hva man planlegger å bruke funksjonen til. 



## 5.0 DingDong
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


## 6.0 Print all
print_all <- function(data) {
  
  print(data, n = nrow(data))
}
iris %>% as_tibble %>% head(25) %>% print_all()

## 7.0 Passordgenerator
gen_pwd <- function(length = 16) {
  
  string <- sample(c(LETTERS, letters, 0:9), size = length)
  
  paste(string, collapse = "")
}
gen_pwd()

# JP:
# Veldig bra løsning! Husk forresten at i "sample" så er "replace" FALSE som default. 
# Dermed kan man kun bruke ett tegn én gang slik det er nå. Dersom man setter replace
# til TRUE kan får man ett passord som er over 8 ganger så sikkert (8.33 ganger flere
# mulige kombinasjoner gitt et 16-bokstavs passord).


## 8.0 NA-funksjoner
###na_summarise <- function(data, var) {
###  
###  data %>% 
###    mutate(N = n()) %>% 
###    group_by({{var}}) %>% 
###    summarise(n = n(), "%" = n*100/mean(N)) %>% 
###    filter(is.na({{var}}))
###
###}
###
###na_summarise(data = airquality, var = Ozone)
###na_summarise(data = airquality, var = Month)
###na_summarise(data = airquality, var = Solar.R)
###na_summarise(data = airquality, var = Wind)
###na_summarise(data = airquality, var = Temp)
###na_summarise(data = airquality, var = Day)
###---------------------------------------------
###map(.x = airquality, .f = ~na_summarise(data = airquality, var = .))
###................................
summarise_levels <- function(data, var, NAcount = FALSE, topN = 2) {
  
  if(NAcount) {
    
  data %>% 
    mutate(N = n()) %>% 
    group_by({{var}}) %>% 
    summarise(n = n(), "%" = n*100/mean(N)) %>% 
    filter(is.na({{var}}))
    
  } else {
  
  data %>% 
    mutate(N = n()) %>% 
    group_by({{var}}) %>% 
    summarise(n = n(), "%" = n*100/mean(N)) %>% 
    top_n(topN) %>% 
    arrange(-n)
  }
  
}
summarise_levels(airquality, Ozone, NAcount = TRUE)
summarise_levels(airquality, Ozone, topN = 5)
###---------------------
na_summarise_all <- function(x) {

data %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_each(funs(sum(is.na(.)))) 

}
na_summarise_all(airquality)

# JP:
# Denne funket ikke for meg. 

## 9.0 Innlasting av mange datafiler
file_paths <- fs::dir_ls(path = "./data/Karakterer_per_fylke", regexp = "\\.csv$")

df <- map_dfr(file_paths, read_csv2) %>% drop_na() %>%
  separate(variabel, into = c("variabel", "year"), sep = "_") %>% 
  filter(variabel == "Gjennomsnittlig standpunktkarakter")

finn_beste_fylke <- function(.year, .fag, .kjonn) {
  
  if(!missing(.year)) {
    df <- df %>% filter(year == .year)
    
    if(!is.element(.year, df$year)) {
      stop(paste(.year, "is not in data"))
    }
  }
  if(!missing(.fag)) {
    df <- df %>% filter(fag == .fag)
  }
  if(!missing(.kjonn)) {
    df <- df %>% filter(kjonn == .kjonn)
  }
  
  df %>% 
    filter(fylke != "0 Hele landet") %>% 
    group_by(fylke) %>% 
    summarise(Gjennomsnittlig_karakter = mean(values)) %>% 
    arrange(-Gjennomsnittlig_karakter) %>% 
    top_n(10)
}
finn_beste_fylke(.kjonn = "Jenter", .fag = "Mat og helse")

# JP:
# Lurt å legge til kjønn og fag som ekstra variabler å filtrere på! 


## 10 Fleksible plots
lag_plot <- function(.data, .x, .y, plot_type = "dot", color_by, theme = theme_minimal()) {
  
  #if(!is.factor(.data$.x) & plot_type == "bar") {
  # stop(paste(.x, " is not categorical. Consider using plot_type 'dot'"))
  #}
  
  if(!missing(color_by)) {
    p <- .data %>%
      ggplot(aes({{.x}},{{.y}}, color = {{color_by}}))
  } else {
    p <- .data %>%
      ggplot(aes({{.x}},{{.y}}))
  }
  
  if(plot_type == "dot") {
      plot <- p + geom_point() +
        theme
  }
  
  if(plot_type == "bar") {
    
    plot <- p + geom_bar(stat = 'identity') +
      theme
  }
  return(plot)
}
lag_plot(iris, Sepal.Length, Sepal.Width, color_by = Species, theme = theme_bw())
lag_plot(.data = iris, .x = Species, .y = Sepal.Length, plot_type = "bar", theme = theme_bw())
lag_plot(mtcars, as.factor(cyl), hp,plot_type = "bar")
lag_plot(mtcars, mpg, hp, color_by = as.factor(cyl), theme = theme_void())

# JP:
# Knallbra!

## 11 Konverteringer 2
mpg_to_kpl <- function(mpg) {
  
  mpg * 0.425144 #US gallon
  
}
mpg_to_kpl(mtcars$mpg[1])

mpg_plot <- function(.y) {
  
  mtcars %>% 
    mutate(kpl = mpg_to_kpl(mpg)) %>% 
    ggplot(aes(kpl, {{.y}})) +
    geom_point(color = 'forestgreen', fill = 'black', size = 2) +
    geom_smooth(se = FALSE, color = 'darkgreen') +
    theme_minimal()
}
mpg_plot(hp)
map(.x = mtcars, .f = ~mpg_plot(.y = .))

## 12 Rescale
if (!require(quantmod)) {
  install.packages("quantmod")
}

if (!require(reshape)) {
  install.packages("reshape")
}

# Last ned aksjedata
quantmod::getSymbols(c("GOOGL", "AAPL", "FB"), from = "2019-01-01")

# Lag liste av dataframes. Gjør radnavn til kolonne med navn "Dato"
list_of_df <- map(.x = list(FB = FB, GOOGL = GOOGL, AAPL = AAPL), 
                  .f = ~(rownames_to_column(.data = as.data.frame(.), 
                                            var = "Dato")))

# Join alle dfs på Dato
df_stocks <- reshape::merge_recurse(list_of_df)

# Plot
df_stocks %>% 
  select(Dato, contains("Close")) %>% 
  pivot_longer(-Dato, names_to = "Stock", values_to = "Close_rescaled") %>% 
  ggplot(aes(x = as.Date(Dato), y = Close_rescaled, col = Stock, group = Stock)) + 
  geom_line() + 
  theme_bw()

## 12 Rescale
rescale_close <- function(close, rescale_date, date_col) {
  
  r_factor = if_else(date_col == rescale_date, 100, close/lag(close))
  
  cumprod(r_factor)
  
}

df_stocks %>% 
  select(Dato, contains("Close")) %>% 
  pivot_longer(-Dato, names_to = "Stock", values_to = "Close") %>% 
  arrange(Dato) %>%
  group_by(Stock) %>% 
  mutate(Close_rescaled = rescale_close(Close, rescale_date = "2019-01-02", date_col = Dato),
         Dato = lubridate::date(Dato)) %>%
  
  
  ggplot(aes(Dato, Close_rescaled, col = Stock, group = Stock)) +
  geom_line() +
  theme_bw()


## 13 Navngivning
has_prefix <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}
remove_last_value <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}
replicate_y_to_length_of_x <- function(x, y) {
  rep(y, length.out = length(x))
}
max_min <- function(x, y, z) {
  pmin(pmax(x, y), z)
}

## 14 Bli kvitt for-løkken
str_c(letters, collapse = "")
sd(x)
x <- runif(100)
out <- cumsum(x)

## 15 Feilmeldinger


## 16 Lag en eksisterende funksjon
which_max <- function(x) {
  which(max(x) == x)
}
which_max(runif(10, 0, 10))

# JP:
# Elegant! Men merk at denne funksjonen vil returnere alle index-posisjonene som
# er lik maksverdi (mens which.max() kun returnerer det første). Feks: 
# which_max(c(10, 8, 10))
# which.max(c(10, 8, 10))
# 
# Dette kan være akkurat det du er ute etter, men hvis ikke så fikser du det lett
# med en "[1]" bak `which(max(x) == x)` inne i funksjonen. 


## 17.1
map(mtcars, mean)

## 17.2
map(.x = mtcars, .f = ~mean(x = ., trim = 0.05))

## 17.3
nycflights13::flights %>% 
  map(.f = class)

## 17.4
iris %>% 
  map(.f = n_distinct)

## 17.5 
list <- c(1:4)
list^2

vector <- c(1:10)
vector^2

## 18
return_mean <- function(.data, plot = TRUE) {
  
  .data <- .data %>% 
    select_if(~ is.numeric(.)) %>% 
    map_df(.f = mean)
  
  print(.data)

  if(plot) {
    
  .data %>% 
      pivot_longer(everything(), names_to = "variable", values_to = "mean") %>% 
      ggplot(aes(variable, mean)) +
      geom_bar(stat = 'identity', fill = 'forestgreen', color = 'black') +
      geom_label(aes(label = round(mean, 2))) + 
      theme_bw() +
      coord_flip()
  }
}
return_mean(iris)

# JP:
# Nice! 

## 19
library(ranger)

mod_ranger <- ranger::ranger(Species ~ ., 
                     data = iris, 
                     importance = "permutation")


plot_importance <- function(mod_ranger) {
  
mod_ranger %>% 
  ranger::importance() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "variable") %>% 
  rename(importance = ".") %>% 
  mutate(variable = fct_reorder(variable, importance, .desc = FALSE)) %>% 
  
  ggplot(aes(variable, importance)) +
  geom_bar(stat = 'identity', fill = 'forestgreen', color = 'black') +
  theme_bw() +
  coord_flip()
  
}
plot_importance(mod_ranger)

# JP:
# Veldig bra!
# Tips: rename er en av de funksjoene som er lurt å spesifisere hvilken pakke de kommer fra.
# Her: dplyr::rename(). Jeg fikk først feilmelding her fordi reshape (som ble brukt under Rescale-oppgaven)
# også har en rename-funksjon.



