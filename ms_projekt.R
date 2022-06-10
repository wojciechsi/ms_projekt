##############################
#######WCZYTANIE DANYCH#######
##############################

data1 <- read.csv(file = 'MS_projekt.csv', sep = ';')
koszty <- data1$koszty
obroty <- data1$obroty
##############################
#========= ZADANIE 1 =========
##############################

#MIARY POŁOŻENIA

srednia_koszty <- mean(koszty)
srednia_obroty <- mean(obroty)
kwantyle_koszty <- quantile(koszty)
kwantyle_obroty <- quantile(obroty)

moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

moda_koszty <- moda(koszty)
moda_obroty <- moda(obroty)

mediana_koszty <- median(koszty)
mediana_obroty <- median(obroty)

#MIARY ZRÓŻNICOWANIA
odchylenie_koszty <- sd(koszty)
odchylenie_obroty <- sd(obroty)

wariancja_koszty <- var(koszty)
wariancja_obroty <- var(obroty)

srednie_odch_bezwzg_koszty <- mad(koszty)
srednie_odch_bezwzg_obroty <- mad(obroty)

wsp_zmiennosci_koszty <-
  sd(koszty) / mean(koszty) * 100
wsp_zmiennosci_obroty <-
  sd(obroty) / mean(obroty) * 100

rozstep_koszty <- IQR(koszty)
rozstep_obroty <- IQR(obroty)

rozstep_cwiartkowy_koszty <- IQR(koszty) / 2
rozstep_cwiartkowy_obroty <- IQR(obroty) / 2

#MIARY ASYMETRII
wspolczynnik_asymetrii <-
  function(tmp_data, tmp_row_name, tmp_odchylenie) {
    moment_centr_tmp <- 0
    for (xi in 1:(nrow(tmp_data))) {
      moment_centr_tmp <-
        moment_centr_tmp + (tmp_data[xi, tmp_row_name] - mean(tmp_data[[tmp_row_name]])) ^
        3
    }
    moment_centr <- 1 / (nrow(tmp_data) - 1) * moment_centr_tmp
    wspolczynnik_asymetrii <- (moment_centr) / (tmp_odchylenie ^ 3)
    return(wspolczynnik_asymetrii)
  }

wspolczynnik_asymetrii_koszty <-
  wspolczynnik_asymetrii(data1, 'koszty', odchylenie_koszty)
wspolczynnik_asymetrii_obroty <-
  wspolczynnik_asymetrii(data1, 'obroty', odchylenie_obroty)

kurtoza <- function(tmp_data, tmp_row_name, tmp_odchylenie) {
  moment_centr_4_tmp <- 0
  for (xi in 1:nrow(tmp_data)) {
    moment_centr_4_tmp <-
      moment_centr_4_tmp + (tmp_data[xi, tmp_row_name] - mean(tmp_data[[tmp_row_name]])) ^
      4
  }
  moment_centr_4 <- 1 / (nrow(tmp_data) - 1) * moment_centr_4_tmp
  tmp_kurtoza <- moment_centr_4 / (tmp_odchylenie ^ 4)
}

kurtoza_koszty <- kurtoza(data1, 'koszty', odchylenie_koszty)
kurtoza_obroty <- kurtoza(data1, 'obroty', odchylenie_obroty)
print(paste("Kurtoza obrotów: ", kurtoza_obroty))
print(paste("Skośność obrotów: ",wspolczynnik_asymetrii_obroty))
print(paste("Kurtoza kosztów: ", kurtoza_koszty))
print(paste("Skośność kosztów: ",wspolczynnik_asymetrii_koszty))
#KOSZTY HISTOGRAM

liczba_przedzialow_koszty <- sqrt(nrow(data1))
szerokosc_przedzialu_koszty <- (max(koszty) - min(koszty)) / liczba_przedzialow_koszty
bins_koszty <-
  seq(min(koszty), max(koszty), by = szerokosc_przedzialu_koszty)
szereg_koszty <- table(cut(koszty, 5))
print(transform(szereg_koszty)) #print szereg
hist(
  koszty,
  breaks = bins_koszty,
  main = "Koszty",
  xlab = "Koszty prowadzenia działaności w tys. zł",
  ylab = "Częstość występowania",
  col = "orange",
  axes = F
)
axis(2)
axis(1, at = seq(min(koszty), max(koszty), by=szerokosc_przedzialu_koszty), labels = seq(min(koszty), max(koszty), by = szerokosc_przedzialu_koszty))

#OBROTY HISTOGRAM

liczba_przedzialow_obroty <- sqrt(nrow(data1))
szerokosc_przedzialu_obroty <- (max(obroty) - min(obroty)) / liczba_przedzialow_obroty
bins_obroty <-
  seq(min(obroty), max(obroty), by = szerokosc_przedzialu_obroty)
szereg_obroty <- table(cut(obroty, 5))
hist(
  obroty,
  breaks = bins_obroty,
  main = "Obroty",
  xlab = "Kwota rocznych obrotów w tys. zł",
  ylab = "Częstość występowania",
  col = "red",
  axes = F
)
axis(2)
axis(1, at = seq(min(obroty), max(obroty), by=szerokosc_przedzialu_obroty) , labels = seq(min(obroty), max(obroty), by = szerokosc_przedzialu_obroty))

##############################
#========= ZADANIE 2 =========
##############################

#używając ks.test

#podajemy odpowiednie kolumny, rozkład hipotetyczny jest normalny, przekazywane estymatory są parametrami spodziewanego rozkładu
ks.test(koszty,
        "pnorm",
        mean = mean(koszty),
        sd = sd(koszty))
#wartość p-value jest większa od poziomu istotności (alfa = 0.05), nie ma więc podstaw do odrzucenia hipotezy

ks.test(obroty,
        "pnorm",
        mean = mean(obroty),
        sd = sd(obroty))
#wartość p-value jest większa od poziomu istotności (alfa = 0.05), nie ma więc podstaw do odrzucenia hipotezy


#"na piechotę"

f_podcalk <- function(x){exp((-x^2)/2) / sqrt(2*pi)}

test_k <- function(tmp_data, tmp_data_mean, tmp_data_deviation) {
  tmp_data_sorted <- sort(tmp_data)
  i <- seq(1, length(tmp_data), by=1)

  tmp_data_stan <- ((tmp_data_sorted-tmp_data_mean)/tmp_data_deviation)

  distr_t <- c()
  
  for(j in 1:length(tmp_data_stan)) {
    distr_t <- c(distr_t, integrate(f_podcalk, lower = -Inf, upper = tmp_data_stan[j])$value)
  }
  
  distr_e <- i/length(tmp_data)
  
  #składowe
  d_p <- abs(distr_e - distr_t)
  k <- (i-1)/length(tmp_data)
  d_m <- abs(k - distr_t)
  
  #statystyka Kołmogorowa
  tmp_data_tab <- c(i, tmp_data_sorted, tmp_data_stan, distr_e, distr_t, d_p, k, d_m)
  tab <- matrix(tmp_data_tab, nrow=length(tmp_data), dimnames = NULL, ncol = 8)
  
  colnames(tab) <- c('i','x','stand. x', 'i / n', 'F0(x)', '|i / n - F0(x)|','(i - 1)/n', '|(i - 1)/n - F0(x)|' )
  rownames(tab) <- seq(1, length(koszty), by=1)
  
  
  print(tab)
  
  d_p_max = max(d_p)
  d_m_max = max(d_m)
  
  return(max(d_p_max, d_m_max))
  
}

koszty_test_k <- test_k(koszty, srednia_koszty, odchylenie_koszty)

wartosc_krytyczna <- 0.2641

print('Wynik testu Kołmogorowa dla Kosztów:')
if(koszty_test_k < wartosc_krytyczna){
  print('Nie ma podstaw, do odrzucenia hipotezy, że rozkład jest normalny')
} else {
  print('Są podstawy, do odrzucenia hipotezy, że rozkład jest normalny')
}

obroty_test_k <- test_k(obroty, srednia_obroty, odchylenie_obroty)

wartosc_krytyczna <- 0.2641

print('Wynik testu Kołmogorowa dla obrotów:')
if(obroty_test_k < wartosc_krytyczna) {
  print('Nie ma podstaw, do odrzucenia hipotezy, że rozkład jest normalny')
} else {
  print('Są podstawy, do odrzucenia hipotezy, że rozkład jest normalny')
}

##############################
#========= ZADANIE 3 =========
##############################
alpha <- 0.02

#kwantyl rozkładu
q_t <- qt(1 - (alpha / 2), nrow(data1) - 1)

#przedział
left_edge_k <-
  (srednia_koszty - (q_t * (odchylenie_koszty / sqrt(nrow(
    data1
  ) - 1))))
right_edge_k <-
  (srednia_koszty + (q_t * (odchylenie_koszty / sqrt(nrow(
    data1
  ) - 1))))

#względna precyzja
blad_maksymalny_k <- (right_edge_k - left_edge_k) / 2

wzgledna_precyzja_k <- (blad_maksymalny_k / srednia_koszty * 100)

# Mamy podstawy do uogólnienia przedziału ufności
##############################
#=======ZADANIE 4=============
##############################

#oszacować przedziałowo wariancje obrotów
estymator_wariancji_obroty <- var(obroty)

alpha <- 0.02
chi_kwadrat1 <- qchisq((1 - (alpha / 2)), (nrow(data1) - 1))
chi_kwadrat2 <- qchisq((alpha / 2), (nrow(data1) - 1))

#przedziaŁ:
left_edge <-
  (nrow(data1) * estymator_wariancji_obroty) / (chi_kwadrat1)
right_edge <-
  (nrow(data1) * estymator_wariancji_obroty) / (chi_kwadrat2)

#względna precyzja
blad_maksymalny <- (right_edge - left_edge) / 2

wzgledna_precyzja <-
  blad_maksymalny / estymator_wariancji_obroty * 100

#WNIOSEK: to jest niemiarodajne / "nie mamy podstaw do uogólnienia otrzymanego przedziału ufności"
##############################
#========= ZADANIE 5 =========
##############################

#H0: wartość średnia obrotów jest równa wartości średniej kosztów
#H1: wartość średnia obrotów jest większa od wartości średniej kosztów

#nie wiemy, czy wariancje są równe
#H0: wariancja_obroty = wariancja_koszty
#H1: wariancja_obroty > wariancja_koszty

nieobc_estymator_wariancji_koszty <-
  var(koszty) * nrow(data1) / (nrow(data1) - 1)
nieobc_estymator_wariancji_obroty <-
  var(obroty) * nrow(data1) / (nrow(data1) - 1)

statystyka_F <-
  nieobc_estymator_wariancji_obroty / nieobc_estymator_wariancji_koszty
print(statystyka_F)

alpha <- 0.05

zbior_krytyczny_left_edge <- qf((1 - alpha), (nrow(data1) - 1), (nrow(data1) - 1))
print("Test równości dwóch wariancji: ")
if(statystyka_F < zbior_krytyczny_left_edge){
  print("Brak podstaw do odrzucenia hipotezy zerowej.")
}else{
  print("Odrzucamy hipotezę zerową, przyjmujemy hipotezę alternatywną - wariancje kosztów i obrotów są różne.")
}
# ponieważ nasza wartośc statystyki testowej jest mniejsza niż lewa krawędź przedziału obszaru krytycznego
# nie ma podstaw do odrzucenia hipotezy

statystyka_T <- (srednia_obroty - srednia_koszty) / sqrt((
  nrow(data1) * nieobc_estymator_wariancji_obroty + nrow(data1) * nieobc_estymator_wariancji_koszty
) / (nrow(data1) * 2 - 2) * (nrow(data1) * 2) / (nrow(data1) ^ 2)
)
left_edge_T <- qt((1 - alpha), (2 * nrow(data1) - 2))
if(statystyka_T < left_edge_T){
  print("Brak podstaw do odrzucenia hipotezy zerowej.")
}else{
  print("Odrzucamy hipotezę zerową, przyjmujemy hipotezę alternatywną - branża jest dochodowa.")
}
