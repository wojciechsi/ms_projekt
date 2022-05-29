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

srednia_koszty <- mean(data1[['koszty']])
srednia_obroty <- mean(data1[['obroty']])
kwantyle_koszty <- quantile(koszty)
kwantyle_obroty <- quantile(obroty)

moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

moda_koszty <- moda(data1[['koszty']])
moda_obroty <- moda(data1[['obroty']])

mediana_koszty <- median(data1[['koszty']])
mediana_obroty <- median(data1[['obroty']])

#MIARY ZRÓŻNICOWANIA
odchylenie_koszty <- sd(data1[['koszty']])
odchylenie_obroty <- sd(data1[['obroty']])

wariancja_koszty <- var(data1[['koszty']])
wariancja_obroty <- var(data1[['obroty']])

srednie_odch_bezwzg_koszty <- mad(data1[['koszty']])
srednie_odch_bezwzg_obroty <- mad(data1[['obroty']])

wsp_zmiennosci_koszty <-
  sd(data1[['koszty']]) / mean(data1[['koszty']]) * 100
wsp_zmiennosci_obroty <-
  sd(data1[['obroty']]) / mean(data1[['obroty']]) * 100

rozstep_koszty <- IQR(data1[['koszty']])
rozstep_obroty <- IQR(data1[['obroty']])

rozstep_cwiartkowy_koszty <- IQR(data1[['koszty']]) / 2
rozstep_cwiartkowy_obroty <- IQR(data1[['obroty']]) / 2

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
koszty <- data1$koszty
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
obroty <- data1$obroty
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

#podajemy odpowiednie kolumny, rozkład hipotetyczny jest normalny, przekazywane estymatory są parametrami spodziewanego rozkładu
ks.test(data1$koszty,
        "pnorm",
        mean = mean(data1$koszty),
        sd = sd(data1$koszty))
#wartość p-value jest większa od poziomu istotności (alfa = 0.05), nie ma więc podstaw do odrzucenia hipotezy

ks.test(data1$obroty,
        "pnorm",
        mean = mean(data1$obroty),
        sd = sd(data1$obroty))
#wartość p-value jest większa od poziomu istotności (alfa = 0.05), nie ma więc podstaw do odrzucenia hipotezy


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
estymator_wariancji_obroty <- var(data1[['obroty']])

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
  var(data1[['koszty']]) * nrow(data1) / (nrow(data1) - 1)
nieobc_estymator_wariancji_obroty <-
  var(data1[['obroty']]) * nrow(data1) / (nrow(data1) - 1)

statystyka_F <-
  nieobc_estymator_wariancji_obroty / nieobc_estymator_wariancji_koszty
print(statystyka_F)

alpha <- 0.05

zbior_krytyczny <- qf((1 - alpha), (nrow(data1) - 1), (nrow(data1) - 1))

# ponieważ nasza wartośc statystyki testowej jest mniejsza niż lewa krawędź przedziału obszaru krytycznego
# nie ma podstaw do odrzucenia hipotezy

statystyka_T <- (srednia_obroty - srednia_koszty) / sqrt((
  nrow(data1) * nieobc_estymator_wariancji_obroty + nrow(data1) * nieobc_estymator_wariancji_koszty
) / (nrow(data1) * 2 - 2) * (nrow(data1) * 2) / (nrow(data1) ^ 2)
)
left_edge_T <- qt((1 - alpha), (2 * nrow(data1) - 2))
#wartośc statystyki należy do obszru krytycznego, zatem odrzucamy hipotezę zerową i przyjmujemy hipotezę alternatywną, że branża jest dochodowa
