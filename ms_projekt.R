data1 <- read.csv(file = 'MS_projekt.csv', sep = ';')

print(data1)

#MIARY POŁOŻENIA

srednia_koszty <- mean(data1[['koszty']])
srednia_obroty <- mean(data1[['obroty']])
print(srednia_koszty)
print(srednia_obroty)


moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


dane <- data1[['koszty']]
dane_w_przedzialach <- cut(dane, breaks = 5) #liczba klas równa jest 5, ponieważ została obliczona ze wzoru k=sqrt(25)
(szereg<-table(dane_w_przedzialach))
# @todo STWORZYĆ NOWY DATA SET - WARTOŚCI MAJĄ BYĆ ŚREDNIĄ KAŻDEGO Z PRZEDZIAŁÓW - TO BĘDZIE MODĄ, TEN PODZIAŁ WYKORZYSTAMY
# DO PLOTOWANIA HISTOGRAMU

moda_koszty <- moda(data1[['koszty']])

print(moda_koszty)

mediana_koszty <- median(data1[['koszty']])
print(mediana_koszty)



# MIARY ZRÓŻNICOWANIA
odchylenie_koszty <- sd(data1[['koszty']])
print(odchylenie_koszty)

wariancja_koszty <- var(data1[['koszty']])
print(wariancja_koszty)

srednie_odch_bezwzg_koszty <- mad(data1[['koszty']])
print(srednie_odch_bezwzg_koszty)

wsp_zmiennosci_koszty <- sd(data1[['koszty']]) / mean(data1[['koszty']]) * 100
print(wsp_zmiennosci_koszty)

rozstep_koszty <- IQR(data1[['koszty']])
print(rozstep_koszty)

rozstep_cwiartkowy_koszty <- IQR(data1[['koszty']]) / 2
print(rozstep_cwiartkowy_koszty)

#MIARY ASYMETRII
# @todo jakiśtam sizeof zamiast 25
# @todo funkcja 
moment_centr_tmp <- 0
for (xi in 1:(nrow(data1))) {
  moment_centr_tmp <- moment_centr_tmp + (data1[xi, "koszty"] - mean(data1[['koszty']]))^3
}
moment_centr_koszty <- 1/24 * moment_centr_tmp

wspolczynnik_asymetrii_koszty <- (moment_centr_koszty) / (odchylenie_koszty ^ 3)
print(wspolczynnik_asymetrii_koszty)

moment_centr_4_tmp <- 0
for (xi in 1:nrow(data1)) {
  moment_centr_4_tmp <- moment_centr_4_tmp + (data1[xi, "koszty"] - mean(data1[['koszty']]))^4
}

moment_centr_4_koszty <- 1/24 * moment_centr_4_tmp

kurtoza_koszty <- moment_centr_4_koszty / (odchylenie_koszty ^ 4)
#                                                                 - 3
#                                                                 "w starszych pracach"
print(kurtoza_koszty)

  dataKoszty <- data1[['koszty']]
  print(ks.test(data1, "pnorm"))
# @todo poziom istotności 0.05

#podajemy odpowiednie kolumny, rozkład hipotetyczny jest normalny, przekazywane estymatory są parametrami spodziewanego rozkładu
ks.test(data1$koszty, "pnorm", mean=mean(data1$koszty), sd=sd(data1$koszty))
#wartość p-value jest większa od poziomu istotności (alfa = 0.05), nie ma więc podstaw do odrzucenia hipotezy

ks.test(data1$obroty, "pnorm", mean=mean(data1$obroty), sd=sd(data1$obroty))
#wartość p-value jest większa od poziomu istotności (alfa = 0.05), nie ma więc podstaw do odrzucenia hipotezy

#oszacować przedziałowo wariancje obrotów
estymator_wariancji_obroty <- var(data1[['obroty']])
print(estymator_wariancji_obroty)

alpha <- 0.02
chi_kwadrat1 <- qchisq((1 - (alpha/2)), (nrow(data1) - 1))
chi_kwadrat2 <- qchisq((alpha/2), (nrow(data1) - 1))

#przedziaŁ:
left_edge <- (nrow(data1) * estymator_wariancji_obroty) / (chi_kwadrat1)
right_edge <- (nrow(data1) * estymator_wariancji_obroty) / (chi_kwadrat2)

print(left_edge)
print(right_edge)

#względna precyzja
blad_maksymalny <- (right_edge - left_edge) / 2
print(blad_maksymalny)

wzgledna_precyzja <- blad_maksymalny / estymator_wariancji_obroty * 100
print(wzgledna_precyzja)


#WNIOSEK: to jest niemiarodajne / "nie mamy podstaw do uogólnienia otrzymanego przedziału ufności"

#zweryfikować hipotezę "branża jest dochodowa"
#H0: wartość średnia obrotów jest równa wartości średniej kosztów
#H1: wartość średnia obrotów jest większa od wartości średniej kosztów

#nie wiemy, czy wariancje są równe
#H0: wariancja_obroty = wariancja_koszty
#H1: wariancja_obroty > wariancja_koszty

nieobc_estymator_wariancji_koszty <- var(data1[['koszty']]) * nrow(data1) / (nrow(data1)-1)
nieobc_estymator_wariancji_obroty <- var(data1[['obroty']]) * nrow(data1) / (nrow(data1)-1)

print(nieobc_estymator_wariancji_koszty)
print(nieobc_estymator_wariancji_obroty)

statystyka_F <- nieobc_estymator_wariancji_obroty / nieobc_estymator_wariancji_koszty
print(statystyka_F)

alpha <- 0.05

zbior_krytyczny <- qf((1- alpha), (nrow(data1)-1), (nrow(data1)-1))
print(zbior_krytyczny)

# ponieważ nasza wartośc statystyki testowej jest mniejsza niż lewa krawędź przedziału obszaru krytycznego
# nie ma podstaw do odrzucenia hipotezy

statystyka_T <- (srednia_obroty - srednia_koszty) / sqrt((nrow(data1)*nieobc_estymator_wariancji_obroty + nrow(data1) * nieobc_estymator_wariancji_koszty)/(nrow(data1) * 2 - 2)*(nrow(data1) * 2) / (nrow(data1)^2))
print(statystyka_T)

left_edge_T <- qt((1 - alpha), (2 * nrow(data1) - 2))
print(left_edge_T)

#wartośc statystyki należy do obszru krytycznego, zatem odrzucamy hipotezę zerową i przyjmujemy hipotezę alternatywną, że branża jest dochodowa

#========= ZADANIE 3 =========

alpha <- 0.02

#kwantyl rozkładu
q_t <- qt(1 - (alpha/2), nrow(data1) - 1)
print(q_t)

#przedział
left_edge_k <- (srednia_koszty - (q_t * (odchylenie_koszty / sqrt(nrow(data1) - 1))))
right_edge_k <- (srednia_koszty + (q_t * (odchylenie_koszty / sqrt(nrow(data1) - 1))))
print(left_edge_k)
print(right_edge_k)

#względna precyzja
blad_maksymalny_k <- (right_edge_k - left_edge_k) / 2
print(blad_maksymalny_k)

wzgledna_precyzja_k <- (blad_maksymalny_k / srednia_koszty * 100)
print(wzgledna_precyzja_k)

# Mamy podstawy do uogólnienia przedziału ufności


