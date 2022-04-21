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
for (xi in 1:25) {
  moment_centr_tmp <- moment_centr_tmp + (data1[xi, "koszty"] - mean(data1[['koszty']]))^3
}
moment_centr_koszty <- 1/24 * moment_centr_tmp

wspolczynnik_asymetrii_koszty <- (moment_centr_koszty) / (odchylenie_koszty ^ 3)
print(wspolczynnik_asymetrii_koszty)

moment_centr_4_tmp <- 0
for (xi in 1:25) {
  moment_centr_4_tmp <- moment_centr_4_tmp + (data1[xi, "koszty"] - mean(data1[['koszty']]))^4
}

moment_centr_4_koszty <- 1/24 * moment_centr_4_tmp

kurtoza_koszty <- moment_centr_4_koszty / (odchylenie_koszty ^ 4)
#                                                                 - 3
#                                                                 "w starszych pracach"
print(kurtoza_koszty)

