# 2. faza: Uvoz podatkov


#sl <- locale("sl", decimal_mark=",", grouping_mark=".")


# Začetni podatki
podatki <- read_csv('podatki/tekme.csv', locale=locale(encoding='Windows-1250'))

tabela_tekem1 <- podatki[-c(2,3,4)]

slovar <- c("France" = "Francija",
            "Germany" = "Nemčija",
            "Italy" = "Italija",
            "Spain" = "Španija",
            "United Kingdom" = "Združeno kraljestvo")

tabela_tekem <- tabela_tekem1 %>% mutate(drzava=slovar[drzava])


#Dodani podatki o osvojenih točkah
tabela_tekem <- tabela_tekem %>% add_column(osvojene_tocke_domaci = 1,
                                            osvojene_tocke_gostje = 1,
                                            .after = "zadetki_gostje")

for (i in 1:nrow(tabela_tekem)) {
  if (tabela_tekem$zadetki_domaci[i] > tabela_tekem$zadetki_gostje[i]) {
    tabela_tekem$osvojene_tocke_domaci[i] = 3
    tabela_tekem$osvojene_tocke_gostje[i] = 0
  }
}

for (i in 1:nrow(tabela_tekem)) {
  if (tabela_tekem$zadetki_domaci[i] < tabela_tekem$zadetki_gostje[i]) {
    tabela_tekem$osvojene_tocke_domaci[i] = 0
    tabela_tekem$osvojene_tocke_gostje[i] = 3
  }
}



# Tabele za posamezno ligo
tabela_fra <- subset(tabela_tekem, drzava == "Francija")
tabela_nem <- subset(tabela_tekem, drzava == "Nemčija")
tabela_ita <- subset(tabela_tekem, drzava == "Italija")
tabela_spa <- subset(tabela_tekem, drzava == "Španija")
tabela_ang <- subset(tabela_tekem, drzava == "Združeno kraljestvo")


#Tabele za posamezno sezono
tabela_18 <- subset(tabela_tekem, sezona == '18')
tabela_19 <- subset(tabela_tekem, sezona == '19')
tabela_20 <- subset(tabela_tekem, sezona == '20')


