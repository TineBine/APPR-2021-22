# 2. faza: Uvoz podatkov


#sl <- locale("sl", decimal_mark=",", grouping_mark=".")


# Začetni podatki
podatki <- read_csv('podatki/tekme.csv', locale=locale(encoding='UTF-8'))

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


imena_ekip <- c( "Spurs" = "Tottenham Hotspur", 
                 "Man Utd" = "Manchester United",
                 "West Ham"  = "West Ham United",
                 "Newcastle" = "Newcastle United",
                 "Bournemouth" = "AFC Bournemouth",
                 "Huddersfield" = "Huddersfield Town",
                 "Wolves" =  "Wolverhampton Wanderers", 
                 "Leicester" = "Leicester City",
                 "Man City" = "Manchester City",
                 "Brighton" = "Brighton & Hove Albion",
                 "Leeds" = "Leeds United",
                 "West Brom" = "West Bromwich Albion",
                 "Sheff Utd" = "Sheffield United",
                 "Norwich" = "Norwich City",
                 "Arsenal" = "Arsenal",
                 "Chelsea" = "Chelsea",
                 "Liverpool" = "Liverpool",
                 "Aston Villa" = "Aston Villa",
                 "Everton" = "Everton",
                 "Southampton" = "Southampton",
                 "Crystal Palace" = "Crystal Palace",
                 "Fulham" = "Fulham",
                 "Watford" = "Watford",
                 "Burnley" = "Burnley",
                 "Cardiff" = "Cardiff")


tabela_ang <- tabela_ang %>% mutate(domaca_ekipa=imena_ekip[domaca_ekipa])
tabela_ang <- tabela_ang %>% mutate(gostujoca_ekipa=imena_ekip[gostujoca_ekipa])

#Klubi, ki so igrali v Premier ligi
ang_ekipe <- unique(tabela_ang$domaca_ekipa)



#----------------------------------------------------------------------------
#Uvoz tabele iz wikipedije angleških stadionov


uvozi.stadione <- function() {
  link <- "https://en.wikipedia.org/wiki/List_of_football_stadiums_in_England"
  stran <- html_session(link) %>% read_html()
  tabela <- html_node(stran, ".wikitable")
  tabela <- html_table(tabela, fill= TRUE)
  tabela = tabela[2:5]
  colnames(tabela) <- c("Stadion", "Mesto", "Kapaciteta", "Ekipa")
  tabela = subset(tabela, (Ekipa %in% ang_ekipe))

  return(tabela)
}





stadioni <- uvozi.stadione()