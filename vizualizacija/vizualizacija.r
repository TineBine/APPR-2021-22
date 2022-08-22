# 3. faza: Vizualizacija podatkov


#--------------------------------------------------------------------------------------
# 1. GRAF - Goli na tekmo po sezonah
#goli_na_tekmo_sezone <- tabela_tekem %>% group_by(tabela_tekem$sezone) %>% summarise((sum(tabela_tekem$zadetki_domaci) + sum(tabela_tekem$zadetki_gostje))/nrow(tabela_tekem))

preimenovanje <- c("zadetki_domaci"="Domači", "zadetki_gostje"="Gosti")
povprecje_golov <- tabela_tekem %>% select(sezona, zadetki_domaci, zadetki_gostje) %>%
  gather(key=Gostovanje, value="Zadetki", -sezona) %>%
  mutate(Gostovanje=preimenovanje[Gostovanje]) %>%
  group_by(sezona, Gostovanje) %>%
  summarise(Povprecje = mean(Zadetki, na.rm = TRUE))


graf1 <- ggplot(povprecje_golov, aes(x=sezona, y=Povprecje, fill=Gostovanje)) + 
  geom_col() + 
  xlab("Sezona") + ylab("Povprečje zadetkov") + ggtitle("Primerjava zadetkov domače in gostujoče ekipe")


print(graf1)
#--------------------------------------------------------------------------------------
#2. GRAF - Goli na tekmo po ligah
povprecje_golov2 <- tabela_tekem %>% select(drzava, zadetki_domaci, zadetki_gostje) %>%
  gather(key=Gostovanje, value="Zadetki", -drzava) %>%
  mutate(Gostovanje=preimenovanje[Gostovanje]) %>%
  group_by(drzava, Gostovanje) %>%
  summarise(Povprecje = mean(Zadetki, na.rm = TRUE))


graf2 <- ggplot(povprecje_golov2, aes(x=drzava, y=Povprecje, fill=Gostovanje)) + 
  geom_col() + 
  xlab("Država") + ylab("Povprečje zadetkov") + ggtitle("Primerjava zadetkov domače in gostujoče ekipe")


print(graf2)



#--------------------------------------------------------------------------------------
#3. GRAF - Odstotek zmag domače ekipe na sezono




#--------------------------------------------------------------------------------------
#4. GRAF - Kakšen delež osvojenih točk je bilo pridobljenih doma




#--------------------------------------------------------------------------------------
#5. GRAF - Na koliko tekmah je prišlo do presenečenja




#--------------------------------------------------------------------------------------
#6. GRAF - Osvojene točke doma glede na kapaciteto stadiona (Ekipe v PL)




#--------------------------------------------------------------------------------------
#7. GRAF - izidi na tekmah TOP 6 ekip





#======================================================================================
#ZEMLJEVIDI

#1. ZEMLJEVID - Najbolj odprte tekme v Evropi




#--------------------------------------------------------------------------------------
#2. ZEMLJEVID - Najtežja gostovanja v PL