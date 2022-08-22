# 3. faza: Vizualizacija podatkov


#--------------------------------------------------------------------------------------
# 1. GRAF - Goli na tekmo po ligah

okrajsave <- c("Francija" = "Fra",
            "Nemčija" = "Nem",
            "Italija" = "Ita",
            "Španija" = "Špa",
            "Združeno kraljestvo" = "ZK")

tabela_tekem$okrajsano <- okrajsave[tabela_tekem$drzava]
tabela_tekem$zdruzeno <- paste(tabela_tekem$okrajsano, tabela_tekem$sezona)

preimenovanje <- c("zadetki_domaci"="Zadeli domači", "zadetki_gostje"="Zadeli gosti")

povprecje_golov_lige <- tabela_tekem %>% select(drzava, zadetki_domaci, zadetki_gostje) %>%
  gather(key=Gostovanje, value="Zadetki", -drzava) %>%
  mutate(Gostovanje=preimenovanje[Gostovanje]) %>%
  group_by(drzava, Gostovanje) %>%
  summarise(Povprecje = mean(Zadetki, na.rm = TRUE))


graf1 <- ggplot(povprecje_golov_lige, aes(x=drzava, y=Povprecje, fill=Gostovanje)) + 
  geom_col() + 
  xlab("Država") + ylab("Povprečje zadetkov") + ggtitle("Primerjava zadetkov domače in gostujoče ekipe glede na državo")


print(graf1)

#--------------------------------------------------------------------------------------
#2. GRAF - Goli na tekmo po sezonah
povprecje_golov_sezone <- tabela_tekem %>% select(zdruzeno, zadetki_domaci, zadetki_gostje) %>%
  gather(key=Gostovanje, value="Zadetki", -zdruzeno) %>%
  mutate(Gostovanje=preimenovanje[Gostovanje]) %>%
  group_by(zdruzeno, Gostovanje) %>%
  summarise(Povprecje = mean(Zadetki, na.rm = TRUE))


graf2 <- ggplot(povprecje_golov_sezone, aes(x=zdruzeno, y=Povprecje, fill=Gostovanje)) + 
  geom_col() + 
  xlab("Sezona") + ylab("Povprečje zadetkov") + ggtitle("Primerjava zadetkov domače in gostujoče ekipe glede na sezono") +
  theme(axis.text.x = element_text(size = rel(0.9)))


print(graf2)



#--------------------------------------------------------------------------------------
#3. GRAF - Odstotek zmag domače in gostujoče ekipe na sezono
#zmage_domaci <- count(tabela_tekem, tabela_tekem$osvojene_tocke_domaci > 1)
#zmage_gostje <- count(tabela_tekem, tabela_tekem$osvojene_tocke_gostje > 1)
#test <- tabela_tekem %>% count(osvojene_tocke_domaci > 1)


preimenovanje2 <- c("osvojene_tocke_domaci" = "Zmagali domači", "osvojene_tocke_gostje" = "Zmagali gosti")
zmage <- tabela_tekem %>% select(sezona, osvojene_tocke_domaci, osvojene_tocke_gostje) %>%
  gather(key=Zmagovalec, value="Tocke", -sezona) %>%
  mutate(Zmagovalec=preimenovanje2[Zmagovalec]) %>%  
  group_by(sezona, Zmagovalec) %>%
  summarise(zmage = count(Tocke > 1))


st_tekem <- c(rep(nrow(tabela_18),2), rep(nrow(tabela_19),2), rep(nrow(tabela_20),2))

zmage$odstotek <- as.numeric(paste(zmage$zmage / st_tekem))


graf3 <- ggplot(zmage, aes(x=sezona, y=odstotek, fill=Zmagovalec)) + 
  geom_col(position = "dodge", orientation = "x") + scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0,0.5)) +
  xlab("Sezona") + ylab("Odstotek zmag") + ggtitle("Odstotki zmag domače in gostujoče ekipe")



print(graf3)
#--------------------------------------------------------------------------------------
#4. GRAF - Na koliko tekmah je prišlo do presenečenja
#presenecenje <- tabela_tekem %>% 
#  select(zdruzeno, kolo, domaca_ekipa, lestvica_domaci, gostujoca_ekipa, lestvica_gostje, osvojene_tocke_domaci, osvojene_tocke_gostje)
 
po_5_tekmah <- tabela_tekem[tabela_tekem$kolo > 5, ]
preimenovanje3 <- c("lestvica_domaci = Domači", "lestvica_gostje" = "Gosti")


presenecenje <- po_5_tekmah %>%
  select(zdruzeno, lestvica_domaci, lestvica_gostje, osvojene_tocke_domaci, osvojene_tocke_gostje) %>%
  mutate(domaci = (lestvica_domaci < lestvica_gostje) & (osvojene_tocke_domaci > osvojene_tocke_gostje), 
         gostje = (lestvica_domaci > lestvica_gostje) & (osvojene_tocke_domaci < osvojene_tocke_gostje)) %>%
  group_by(zdruzeno) %>%
  summarise(presenetili_domaci = count(domaci),
            presenetili_gostje = count(gostje))
  
#presenetili_domaci <- count(presenecenje, (presenecenje$lestvica_domaci < presenecenje$lestvica_gostje) & (presenecenje$osvojene_tocke_domaci > presenecenje$osvojene_tocke_gostje))
#presenetili_gostje <-  count(presenecenje, (presenecenje$lestvica_domaci > presenecenje$lestvica_gostje) & (presenecenje$osvojene_tocke_domaci < presenecenje$osvojene_tocke_gostje))

  #gather(key=Zmagovalec, value="Tocke", -zdruzeno) %>%
  #mutate(Zmagovalec=preimenovanje2[Zmagovalec]) %>%  
  #group_by(zdruzeno, Zmagovalec) 


graf4 <- ggplot(presenecenje, aes(x=zdruzeno, y=presenetili_domaci + presenetili_gostje, fill = presenetili_domaci)) + 
  geom_col() + 
  xlab("Sezona") + ylab("Povprečje zadetkov") + ggtitle("Primerjava zadetkov domače in gostujoče ekipe glede na sezono") +
  theme(axis.text.x = element_text(size = rel(0.9)))
  
  
print(graf4)

#--------------------------------------------------------------------------------------
#5. GRAF - Ekipe, ki so osvojile največ točk 

tocke <- tabela_tekem %>% select(sezona, osvojene_tocke_domaci, osvojene_tocke_gostje) %>%
  gather(key=Zmagovalec, value="Tocke", -sezona) %>%
  mutate(Zmagovalec=preimenovanje2[Zmagovalec]) %>%  
  group_by(sezona, Zmagovalec) %>%
  summarise(zmage = count(Tocke > 1), st_tekem = nrow(sezona))




zmage <- Sezone %>%
  transmute(Domaca_ekipa, Gostujoca_ekipa,
            Zmaga_domaci = Zadetki_domaca_ekipa > Zadetki_gostujoca_ekipa,
            Remi=Zadetki_domaca_ekipa == Zadetki_gostujoca_ekipa,
            Zmaga_gosti= Zadetki_domaca_ekipa < Zadetki_gostujoca_ekipa)

zmage.skupaj <- rbind(select(zmage, Ekipa=Domaca_ekipa, Zmaga=Zmaga_domaci, Remi, Poraz=Zmaga_gosti),
                      select(zmage, Ekipa=Gostujoca_ekipa, Zmaga=Zmaga_gosti, Remi, Poraz=Zmaga_domaci)) %>%
  group_by(Ekipa) %>% summarise(Zmage=sum(Zmaga), Remiji=sum(Remi), Porazi=sum(Poraz)) %>%
  mutate(Tocke = 3*Zmage + Remiji)



#--------------------------------------------------------------------------------------
#6. GRAF - Kakšen delež točk so ekipe osvojile doma (Ekipe v PL)




#--------------------------------------------------------------------------------------
#7. GRAF - Osvojene točke doma glede na kapaciteto stadiona (Ekipe v PL)




#--------------------------------------------------------------------------------------
#8. GRAF - izidi na tekmah TOP 6 ekip





#======================================================================================
#ZEMLJEVIDI

#1. ZEMLJEVID - Najbolj odprte tekme v Evropi




#--------------------------------------------------------------------------------------
#2. ZEMLJEVID - Najtežja gostovanja v PL