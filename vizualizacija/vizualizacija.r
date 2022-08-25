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
  xlab("Država") + ylab("Povprečni goli na tekmo") + ggtitle("Primerjava golov domače in gostujoče ekipe glede na državo")


#print(graf1)

#--------------------------------------------------------------------------------------
#2. GRAF - Goli na tekmo po sezonah
povprecje_golov_sezone <- tabela_tekem %>% select(zdruzeno, zadetki_domaci, zadetki_gostje) %>%
  gather(key=Gostovanje, value="Zadetki", -zdruzeno) %>%
  mutate(Gostovanje=preimenovanje[Gostovanje]) %>%
  group_by(zdruzeno, Gostovanje) %>%
  summarise(Povprecje = mean(Zadetki, na.rm = TRUE))


graf2 <- ggplot(povprecje_golov_sezone, aes(x=zdruzeno, y=Povprecje, fill=Gostovanje)) + 
  geom_col() + 
  xlab("Sezona") + ylab("Povprečni goli na tekmo") + ggtitle("Primerjava golov domače in gostujoče ekipe glede na sezono") +
  theme(axis.text.x = element_text(angle = 45, size = rel(0.9)))


#print(graf2)



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



#print(graf3)
#--------------------------------------------------------------------------------------
#4. GRAF - Na koliko tekmah je prišlo do presenečenja
po_5_tekmah <- tabela_tekem[tabela_tekem$kolo > 5, ]

presenecenje <- po_5_tekmah %>%
  select(zdruzeno, lestvica_domaci, lestvica_gostje, osvojene_tocke_domaci, osvojene_tocke_gostje) %>%
  mutate(domaci = (lestvica_domaci < lestvica_gostje) & (osvojene_tocke_domaci > osvojene_tocke_gostje), 
         gostje = (lestvica_domaci > lestvica_gostje) & (osvojene_tocke_domaci < osvojene_tocke_gostje)) %>%
  group_by(zdruzeno) %>%
  summarise(presenetili_domaci = count(domaci),
            presenetili_gostje = count(gostje))


graf4 <- ggplot(presenecenje, aes(x=zdruzeno, y=presenetili_domaci + presenetili_gostje, fill = presenetili_domaci)) + 
  geom_col() + 
  xlab("Sezona") + ylab("Število presenečenj") + ggtitle("Število presenečenj na tekmah glede na sezono") +
  theme(axis.text.x = element_text(angle = 45 , size = rel(0.9))) + labs(fill = "Presenečenja domačinov")
  
  
#print(graf4)

#--------------------------------------------------------------------------------------
#5. GRAF - Ekipe, ki so osvojile največ točk 
razpleti_tekem <- tabela_tekem %>%
  transmute(domaca_ekipa, gostujoca_ekipa,
            Zmaga_domaci = zadetki_domaci > zadetki_gostje,
            Remi=zadetki_domaci == zadetki_gostje,
            Zmaga_gosti= zadetki_domaci < zadetki_gostje)

skupne_tocke <- rbind(select(razpleti_tekem, Ekipa=domaca_ekipa, Zmaga=Zmaga_domaci, Remi, Poraz=Zmaga_gosti),
                      select(razpleti_tekem, Ekipa=gostujoca_ekipa, Zmaga=Zmaga_gosti, Remi, Poraz=Zmaga_domaci)) %>%
  group_by(Ekipa) %>% summarise(Zmage=sum(Zmaga), Remiji=sum(Remi), Porazi=sum(Poraz)) %>%
  mutate(Tocke = 3*Zmage + Remiji) 

top_20_1 <- skupne_tocke[order(skupne_tocke$Tocke, decreasing = TRUE), ]
top_20 <- top_20_1[1:20, ]

graf5 <- ggplot(top_20, aes(x=reorder(Ekipa, Tocke), y=Tocke, fill=Ekipa)) + 
            geom_col() + coord_flip() + xlab("Ekipa") + ylab("Točke") + 
  scale_color_gradientn(colours = rainbow(20))


#print(graf5)

#--------------------------------------------------------------------------------------
#6. GRAF - Kakšen delež točk so ekipe osvojile doma (Ekipe v PL)
ang_tekme_doma <- tabela_ang %>% select(domaca_ekipa, osvojene_tocke_domaci) %>%
  group_by(domaca_ekipa) %>% summarise(sum(osvojene_tocke_domaci))
colnames(ang_tekme_doma) <- c("Ekipa", "Tocke_doma")

ang_tekme_gosti <- tabela_ang %>% select(gostujoca_ekipa, osvojene_tocke_gostje) %>%
  group_by(gostujoca_ekipa) %>% summarise(sum(osvojene_tocke_gostje))
colnames(ang_tekme_gosti) <- c("Ekipa", "Tocke_v_gosteh")


ang_tekme <- merge(ang_tekme_doma, ang_tekme_gosti, by = 'Ekipa')
ang_tekme$Razmerje_tock <- as.numeric(paste(ang_tekme$Tocke_doma /(ang_tekme$Tocke_doma + ang_tekme$Tocke_v_gosteh)))
#ang_tekme1 <- ang_tekme[order(ang_tekme$Razmerje_tock, decreasing = TRUE), ]


graf6 <- ggplot(ang_tekme, aes(x=Tocke_doma, y=Tocke_v_gosteh, label = Ekipa)) + 
  geom_point(color = dplyr::case_when(ang_tekme$Tocke_doma > 100 ~ "green2",
                                      ang_tekme$Tocke_doma < 25 ~ "red",
                                      ang_tekme$Tocke_doma + ang_tekme$Tocke_v_gosteh < 100 ~ "orange",
                                      TRUE ~ "yellow2"), size = 2) +
  xlab("Točke doma") + ylab("Točke v gosteh") + 
  geom_label_repel(aes(label=Ekipa,),
                   size = 2.5,
                   box.padding = 0.25,
                   point.padding = 0.35) 




#print(graf6)
#--------------------------------------------------------------------------------------
#7. GRAF - Osvojene točke doma glede na kapaciteto stadiona (Ekipe v PL)
tocke_kapaciteta <- merge(ang_tekme, stadioni, by = 'Ekipa')

graf7 <- ggplot(tocke_kapaciteta, aes(x=Kapaciteta, y=Tocke_doma, label = Stadion)) + 
  geom_point(color = dplyr::case_when(ang_tekme$Tocke_doma > 100 ~ "green2",
                                      ang_tekme$Tocke_doma < 25 ~ "red",
                                      ang_tekme$Tocke_doma + ang_tekme$Tocke_v_gosteh < 100 ~ "orange",
                                      TRUE ~ "yellow2"), size = 2) +
  geom_smooth(method = lm, se = FALSE) +
  xlab("Kapaciteta") + ylab("Točke doma") + 
  geom_label_repel(aes(label=Stadion,),
                   size = 2.5,
                   box.padding = 0.25,
                   point.padding = 0.35) 




#print(graf7)


#--------------------------------------------------------------------------------------
#8. GRAF - izidi na tekmah TOP 6 ekip
razpleti_tekem_top6a <- po_5_tekmah %>%
  select(drzava, lestvica_domaci, lestvica_gostje, osvojene_tocke_domaci, osvojene_tocke_gostje) %>%
  mutate(top_ekipe = ((lestvica_domaci < 7) & (lestvica_gostje < 7)), 
         zmaga_domacih = osvojene_tocke_domaci > osvojene_tocke_gostje,
         zmaga_gostov = osvojene_tocke_domaci < osvojene_tocke_gostje,
         remi = osvojene_tocke_domaci == osvojene_tocke_gostje
         ) %>%
  group_by(drzava)

razpleti_tekem_top6 <- razpleti_tekem_top6a[razpleti_tekem_top6a$top_ekipe == TRUE, ]

tekme_top6 <- razpleti_tekem_top6 %>% select_all() %>% group_by(drzava) %>%
  summarise(zmage_domacih = count(zmaga_domacih, top_ekipe),
            zmage_gostov = count(zmaga_gostov),
            remiji = count(remi))

za_graf <- tekme_top6 %>% gather(key = Rezultat, value = Value, zmage_domacih:remiji)
tekme_urejeno <- za_graf[order(za_graf$drzava), ]


graf8 <- ggplot(tekme_urejeno, aes(drzava, Value, fill = Rezultat)) + geom_col(position = "dodge") +
  labs(x = "Država", y = "Število rezultatov", title = "Rezultati med TOP 6 ekipami glede na ligo")



#print(graf8)
#======================================================================================
#ZEMLJEVIDI
#1. ZEMLJEVID - Najbolj odprte tekme v Evropi
zemljevid <-  uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                              "ne_50m_admin_0_countries", encoding = "UTF-8")
zemljevid1 <- zemljevid[zemljevid$CONTINENT == "Europe",]

slovar_inverz <- slovar <- c("Francija" = "France",
                             "Nemčija" = "Germany",
                             "Italija" = "Italy",
                             "Španija" = "Spain",
                             "Združeno kraljestvo" = "United Kingdom")

drzave_goli_na_tekmo <- povprecje_golov_lige %>% mutate(drzava=slovar_inverz[drzava]) %>% 
  select(drzava, Povprecje) %>% group_by(drzava) %>% 
  summarise(goli_na_tekmo = sum(Povprecje))

zem_goli <- merge(zemljevid1, drzave_goli_na_tekmo, by.x="SOVEREIGNT", by.y="drzava")


zemljevid_EUR <- tm_shape(merge(zemljevid1, drzave_goli_na_tekmo, by.x="NAME", by.y="drzava"),
                          xlim=c(-15, 38), ylim=c(30, 75)) + tm_polygons("goli_na_tekmo", title = "Goli na tekmo") + tm_legend(show=TRUE)

#--------------------------------------------------------------------------------------
#2. ZEMLJEVID - Najtežja gostovanja v PL
UK <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_GBR_shp.zip", "gadm36_GBR_2",
                      encoding="UTF-8")

klubi <- c("Manchester United"="Manchester",
           "Manchester City" = "Salford",
           "AFC Bournemouth" = "Bournemouth",
           "Leicester City" = "Leicester",
           "Southampton" = "Southampton",
           "Cardiff City" = "Cardiff",
           "Brighton & Hove Albion" = "Brighton and Hove",
           "Huddersfield Town" = "Calderdale",
           "Everton" = "Sefton",
           "Aston Villa" = "Birmingham",
           "Newcastle United" = "Newcastle upon Tyne",
           "Wolverhampton Wanderers" = "Wolverhampton", 
           "West Bromwich Albion" = "Dudley",
           "Liverpool" = "Saint Helens",
           "Watford" = "Luton",
           "Burnley" = "Bury",
           "Norwich City" = "Norfolk",
           "Arsenal" = "Greater London",
           "West Ham United" = "Greater London",
           "Fulham" = "Greater London",
           "Chelsea"= "Greater London",
           "Crystal Palace" = "Greater London",
           "Tottenham Hotspur" = "Greater London",
           "Leeds United" = "Leeds",
           "Sheffield United" = "Sheffield"
)


klubi_regija <- data.frame(Ekipa=names(klubi),
                           Regija=parse_factor(klubi, levels(UK$NAME_2)),
                           stringsAsFactors=FALSE)
AngWal <- UK[UK$NAME_1 %in% c("England", "Wales"),]


ang_tocke_gostov <- tabela_ang %>% select(domaca_ekipa, osvojene_tocke_gostje) %>%
  group_by(domaca_ekipa) %>% summarise(mean(osvojene_tocke_gostje))
colnames(ang_tocke_gostov) <- c("Ekipa", "Tocke_gostov")


tocke_regije <- ang_tocke_gostov %>% right_join(klubi_regija, by = "Ekipa") %>%
  select(Tocke_gostov, Regija) %>% group_by(Regija) %>% summarise(Tocke= mean(Tocke_gostov))

zem_tocke <- merge(AngWal, tocke_regije, by.x="NAME_2", by.y="Regija")

zemljevid_PL <- tm_shape(zem_tocke) + tm_polygons("Tocke", title = "Osvojene točke gostov") + tm_legend(show=TRUE)


