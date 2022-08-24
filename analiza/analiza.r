# 4. faza: Napredna analiza podatkov

#Tabela za Shiny
povp_domaci <- tabela_ang %>% select(sezona, domaca_ekipa, osvojene_tocke_domaci, zadetki_domaci, lestvica_domaci) %>%
  group_by(sezona, domaca_ekipa) %>% summarise(mean(osvojene_tocke_domaci), mean(zadetki_domaci), mean(lestvica_domaci)) %>%
  rename(Ekipa = domaca_ekipa)

povp_gostje <- tabela_ang %>% select(sezona, gostujoca_ekipa, osvojene_tocke_gostje, zadetki_gostje, lestvica_gostje) %>%
  group_by(sezona, gostujoca_ekipa) %>% summarise(mean(osvojene_tocke_gostje), mean(zadetki_gostje), mean(lestvica_gostje)) %>%
  rename(Ekipa = gostujoca_ekipa)


stadioni2 <- stadioni %>% select(Ekipa, Kapaciteta) %>% arrange(Ekipa)

povprecje_ekip <- povp_domaci %>% right_join(povp_gostje, by = c("sezona", "Ekipa")) %>% full_join(stadioni2, by = "Ekipa")

colnames(povprecje_ekip) <- c("Sezona", "Ekipa", "Tocke_doma", "Zadetki_doma", "Lestvica_doma", "Tocke_v_gosteh", 
                              "Zadetki_v_gosteh", "Lestvica_v_gosteh", "Kapaciteta")


povprecje_ekip <- povprecje_ekip %>% mutate(Polozaj_na_lestvici = (Lestvica_doma + Lestvica_v_gosteh)/2, .before = "Kapaciteta") %>%
  select(-Lestvica_doma, -Lestvica_v_gosteh)
  
#========================================================================================================================
#Podatki za napovedovanje pričakovanih točk

#Tabela Sezone 18
po_kolih_domaci_18 <- tabela_18 %>% select(kolo, domaca_ekipa, lestvica_gostje, osvojene_tocke_domaci) %>% 
  group_by(domaca_ekipa) %>% arrange(domaca_ekipa) %>% rename(Ekipa = domaca_ekipa,
                                                              Lestvica_nasprotnik = lestvica_gostje,
                                                              Osvojene_tocke = osvojene_tocke_domaci)

po_kolih_gostje_18 <- tabela_18 %>% select(kolo, gostujoca_ekipa, lestvica_domaci, osvojene_tocke_gostje) %>% 
  group_by(gostujoca_ekipa) %>% arrange(gostujoca_ekipa) %>% rename(Ekipa = gostujoca_ekipa,
                                                                    Lestvica_nasprotnik = lestvica_domaci,
                                                                    Osvojene_tocke = osvojene_tocke_gostje)

po_kolih_ekipe_18 <- po_kolih_domaci_18 %>% 
  full_join(po_kolih_gostje_18, by = c("kolo", "Ekipa", "Lestvica_nasprotnik", "Osvojene_tocke")) %>% arrange(Ekipa, kolo)

po_kolih_forma_18 <- po_kolih_ekipe_18 %>% select_all %>% group_by(Ekipa) %>%
  mutate(Forma = lag(rollsumr(Osvojene_tocke, k = 5, fill = NA)))

#------------------------------------------------------------------------------------------------------
#Tabela Sezone 19
po_kolih_domaci_19 <- tabela_19 %>% select(kolo, domaca_ekipa, lestvica_gostje, osvojene_tocke_domaci) %>% 
  group_by(domaca_ekipa) %>% arrange(domaca_ekipa) %>% rename(Ekipa = domaca_ekipa,
                                                              Lestvica_nasprotnik = lestvica_gostje,
                                                              Osvojene_tocke = osvojene_tocke_domaci)

po_kolih_gostje_19 <- tabela_19 %>% select(kolo, gostujoca_ekipa, lestvica_domaci, osvojene_tocke_gostje) %>% 
  group_by(gostujoca_ekipa) %>% arrange(gostujoca_ekipa) %>% rename(Ekipa = gostujoca_ekipa,
                                                                    Lestvica_nasprotnik = lestvica_domaci,
                                                                    Osvojene_tocke = osvojene_tocke_gostje)

po_kolih_ekipe_19 <- po_kolih_domaci_19 %>% 
  full_join(po_kolih_gostje_19, by = c("kolo", "Ekipa", "Lestvica_nasprotnik", "Osvojene_tocke")) %>% arrange(Ekipa, kolo)

po_kolih_forma_19 <- po_kolih_ekipe_19 %>% select_all %>% group_by(Ekipa) %>%
  mutate(Forma = lag(rollsumr(Osvojene_tocke, k = 5, fill = NA)))

#--------------------------------------------------------------------------------------------------------
#Tabela Sezone 20
po_kolih_domaci_20 <- tabela_20 %>% select(kolo, domaca_ekipa, lestvica_gostje, osvojene_tocke_domaci) %>% 
  group_by(domaca_ekipa) %>% arrange(domaca_ekipa) %>% rename(Ekipa = domaca_ekipa,
                                                              Lestvica_nasprotnik = lestvica_gostje,
                                                              Osvojene_tocke = osvojene_tocke_domaci)

po_kolih_gostje_20 <- tabela_20 %>% select(kolo, gostujoca_ekipa, lestvica_domaci, osvojene_tocke_gostje) %>% 
  group_by(gostujoca_ekipa) %>% arrange(gostujoca_ekipa) %>% rename(Ekipa = gostujoca_ekipa,
                                                                    Lestvica_nasprotnik = lestvica_domaci,
                                                                    Osvojene_tocke = osvojene_tocke_gostje)

po_kolih_ekipe_20 <- po_kolih_domaci_20 %>% 
  full_join(po_kolih_gostje_20, by = c("kolo", "Ekipa", "Lestvica_nasprotnik", "Osvojene_tocke")) %>% arrange(Ekipa, kolo)

po_kolih_forma_20 <- po_kolih_ekipe_20 %>% select_all %>% group_by(Ekipa) %>%
  mutate(Forma = lag(rollsumr(Osvojene_tocke, k = 5, fill = NA)))

