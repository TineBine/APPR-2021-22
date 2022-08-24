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
  
  