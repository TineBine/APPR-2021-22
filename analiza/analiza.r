# 4. faza: Napredna analiza podatkov

set.seed(2022)
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

#Vsi podatki
po_kolih_domaci <- tabela_tekem %>% select(sezona, kolo, domaca_ekipa, lestvica_domaci, zadetki_domaci,
                                           lestvica_gostje, osvojene_tocke_domaci) %>%
  group_by(domaca_ekipa) %>% arrange(domaca_ekipa) %>% rename(Ekipa = domaca_ekipa,
                                                              Lestvica = lestvica_domaci,
                                                              Goli = zadetki_domaci,
                                                              Lestvica_nasprotnik = lestvica_gostje,
                                                              Osvojene_tocke = osvojene_tocke_domaci)


po_kolih_gostje <- tabela_tekem %>% select(sezona, kolo, gostujoca_ekipa, lestvica_gostje, zadetki_gostje,
                                           lestvica_domaci, osvojene_tocke_gostje) %>%
  group_by(gostujoca_ekipa) %>% arrange(gostujoca_ekipa) %>% rename(Ekipa = gostujoca_ekipa,
                                                              Lestvica = lestvica_gostje,
                                                              Goli = zadetki_gostje,
                                                              Lestvica_nasprotnik = lestvica_domaci,
                                                              Osvojene_tocke = osvojene_tocke_gostje)

po_kolih_ekipe <- po_kolih_domaci %>% 
  full_join(po_kolih_gostje, by = c("sezona", "kolo", "Ekipa", "Lestvica", "Goli",
                                    "Lestvica_nasprotnik", "Osvojene_tocke")) %>% arrange(Ekipa, sezona, kolo)

po_kolih_forma <- po_kolih_ekipe %>% group_by(Ekipa, sezona) %>%
  mutate(Forma = lag(rollsumr(Osvojene_tocke, k = 5, fill = NA)), .before = "Lestvica_nasprotnik") %>%
  mutate(Strelska_forma = lag(rollsumr(Goli, k = 5, fill = NA)), .before = "Lestvica_nasprotnik") %>%
  select(-Goli)

#=========================================================================================================
#STROJNO UČENJE

#Podatki
podatki_ucenje <- po_kolih_forma %>% drop_na %>% rename(x1 = sezona,
                                                        x2 = kolo,
                                                        x3 = Lestvica,
                                                        x4 = Forma,
                                                        x5 = Strelska_forma,
                                                        x6 = Lestvica_nasprotnik,
                                                        yn = Osvojene_tocke)  %>% group_by(x2) %>% 
  select(-Ekipa) %>% add_column(yd = 1)

podatki_ucenje$yd[podatki_ucenje$yn != 3] = -1
podatki_ucenje$yd <- podatki_ucenje$yd %>% as.factor()
#-----------------------------------------------------------------------------------------------
#Funkcije, sposojene od profesorja

#FUNCIJE ZA UČENJE, NAPOVEDI IN NAPAKE
ucenje = function(podatki, formula, algoritem) {
  switch(
    algoritem,
    lin.reg = lm(formula, data = podatki),
    log.reg = glm(formula, data = podatki, family = "binomial"),
    ng = ranger(formula, data = podatki)
  )
}

napovedi = function(podatki, model, algoritem) {
  switch(
    algoritem,
    lin.reg = predict(model, podatki),
    log.reg = ifelse(
      predict(model, podatki, type = "response") >= 0.5,
      1, -1
    ),
    ng = predict(model, podatki)$predictions
  )
}

napaka_regresije = function(podatki, model, algoritem) {
  podatki %>%
    bind_cols(yn.hat = napovedi(podatki, model, algoritem)) %>%
    mutate(
      izguba = (yn - yn.hat) ^ 2
    ) %>%
    select(izguba) %>%
    unlist() %>%
    mean()
}

napaka_razvrscanja = function(podatki, model, algoritem) {
  podatki %>%
    bind_cols(yd.hat = napovedi(podatki, model, algoritem)) %>%
    mutate(
      izguba = (yd != yd.hat)
    ) %>%
    select(izguba) %>%
    unlist() %>%
    mean()
}



#FUNKCIJA ZA RAZBITJE VEKTORJA NA k ENAKIH INTERVALOV
razbitje = function(x, k) {
  
  # Razreži indekse vektorja na k intervalov
  razrez = cut(seq_along(x), k, labels = FALSE)
  
  # Razbij vektor na k seznamov
  # na osnovi razreza intervalov
  split(x, razrez)
}


#FUNKCIJA ZA RAZBITJE UČNE MNOŽICE NA k DISJUNKTNIH PODMNOŽIC
pp.razbitje = function(n, k, stratifikacija = NULL, seme = NULL) {
  
  # najprej nastavimo seme za naključna števila, če je podano
  if (!is.null(seme)) {
    set.seed(seme)
  }
  
  # če ne opravljamo stratifikacije, potem vrnemo navadno razbitje
  # funkcijo sample uporabimo zato, da naključno premešamo primere
  if (is.null(stratifikacija)) {
    return(razbitje(sample(1:n), k))
  }
  
  # če pa opravljamo stratifikacijo, razbitje izvedemo za vsako
  # vrednost spremenljive stratifikacija posebej in nato
  # podmnožice združimo v skupno razbitje
  r = NULL
  for (v in levels(stratifikacija)) {
    
    # Če smo pri prvi vrednosti vzpostavimo razbitje
    if (is.null(r)) {
      # opravimo razbitje samo za primere z vrednostjo v
      r = razbitje(sample(which(stratifikacija == v)), k)
    } else {
      # opravimo razbitje za vrednost v
      # in podmnožice združimo s trenutnim razbitjem
      r.v = razbitje(sample(which(stratifikacija == v)), k)
      for (i in 1:k) {
        r[[i]] = c(r[[i]], r.v[[i]])
      }
    }
  }
  r
}





#FUNKCIJA ZA PREČNO PREVERJANJE
precno.preverjanje = function(podatki, razbitje, formula, algoritem, razvrscanje) {
  
  # pripravimo vektor za napovedi
  if (razvrscanje) {
    pp.napovedi = factor(rep(1, nrow(podatki)), levels = c(-1,1))
  } else {
    pp.napovedi = rep(0, nrow(podatki))
  }
  
  # gremo čez vse podmnožice Si razbitja S
  for (i in 1:length(razbitje)) {
    # naučimo se modela na množici S \ Si
    model = podatki[ -razbitje[[i]], ] %>% ucenje(formula, algoritem)
    
    # naučen model uporabimo za napovedi na Si
    pp.napovedi[ razbitje[[i]] ] = podatki[ razbitje[[i]], ] %>% napovedi(model, algoritem)
  }
  
  if (razvrscanje) {
    mean(pp.napovedi != podatki$yd)
  } else {
    mean((pp.napovedi - podatki$yn) ^ 2)
  }
}


#----------------------------------------------------------------------------------------------
#modeli
pp.ucni <- pp.razbitje(nrow(podatki_ucenje), k=10, stratifikacija = podatki_ucenje$yd)
model1_klas <- precno.preverjanje(podatki_ucenje, pp.ucni, yd ~ x1 + x2 + x3 + x4 + x5 + x6, "ng", TRUE)
model2_klas <- precno.preverjanje(podatki_ucenje, pp.ucni, yd ~ x3 + x4 + x5 + x6, "ng", TRUE)
model3_klas <- precno.preverjanje(podatki_ucenje, pp.ucni, yd ~ x2 + x3 + x4 + x5 + x6, "ng", TRUE)
model4_klas <- precno.preverjanje(podatki_ucenje, pp.ucni, yd ~ x1 + x2 + x3 + x4 + x5 + x6, "log.reg", TRUE)
model5_klas <- precno.preverjanje(podatki_ucenje, pp.ucni, yd ~ x3 + x4 + x5 + x6, "log.reg", TRUE)

model1_reg <- precno.preverjanje(podatki_ucenje, pp.ucni, yn ~ x1 + x2 + x3 + x4 + x5 + x6, "ng", FALSE)
model2_reg <- precno.preverjanje(podatki_ucenje, pp.ucni, yn ~ x3 + x4 + x5 + x6, "ng", FALSE)
model3_reg <- precno.preverjanje(podatki_ucenje, pp.ucni, yn ~ x2 + x3 + x4 + x5 + x6, "ng", FALSE)
model4_reg <- precno.preverjanje(podatki_ucenje, pp.ucni, yn ~ x1 + x2 + x3 + x4 + x5 + x6, "lin.reg", FALSE)
model5_reg <- precno.preverjanje(podatki_ucenje, pp.ucni, yn ~ x3 + x4 + x5 + x6, "lin.reg", FALSE)

napake <- tibble(
  model = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
  tip = c(rep("klas", 5), rep("reg", 5)),
  napaka = c(model1_klas, model2_klas, model3_klas, model4_klas, model5_klas,
              model1_reg, model2_reg, model3_reg, model4_reg, model5_reg)
)

graf_napak <- ggplot(napake, aes(x=model, y=napaka, color=tip)) + geom_point() + 
  geom_line(size = 1) + scale_x_continuous(breaks = 1:5) +
  scale_y_continuous()

#print(graf_napak)
