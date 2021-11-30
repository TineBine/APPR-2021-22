# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza nogometnih tekem v TOP 5 ligah

V analizi si bom ogledal zadnje 3 sezone v 5 najmočnjejših evropskih nogometnih ligah. Pri tem se bom osredotočil enkrat na sezono ter primerjal med sabo dogajanje po ligah, drugič pa na ligo in pregledal vse tri sezone. Določil bom v kateri ligi se igra najbolj napadalno in v kateri najbolj defenzivno. Podrobneje si bom ogledal rezultate derbijev v vsaki ligi, (top 6 ekip). Zanima me tudi, če bo opazna sprememba zaradi korona premora ter igranja pred praznimi tribuami. 


Podatki za lige bodo v tabelah:
- kolo
- dan
- datum
- ura
- lestvica_domaci
- domaca_ekipa
- lestvica_gostje
- gostujoca_ekipa
- zadetki_domaci
- zadetki_gostje
- drzava
- sezona

Vir podatkov: https://www.transfermarkt.com/


## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
