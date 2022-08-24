library(shiny)


shinyUI(fluidPage(
  titlePanel(""),
  inputPanel(
    selectInput('x', 'X', choices=c("Goli na domačo tekmo"="Zadetki_doma","Goli na gostujočo tekmo"="Zadetki_v_gosteh",
                                    "Kapaciteta stadiona"="Kapaciteta"),
                selected="Kapaciteta"),
    selectInput('y', 'Y', choices=c("Točke na domačo tekmo"="Tocke_doma","Točke na gostujočo tekmo"="Tocke_v_gosteh",
                                    "Povprečni položaj na lestvici"="Polozaj_na_lestvici"),
                selected="goalPerGame"),
    selectInput('z', 'Sezona', choices=unique(povprecje_ekip$Sezona),selected="18")
  ),
  mainPanel(plotOutput("outplot"))))
