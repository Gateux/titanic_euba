pageWithSidebar(
  headerPanel('Titanic s. r. o - Pricing Tool'),
  sidebarPanel(
    sliderInput('sum_insured', label = 'Sum Insured', min = 1, max = 100, step = 1, value = 15),
    selectInput('sex', 'Sex', c("male", "female")),
    sliderInput('age', label = 'Age', min = 1, max = 80, step = 1, value = 30),
    sliderInput('fare', label = 'Fare', min = 1, max = 500, step = 1, value = 15),
    numericInput('pclass', 'Socio-Economic Class', 3, min = 1, max = 3)
  ),
  mainPanel(
    htmlOutput('prob1'),
    htmlOutput('price1')
  )
)