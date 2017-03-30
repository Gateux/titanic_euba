# load libraries
if (!require("shiny")) install.packages("shiny")

# load final model
model_final <- readRDS("data/model_final.rds")

function(input, output, session) {
  # predict current account based on final model
  prediction <- reactive({
    # inputs from the screen ui
    input_vars <- data.frame(sex = input$sex,
                             pclass = input$pclass,
                             fare = input$fare)
    # prediction function: probability of NOT having a claim
    predict(model_final, input_vars, type = "response")
  })
  
  output$prob1<- renderText({
    paste(
      h4("Probability of having claim: "), 
      h3(
        round(1 - prediction(), 2) # main element, rounded probability of Having claim
        )
      )
  })
  
  output$price1<- renderText({
    paste(
      h4("Recomended price: "), 
      h3(
        paste0("$", round(1 - prediction(), 2) * input$sum_insured) # price as sum insured * probability of having claim
      )
    )
  })
  
}