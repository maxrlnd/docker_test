output$weanVal <- renderText({
  req(input$quizSub)
  isolate(
    validate(
      need(input$weanQ, genericWrong) %then%
        need(all(input$weanQ == c("grow", "wean", "land")), genericWrong)
    )
  )
})

output$ranchVal <- renderText({
  req(input$quizSub)
  isolate(
    validate(
      need(input$ranchSizeQ == 3000, "This is the incorrect size try again")
    )
  )
})

output$cullVal <- renderText({
  req(input$quizSub)
  isolate(
    validate(
      need(input$cullQ == "To sell a cow", genericWrong)
    )
  )
})

output$herdSizeVal <- renderText({
  req(input$quizSub)
  isolate(
    validate(
      need(input$herdSizeQ == 600, genericWrong)
    )
  )
})

output$lHerdVal <- renderText(({
  req(input$quizSub)
  isolate(
    validate(
      need(input$lHerdQ == 600, genericWrong)
    )
  )
}))

output$bigHerdVal <- renderText({
  req(input$quizSub)
  isolate(
    validate(
      need(input$bigHerdQ, genericWrong) %then%
        need(all(input$bigHerdQ == c("moreCalves", "damage")), genericWrong)
    )
  )
})

output$priceVal <- renderText({
  req(input$quizSub)
  isolate(
    validate(
      need(input$priceQ == "same", genericWrong)
    )
  )
})

output$adaptVal <- renderText({
  req(input$quizSub)
  isolate(
    validate(
      need(input$adaptQ, genericWrong) %then%
        need(all(input$adaptQ == c("underweight", "fewerBirths", "fewerCalves")), genericWrong)
    )
  )
})

output$earningsVal <- renderText({
  req(input$quizSub)
  isolate(
    validate(
      need(input$earningsQ == "$3", genericWrong)
    )
  )
})

output$practiceVal <- renderText({
  req(input$quizSub)
  isolate(
    validate(
      need(input$practiceQ == "True", genericWrong)
    )
  )
})

output$bonusVal <- renderText({
  req(input$quizSub)
  isolate(
    validate(
      need(input$bonusQ == "No", genericWrong)
    )
  )
})

output$premiumVal <- renderText({
  req(input$quizSub)
  isolate(
    validate(
      need(input$premiumQ == "something reasonable", genericWrong)
    )
  )
})

output$rainmonthsVal <- renderText({
  req(input$quizSub)
  isolate(
    validate(
      need(input$rainmonthsQ == "May-June, July-August", genericWrong)
    )
  )
})

output$payoutVal <- renderText({
  req(input$quizSub)
  isolate(
    validate(
      need(input$payoutQ == "2 inches", genericWrong)
    )
  )
})
# output$insuranceQuiz <- renderUI({
#   if(purchaseInsurance){
#     tagList(
#       selectInput("premiumQ", "How much does your rain-index insurance cost each year?",
#                   choices = c("", "$0", "$100", "something reasonable")),
#       textOutput("premiumVal"),
#       radioButtons("rainmonthsQ", "Your insurance payouts depend on rain in which months?",
#                    choices = c("May-June, July-August", "May-June, June-July", 
#                                "February-March, May-June", "July-August, October-November")),
#       textOutput("rainmonthsVal"),
#       selectInput("payoutQ", "Would you get a larger insurance payout if you get 5 inches of rain or 2 inches of rain during 
#                   a month that is insured?", choices = c("", "5 inches", "2 inches")),
#       textOutput("payoutVal")
#   )}
# })