###################################
# Keep
##################################
observeEvent( input$var, {
  val <- values[,input$var]
  
  # Control the value, min, max, and step.
  # Step size is 2 when input value is even; 1 when value is odd.
  updateSliderInput(session, "range", value = val,
                    min = min(val), max = max(val))
})
########################################