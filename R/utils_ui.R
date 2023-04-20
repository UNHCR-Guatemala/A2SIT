# slider for weights
weights_slider <- function(id, label) {
  sliderInput(id, label = label, min = 0, max = 1, value = 1, step = 0.1, ticks = FALSE)
}
