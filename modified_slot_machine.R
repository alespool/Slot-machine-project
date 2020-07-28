#g et the symbols
get_symbols <- function(n_wheels = 3) {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = n_wheels, replace = TRUE, 
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

calculate_score <- function(wheel_symbols){
  stopifnot(is.vector(wheel_symbols))
  
  if(all(wheel_symbols == wheel_symbols[1])){
    
    
    payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
                 "B" = 10, "C" = 10, "0" = 0)
    
    prize_index <- which(names(payouts) == wheel_symbols[1])
    
    
    prize <- payouts[[prize_index]]
    
  } else if(all(wheel_symbols %in% c("B","BB","BBB"))) {
    prize <- 5
  } else {
    n_cherries <- sum(wheel_symbols == "C")
    prize <- c(0, 2, 5)[n_cherries + 1]
  }
  
  diamonds <- sum(wheel_symbols == "DD")
  
  prize * (2 ^ diamonds)
  
}

calculate_score(wheel_symbols = c("C","DD","DD"))


play_slots <- function(bet = 1,userprompt = TRUE){
  wheel_outcome <- get_symbols()
  wheel_score <- calculate_score(wheel_symbols = wheel_outcome)
  
  wheel_nice <- wheel_outcome
  wheel_nice[wheel_nice == "DD"] <- "Nice"
  wheel_nice <- paste(wheel_nice,collapse = " ")
  
  wheel_text <- paste(wheel_outcome,collapse = " ")
  
  
  
  
  slot_results <- paste("Slot results:",wheel_text,collapse = " ")
  slot_nice <- paste("Slot niceness:",wheel_nice,collapse = " ")
  slot_prize <- paste("Slot Prize:",wheel_score * bet,collapse = " ")
  
  slot_text <- paste(slot_results,slot_nice,slot_prize,sep = "|")
  
  print(slot_text)
  
  if(userprompt){
    user_choice <- tolower(readline(prompt = "Play again? (y/n)"))
    if(user_choice == "y" | user_choice == ""){
      play_slots(bet = bet,userprompt = TRUE)
    } else {
      print("Thanks for making me rich!")
    }
  }
}

play_slots(userprompt = T)

play_slots(userprompt = F)
