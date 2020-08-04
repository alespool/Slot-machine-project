get_many_symbols <- function(n) {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  vec <- sample(wheel, size = 3 * n, replace = TRUE,
                prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
  matrix(vec, ncol = 3)
}


play_many <- function(n) {
  symb_mat <- get_many_symbols(n = n)
  data.frame(Num1 = symb_mat[,1], Num2 = symb_mat[,2],
             Num3 = symb_mat[,3], prize = score_many(symb_mat))
}

symbols <- matrix(
  c("DD", "DD", "DD", 
    "C", "DD", "0", 
    "B", "B", "B", 
    "B", "BB", "BBB", 
    "C", "C", "0", 
    "7", "DD", "DD"), nrow = 6, byrow = TRUE)

symbols

score_many <- function(symbols){
  
  #count number of cherries
  cherries <- rowSums(symbols == "C")
  diamonds <- rowSums(symbols == "DD")
  
  #wild diamonds count as cherries
  prize <- c(0, 2, 5)[cherries + diamonds + 1]
  #but if there are no cherries, no diamonds too
  prize[!cherries] <- 0
  
  #price for each combination containing three of a kind symbols
  same <- symbols[,1] == symbols[,2] & symbols[,2] == symbols[,3]
  payoffs <- c("DD" = 100, "7" = 80, "BBB" = 40, 
               "BB" = 25, "B" = 10, "C" = 10, "0" = 0)
  prize[same] <- payoffs[symbols[same, 1]]
  
  #price for all BARS
  bars <- symbols == "B" | symbols == "BB" | symbols == "BBB" #the ||is logical OR
  all_bars <- bars[,1] & bars[,2] & bars[,3] & !same
  prize[all_bars] <- 5
  
  #wild diamonds
  two_wilds <- diamonds == 2
  
  #Identify the nonwild symbol, each case where 1 of the symbols isn't a DD
  one <- two_wilds & symbols[, 1] != symbols[, 2] & 
    symbols[, 2] == symbols[, 3]
  two <- two_wilds & symbols[, 1] != symbols[, 2] & 
    symbols[, 1] == symbols[, 3]
  three <- two_wilds & symbols[, 1] == symbols[, 2] & 
    symbols[, 2] != symbols[, 3]
  
  #treat as three of a kind, aka give each a different price 
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  #combos with one wild
  one_wild <- diamonds == 1
  
  #treat as all bars (if necessary or appropriate)
  wild_bars <- one_wild & (rowSums(bars) == 2)
  prize[wild_bars] <- 5
  
  #treat as three of a kind if appropriate
  one <- one_wild & symbols [,1] == symbols [,2]
  two <- one_wild & symbols [,2] == symbols [,3]
  three <- one_wild & symbols[,3] == symbols [,1]
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  #double prize for each diamond
  unname(prize * 2^diamonds)
  
}

system.time(play_many(10000000))

