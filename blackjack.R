# 
# @author: Harvey Villanueva
# instead of working on my midterm, let's make this project lol
# what an amazing use of my time
# @github: tptnol

# Function Definitions --------------------------------------------------

# creates a deck of cards
create_deck <- function() {
  suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
  values <- c(2:10, "J", "Q", "K", "A")
  deck <- expand.grid(Value = values, Suit = suits)
  deck$Value <- as.character(deck$Value)
  return(deck)
}

# assigns the numerical values to cards
card_value <- function(card) {
  if (card %in% c("J", "Q", "K")) {
    return(10)
  } else if (card == "A") {
    return(11)  # ace initially counts as 11, switches to 1 when over 21
  } else {
    return(as.numeric(card))
  }
}

# function to calculate the hand's total value
calculate_hand <- function(hand) {
  # yoink card value
  values <- sapply(hand$Value, card_value)
  
  # sum dat hoe up
  total <- sum(values)
  
  # adjust for Aces if the total is greater than 21
  num_aces <- sum(hand$Value == "A")
  
  # i have to check for multiple aces oop
  while (total > 21 && num_aces > 0) {
    total <- total - 10  # reduce the Ace from 11 to 1
    num_aces <- num_aces - 1
  }
  
  return(total)
}

# function to deal singular card from the deck
deal_card <- function(deck) {
  # grab one row
  card <- deck[sample(nrow(deck), 1), ]
  
  # remove the dealt card from deck
  deck <- deck[!rownames(deck) %in% rownames(card), ]
  
  # throw it back like ice spice
  return(list(card = card, deck = deck))
}

# GAME LOOP --------------------------------------------------------------

# game loop starts here
blackjack <- function() {
  # create the deck with the function
  deck <- create_deck()
  
  # initialize player and dealer hands
  player_hand <- data.frame()
  dealer_hand <- data.frame()
  
  # deal two cards to player and dealer
  for (i in 1:2) {
    # deal two cards to player
    dealt <- deal_card(deck)
    player_hand <- rbind(player_hand, dealt$card)
    deck <- dealt$deck
    
    # deal two cards to dealer
    dealt <- deal_card(deck)
    dealer_hand <- rbind(dealer_hand, dealt$card)
    deck <- dealt$deck
  }
  
  # show player hand
  cat("\nYour hand:\n")
  print(player_hand)
  
  # show dealer hand
  cat("\nDr. Ho shows only one of her two cards:\n")
  print(dealer_hand[1,]) # only show one 
  
  player_total <- calculate_hand(player_hand)
  dealer_total <- calculate_hand(dealer_hand)
  
  # on the player's turn
  while (TRUE) {
    cat("\nYour current total:", player_total, "\n")
    if (player_total > 21) {
      cat("\nYou busted! Dr. Ho wins. No bonus points for you.\n")
      return("womp womp womp *Fortnite dance* womp womp womp") # return to break out main loop
    }
    
    # prompt player, read input
    action <- readline(prompt = "Do you want to 'hit' or 'stand'? ")
    
    # if stand, then do not draw, let dealer draw
    if (action == "stand") {
      break
    }
    
    # draw card
    dealt <- deal_card(deck)
    
    # add that card to the player hand
    player_hand <- rbind(player_hand, dealt$card)
    deck <- dealt$deck
    
    # calculate that value
    player_total <- calculate_hand(player_hand)
    cat("\nYour hand:\n")
    print(player_hand)
  }
  
  # dealer's turn
  cat("\nDr Ho's hand:\n")
  print(dealer_hand)
  
  # logic, when the dealer is under 17, will always hit!
  while (dealer_total < 17) {
    dealt <- deal_card(deck)
    dealer_hand <- rbind(dealer_hand, dealt$card)
    deck <- dealt$deck
    dealer_total <- calculate_hand(dealer_hand)
    cat("\nDr. Ho hits:\n")
    print(dealer_hand)
  }
  
  cat("\nDr. Ho's total:", dealer_total, "\n")
  if (dealer_total > 21) {
    cat("\nDr. Ho busted! You win bonus points.\n")
  } else if (dealer_total > player_total) {
    cat("\nDr. Ho wins. Sorry! No bonus points.\n")
  } else if (dealer_total < player_total) {
    cat("\nYou win! Time for bonus points!\n")
  } else {
    cat("\nIt's a tie! Let's try this again.\n")
  }
}

# begin game
blackjack()
