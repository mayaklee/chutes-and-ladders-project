# Step 1
# representation of the game board
board <- list("ladders_pos" = rbind(c(1, 38),c(4, 14),c(9, 31),c(21, 42),c(28, 84),c(36, 44),c(51, 67),c(71, 91),c(80, 100)),
              "chutes_pos" = rbind(c(16, 6),c(48, 26),c(49, 11),c(56, 53),c(62, 19),c(64, 60),c(87, 24),c(93, 73),c(95, 75),c(98, 78)),
              n_rows = 10, n_cols = 10)


show_board <- function(board) {
  n_rows <- board$n_rows
  n_cols <- board$n_cols

  plot(1:n_rows, 1:n_cols, type = "n", xaxt = "n", yaxt = "n", main = "Chutes and Ladders")  # set up coord. system
  grid(nx = board$n_rows, ny = board$n_cols, col = "black", lty = "solid", lwd = par("lwd"), equilogs = TRUE) # plot gridlines

  for (i in 1:board$n_rows) {
    if (i %% 2 == 0) {
      text(1:board$n_cols,rep(i,board$n_cols),label = (i*n_cols):((i-1)*n_cols + 1))
    } else {
      text(1:board$n_cols,rep(i,board$n_cols),label = ((i-1)*n_cols + 1):(i*n_cols))
    }
  }

  # draw ladders
  arrows(1,1,3,4, col = "yellow", lwd = 3)
  arrows(4,1,7,2, col = "yellow", lwd = 3)
  arrows(9,1,10,4, col = "yellow", lwd = 3)
  arrows(8,3,4,9, col = "yellow", lwd = 3)
  arrows(1,3,2,5, col = "yellow", lwd = 3)
  arrows(5,4,4,5, col = "yellow", lwd = 3)
  arrows(10,6,7,7, col = "yellow", lwd = 3)
  arrows(10,8,10,10, col = "yellow", lwd = 3)
  arrows(1,8,1,10, col = "yellow", lwd = 3)
  arrows(9,1,10,4, col = "yellow", lwd = 3)

  # draw chutes
  arrows(5,2,6,1, col = "red", lwd = 3)
  arrows(9,5,10,2, col = "red", lwd = 3)
  arrows(2,7,2,2, col = "red", lwd = 3)
  arrows(7,9,4,3, col = "red", lwd = 3)
  arrows(7,5,6,3, col = "red", lwd = 3)
  arrows(5,6,8,6, col = "red", lwd = 3)
  arrows(4,7,1,6, col = "red", lwd = 3)
  arrows(8,10,8,8, col = "red", lwd = 3)
  arrows(6,10,6,8, col = "red", lwd = 3)
  arrows(3,10,3,8, col = "red", lwd = 3)
}

# Step 2
# play the game

play_cl <- function(n_players = 1, spinner, board) {
  if(!is.numeric(n_players) | n_players <= 0) {
    stop("The number of players is not numeric!")
  }
  if(!is.numeric(spinner)) {
    stop("The spinner input is not numeric!")
  }

  if(!is.vector(spinner)) {
      spinner <- c(1:spinner)
  }

  board_rows <- board$n_rows
  board_cols <- board$n_cols
  board_size <- board_rows * board_cols

  current_position <- rep(0,n_players)
  turns <- rep(0,n_players)
  chutes <- rep(0,n_players)
  ladders <- rep(0,n_players)
  might_win <- rep(0,n_players)
  sixth_turn <- rep(0,n_players)

  i <- 0
  while(TRUE) {
    player <- (i %% n_players) + 1 # index of which player is going
    # increase turn for player
    turns[player] <- turns[player] + 1
    # have player take one turn
    spin <- sample(spinner,1)

    # move forward
    if(current_position[player] + spin <= board_size) {
      current_position[player] <- current_position[player] + spin
    }

    # go up ladder
    if(any(board$ladders_pos[,1] %in% current_position[player])) {
      row <- which(board$ladders_pos[,1] == current_position[player])
      current_position[player] <- board$ladders_pos[row,2]
      ladders[player] <- ladders[player] + 1
    }
    # go down chute
    if(any(board$chutes_pos[,1] %in% current_position[player])) {
      row <- which(board$chutes_pos[,1] == current_position[player])
      current_position[player] <- board$chutes_pos[row,2]
      chutes[player] <- chutes[player] + 1
    }
    # save sixth turn position
    if(turns[player] == 6) {
      sixth_turn[player] <- current_position[player]
    }
    # check if won
    if(current_position[player] == board_size) {
      close <- sum(might_win[-player]) > 0
      result <- list("Winner" = player, "Turns" = turns, "Chutes" = chutes, "Ladders" = ladders, "Close_Game" = close, "Sixth_Square" = sixth_turn)
      return(result)
    }
    # see if any could win on next round
    if(any((board_size - 5):(board_size - 1) %in% current_position[player]) | any(74:79 %in% current_position[player])) {
      might_win[player] <- 1
    } else {
      might_win[player] <- 0
    }

    # increase number of turns after turn has been taken
    i <- i + 1

  }
}

make_random_board <- function(n_rows = 10, n_cols = 10, n_chutes = 10, n_ladders = 9) {
  # determine size of board
  board_squares <- n_cols * n_rows
  # pick eligible squares for chutes and ladders
  chutes_ladders_pos <- sample(board_squares,(n_chutes*2)+(n_ladders*2),replace = FALSE)
  # empty chutes position matrix
  chutes_pos <- matrix(data = NA, nrow = n_chutes, ncol = 2)
  # empty ladders position matrix
  ladders_pos <- matrix(data = NA, nrow = n_ladders, ncol = 2)


  for (j in 1:n_chutes) {
   a <- chutes_ladders_pos[2*j-1]
   b <- chutes_ladders_pos[2*j]
   chutes_pos[j,1] <- max(a,b)
   chutes_pos[j,2] <- min(a,b)
  }

  for (j in 1:n_ladders) {
    a <- chutes_ladders_pos[(2*n_chutes) + 2*j-1]
    b <- chutes_ladders_pos[(2*n_chutes) + 2*j]
    ladders_pos[j,1] <- min(a,b)
    ladders_pos[j,2] <- max(a,b)
  }
  output <- list("ladders_pos" = ladders_pos, "chutes_pos" = chutes_pos, "n_rows" = n_rows, "n_cols" = n_cols)
  return(output)
}

## Simulations
## Regular Game, One Player
set.seed(304787099)

board <- list("ladders_pos" = rbind(c(1, 38),c(4, 14),c(9, 31),c(21, 42),c(28, 84),c(36, 44),c(51, 67),c(71, 91),c(80, 100)),"chutes_pos" = rbind(c(16, 6),c(48, 26),c(49, 11),c(56, 53),c(62, 19),c(64, 60),c(87, 24),c(93, 73),c(95, 75),c(98, 78)),n_rows = 10, n_cols = 10)

mat_one <- matrix(data = NA, nrow = 10000, ncol = 4)
for(i in 1:10000) {
  mat_one[i,1] <- play_cl(1,6,board)$Turns
  mat_one[i,2] <- play_cl(1,6,board)$Winner
  mat_one[i,3] <- play_cl(1,6,board)$Close_Game
  mat_one[i,4] <- play_cl(1,6,board)$Sixth_Square
}
minimum_turns <- min(mat_one[,1])
minimum_turns
prop_p1 <- sum(mat_one[,2] == 1)/10000
prop_p1 # proportion won by player 1
min_poss <- (sum(as.logical((which(mat_one[,1] == minimum_turns)))))/10000
min_poss
close_prop <- sum(mat_one[,3])/10000
close_prop
maximum_turns <- max(mat_one[,1])
maximum_turns
nums <- unique(mat_one[,4])
mode <- nums[which.max(tabulate(match(mat_one[,4], nums)))]
mode

## Regular Game, Two Player
set.seed(990787403)
board <- list("ladders_pos" = rbind(c(1, 38),c(4, 14),c(9, 31),c(21, 42),c(28, 84),c(36, 44),c(51, 67),c(71, 91),c(80, 100)),"chutes_pos" = rbind(c(16, 6),c(48, 26),c(49, 11),c(56, 53),c(62, 19),c(64, 60),c(87, 24),c(93, 73),c(95, 75),c(98, 78)),n_rows = 10, n_cols = 10)

mat_two <- matrix(data = NA, nrow = 10000, ncol = 6)
for(i in 1:10000) {
  mat_two[i,1:2] <- play_cl(2,6,board)$Turns
  mat_two[i,3] <- play_cl(2,6,board)$Winner
  mat_two[i,4] <- play_cl(2,6,board)$Close_Game
  mat_two[i,5:6] <- play_cl(2,6,board)$Sixth_Square
}
minimum_turns <- min(mat_two[,1:2]) * 2
minimum_turns
prop_p1 <- sum(mat_two[,3] == 1)/10000
prop_p1 # proportion won by player 1
min_poss <- (sum(as.logical((which(mat_two[,1:2] == minimum_turns)))))/10000
min_poss
close_prop <- sum(mat_two[,4])/10000
close_prop
maximum_turns <- max(mat_two[,1:2]) * 2
maximum_turns
nums <- unique(mat_two[,5:6])
mode <- nums[which.max(tabulate(match(mat_two[,5:6], nums)))]
mode

## Regular Game, Three Players
set.seed(304787099)
set.seed(sample(1e4,1))
board <- list("ladders_pos" = rbind(c(1, 38),c(4, 14),c(9, 31),c(21, 42),c(28, 84),c(36, 44),c(51, 67),c(71, 91),c(80, 100)),"chutes_pos" = rbind(c(16, 6),c(48, 26),c(49, 11),c(56, 53),c(62, 19),c(64, 60),c(87, 24),c(93, 73),c(95, 75),c(98, 78)),n_rows = 10, n_cols = 10)

mat_three <- matrix(data = NA, nrow = 10000, ncol = 8)
for(i in 1:10000) {
  mat_three[i,1:3] <- play_cl(3,6,board)$Turns
  mat_three[i,4] <- play_cl(3,6,board)$Winner
  mat_three[i,5] <- play_cl(3,6,board)$Close_Game
  mat_three[i,6:8] <- play_cl(3,6,board)$Sixth_Square
}
minimum_turns <- min(mat_three[,1:3]) * 3
minimum_turns
prop_p1 <- sum(mat_three[,4] == 1)/10000
prop_p1 # proportion won by player 1
prop_p2 <- sum(mat_three[,4] == 2)/10000
prop_p2
min_poss <- (sum(as.logical((which((mat_three[,1]) == minimum_turns)))))/10000
min_poss
close_prop <- sum(mat_three[,5])/10000
close_prop
maximum_turns <- max(mat_three[,1:3]) * 3
maximum_turns
nums <- unique(mat_three[,6:8])
mode <- nums[which.max(tabulate(match(mat_three[,6:8], nums)))]
mode

## Question 4
turns_one <- mat_one[,1]
turns_two <- mat_two[,1:2]
turns_three <- mat_three[,1:3]
hist(turns_one,col=rgb(0,0,1,1/4), xlim=c(0,250), ylim=c(0,7000),xlab = "Turns Needed to Finish Game", main = "Distribution of Turns Needed to Finish Each Game Setup")
hist(turns_two,col=rgb(0.5,0,0,1/4),xlim=c(0,250),ylim=c(0,7000), add=T)
hist(turns_three,col=rgb(1,0,1,1/4),xlim=c(0,250),ylim=c(0,7000), add=T)
legend("topright", fill = c(rgb(0,0,1,1/4), rgb(0.5,0,0,1/4), rgb(1,0,1,1/4)), legend = c("One Player", "Two Players", "Three Players"))

## Question 5
fewer_turns <- sum(as.logical(which((mat_three[,1]*3) < max(mat_one[,1] * 3))))/10000
fewer_turns

## Custom Game, One Player
set.seed(304787099)

board <- make_random_board(11,13,13,11)

mat_one <- matrix(data = NA, nrow = 10000, ncol = 4)
for(i in 1:10000) {
  mat_one[i,1] <- play_cl(1,c(1,2,3,5,7,11,13),board)$Turns
  mat_one[i,2] <- play_cl(1,c(1,2,3,5,7,11,13),board)$Winner
  mat_one[i,3] <- play_cl(1,c(1,2,3,5,7,11,13),board)$Close_Game
  mat_one[i,4] <- play_cl(1,c(1,2,3,5,7,11,13),board)$Sixth_Square
}
minimum_turns <- min(mat_one[,1])
minimum_turns
prop_p1 <- sum(mat_one[,2] == 1)/10000
prop_p1 # proportion won by player 1
min_poss <- (sum(as.logical((which(mat_one[,1] == minimum_turns)))))/10000
min_poss
close_prop <- sum(mat_one[,3])/10000
close_prop
maximum_turns <- max(mat_one[,1])
maximum_turns
nums <- unique(mat_one[,4])
mode <- nums[which.max(tabulate(match(mat_one[,4], nums)))]
mode

## Custom Game, Two Player
set.seed(990787403)

board <- make_random_board(11,13,13,11)

mat_two <- matrix(data = NA, nrow = 10000, ncol = 6)
for(i in 1:10000) {
  mat_two[i,1:2] <- play_cl(2,c(1,2,3,5,7,11,13),board)$Turns
  mat_two[i,3] <- play_cl(2,c(1,2,3,5,7,11,13),board)$Winner
  mat_two[i,4] <- play_cl(2,c(1,2,3,5,7,11,13),board)$Close_Game
  mat_two[i,5:6] <- play_cl(2,c(1,2,3,5,7,11,13),board)$Sixth_Square
}
minimum_turns <- min(mat_two[,1:2]) * 2
minimum_turns
prop_p1 <- sum(mat_two[,3] == 1)/10000
prop_p1 # proportion won by player 1
min_poss <- (sum(as.logical((which(mat_two[,1:2] * 2 == minimum_turns)))))/10000
min_poss
close_prop <- sum(mat_two[,4])/10000
close_prop
maximum_turns <- max(mat_two[,1:2]) * 2
maximum_turns
nums <- unique(mat_two[,5:6])
mode <- nums[which.max(tabulate(match(mat_two[,5:6], nums)))]
mode

## Custom Game, Three Player
set.seed(304787099)
set.seed(sample(1e4,1))

board <- make_random_board(11,13,13,11)

mat_three <- matrix(data = NA, nrow = 10000, ncol = 8)
for(i in 1:10000) {
  mat_three[i,1:3] <- play_cl(3,c(1,2,3,5,7,11,13),board)$Turns
  mat_three[i,4] <- play_cl(3,c(1,2,3,5,7,11,13),board)$Winner
  mat_three[i,5] <- play_cl(3,c(1,2,3,5,7,11,13),board)$Close_Game
  mat_three[i,6:8] <- play_cl(3,c(1,2,3,5,7,11,13),board)$Sixth_Square
}
minimum_turns <- min(mat_three[,1:3]) * 3
minimum_turns
prop_p1 <- sum(mat_three[,4] == 1)/10000
prop_p1 # proportion won by player 1
prop_p2 <- sum(mat_three[,4] == 2)/10000
prop_p2 # proportion won by player 2
min_poss <- (sum(as.logical((which(mat_three[,1:3] * 3 == minimum_turns)))))/10000
min_poss
close_prop <- sum(mat_three[,5])/10000
close_prop
maximum_turns <- max(mat_three[,1:3]) * 3
maximum_turns
nums <- unique(mat_three[,6:8])
mode <- nums[which.max(tabulate(match(mat_three[,6:8], nums)))]
mode
