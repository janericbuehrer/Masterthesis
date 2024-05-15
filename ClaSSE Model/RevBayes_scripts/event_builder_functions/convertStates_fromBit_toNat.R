path <- "C:/Users/janer/Documents/ETH MASTER/Master Thesis/Gentiana/"
setwd(path)
file <- paste0(path, "data//Gent_completeModel_bit.csv")
dat <- read.csv2(file = file, header = T)

bit_state <- c(
  '00001', '00010', '00011', '00100', '00101', '00110', '00111',
  '01000', '01001', '01010', '01011', '01100', '01101', '01110', '01111',
  '10000', '10001', '10010', '10011', '10100', '10101', '10110', '10111',
  '11000', '11001', '11010', '11011', '11100', '11101', '11110', '11111'
)


dat_state <- c()
nat_state <- c()
for (row_index in 1:nrow(dat)) {
  # Convert elements to characters and concatenate
  dat_state[row_index] <- paste0(as.character(dat[row_index, -1]), collapse = "")
  nat_state[row_index] <- which(bit_state == dat_state[row_index]) -1
}
nat_state <- as.integer(nat_state)
Gent_completeModel_nat <- data.frame(taxa = dat[,1], state = nat_state)

write.csv2(Gent_completeModel_nat, file = paste0(path, "data/Gent_completeModel_nat.csv"), row.names = F)
