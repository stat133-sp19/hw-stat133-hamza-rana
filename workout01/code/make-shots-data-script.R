" 
title: make-shots-data-script
description: a file to compile shot data for NBA players and create text files for each player's stats
inputs: stephen-curry.csv, andre-iguodala.csv, klay-thompson.csv, draymond-green.csv, kevin-durant.csv
outputs: stephen-curry-summary.txt, andrew-iguodala-summary.txt, klay-thompson-summary.txt, kevin-durant-summary.txt, draymond-green-summary.txt, shots-data-summary.txt
"

curry <- read.csv("data/stephen-curry.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("data/andre-iguodala.csv", stringsAsFactors = FALSE)
green <- read.csv("data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("data/klay-thompson.csv", stringsAsFactors = FALSE)

curry$name <- "Stephen Curry"
curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"
curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"
curry$minute <- curry$period*12 - curry$minutes_remaining
iguodala$name <- "Andre Iguodala"
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- "shot_yes"
iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- "shot_no"
iguodala$minute <- iguodala$period*12 - iguodala$minutes_remaining
green$name <- "Draymond Green"
green$shot_made_flag[green$shot_made_flag == "y"] <- "shot_yes"
green$shot_made_flag[green$shot_made_flag == "n"] <- "shot_no"
green$minute <- green$period*12 - green$minutes_remaining
thompson$name <- "Klay Thompson"
thompson$shot_made_flag[thompson$shot_made_flag == "y"] <- "shot_yes"
thompson$shot_made_flag[thompson$shot_made_flag == "n"] <- "shot_no"
thompson$minute <- thompson$period*12 - thompson$minutes_remaining
durant$name <- "Kevin Durant"
durant$shot_made_flag[durant$shot_made_flag == "y"] <- "shot_yes"
durant$shot_made_flag[durant$shot_made_flag == "n"] <- "shot_no"
durant$minute <- durant$period*12 - durant$minutes_remaining
sink(file = "output/stephen-curry-summary.txt")
summary(curry)
sink()
sink(file = "output/andre-iguodala-summary.txt")
summary(iguodala)
sink()
sink(file = "output/draymond-green-summary.txt")
summary(green)
sink()
sink(file = "output/klay-thompson-summary.txt")
summary(thompson)
sink()
sink(file = "output/kevin-durant-summary.txt")
summary(durant)
sink()
shots_data <- rbind(curry, iguodala, green, thompson, durant)
write.csv(shots_data, file = "data/shots-data.csv")
sink(file = "output/shots-data-summary.txt")
summary(shots_data)
sink()

