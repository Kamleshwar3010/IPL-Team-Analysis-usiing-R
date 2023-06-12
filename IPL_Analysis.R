library(dplyr)
library(ggplot2)
setwd("C:/Users/Admin/OneDrive/Desktop/Data Analytics/Project/")
d <- read.csv("IPL Matches 2008-2020.csv", header = TRUE, sep = ",")
tbl <- as_tibble(d)

#taking overview of data 
head(tbl) 

# Count the number of times each team won the toss
toss <- count(tbl, toss_winner)

# Rename the count column to "Toss Wins" and 
toss <- rename(toss, Toss_Wins = n,)


# Print the count of toss wins for each team
#print(toss)

#sorteng toss data in descending order
toss_sorted <- arrange(toss,desc(Toss_Wins))
print(toss_sorted)

# Grouping team1 column
t1 <- group_by(d,team1)
# Grouping team1 column
t2 <- group_by(d,team2)

#count total match of any team as team1
c_t1 <- count(t1)
# renaming n 
c_t1 <- rename(c_t1, `Total Match as Team 1` = n,)

#print(c_t1)
#count total match of any team as team1
c_t2 <- count(t2)
# renaming n 
c_t2 <- rename(c_t2, `Total Match as Team 2` = n,)

#print(c_t2)

# adding total no of match played by any team (as sum of team1 and team2 )
Sum <- c_t1$`Total Match as Team 1` + c_t2$`Total Match as Team 2`
# print(Sum)

#creating new data frame for total match played by team
total_match <- data.frame(`Team`=c_t1$team1,`match`=Sum)

#print(total_match)

# sorting total match played by team in descending order
total_match_sorted <- arrange(total_match,desc(match))
#print(total_match_sorted)

# calculate toss wins percentage
toss_percentage <- (toss$Toss_Wins/total_match$match)*100
toss_percentage <- round(toss_percentage,3)
#print(toss_percentage)

#creating data frame for toss percentage
toss_percentage_df <- data.frame(`Team`=total_match$Team,toss_percentage)

print(toss_percentage_df)

#sorting data 
toss_percentage_sorted <- arrange(toss_percentage_df,desc(toss_percentage))
#print(toss_percentage_sorted)

# count the no. of time potm award won by player
most_potm <- count(tbl, player_of_match)
#print(most_potm)

# sorting the most potm award in descending order
most_potm_sorted <- arrange(most_potm, desc(n))
#print(most_potm_sorted)

#selectenging top 15 player only
potm <- head(most_potm_sorted,15)
print(potm)

#total match win by a team
match_wins <- count(tbl,winner)
match_wins<- na.omit(match_wins)
print(match_wins)

#sorting data in descending order of match wins
match_wins_sorted <- arrange(match_wins,desc(n))
# Remove rows with missing values
match_wins_sorted <- na.omit(match_wins_sorted)
print(match_wins_sorted)

# win percentage of every team
win_percentage <-(match_wins$n/total_match$match)*100
win_percentage <- round(win_percentage,3)
#print(win_percentage)

# creating win percentage data frame
win_percentage_df <- data.frame(`Team`=match_wins$winner,`Win Percentage`=round(win_percentage,3))
#print(win_percentage_df)

# sorting in decending order
win_percentage_df_sorted <- arrange(win_percentage_df,desc(win_percentage_df$Win.Percentage))
win_percentage_df_sorted <- na.omit(win_percentage_df_sorted)
print(win_percentage_df_sorted)

#finding team that win match and toss both
t_m_w <- tbl %>%filter(toss_winner == winner)
#print(t_m_w)

#grouping them
toss_and_match_winner <-group_by(t_m_w,winner)

#counting no of time team win toss and match both
toss_and_match_winner <- count(toss_and_match_winner)
print(toss_and_match_winner)

#sorting in descending order
toss_and_match_winner_sorted=arrange(toss_and_match_winner,desc(n))
print(toss_and_match_winner_sorted)

#check percentage how many time team won match and toss both
t_a_m_percentage <- data.frame(Team=toss_and_match_winner$winner,Tm_percentage=(round(toss_and_match_winner$n/total_match$match*100,3)))
print(t_a_m_percentage)

# finding team that choose batting after winning toss
toss_bat <- tbl%>% filter(toss_decision =="bat")
print(toss_bat)

#group them 
tb<- group_by(toss_bat,toss_winner)

# counting team choose bat no. of times after winning toss
bat_first<- count(tb)
#print(bat_first)

#sorting in descending order
bat_first_sorted<- arrange(bat_first,desc(n))
print(bat_first_sorted)

#bat first percentage
toss_bat_percentage <- data.frame(Team=bat_first$toss_winner,bat_percentage=(round(bat_first$n/toss$Toss_Wins*100,3)))
print(toss_bat_percentage)
# finding team that choose fielding after winning toss
toss_field <- tbl%>% filter(toss_decision =="field")
print(toss_field)

#group them 
tf<- group_by(toss_field,toss_winner)

# counting team choose fielding no. of times after winning toss
field_first<- count(tf)
#print(field_first)

#sorting in descending order
field_first_sorted<- arrange(field_first,desc(n))
print(field_first_sorted)
#field  first percentage
toss_field_percentage <- data.frame(Team=field_first$toss_winner,field_percentage=(round(field_first$n/toss$Toss_Wins*100,3)))
print(toss_field_percentage)

#counting number of match played by csk against all team
#filter when csk played as team 1
csk_t1 <- tbl%>% filter(team1=="Chennai Super Kings")%>%
  count(team2)
print(csk_t1)

#filter when csk played as team 2
csk_t2 <- tbl%>% filter(team2=="Chennai Super Kings")%>%
  count(team1)
print(csk_t2)

#total match played by csk against each team
csk <- data.frame(Team=csk_t1$team2, match=csk_t1$n+csk_t2$n)
print(csk)

#filter when Decan charger played as team 1
dec_t1 <- tbl%>% filter(team1=="Deccan Chargers")%>%
  count(team2)
print(dec_t1)

#filter when csk played as team 2
dec_t2 <- tbl%>% filter(team2=="Deccan Chargers")%>%
  count(team1)
print(dec_t2)

#total match played by csk against each team
dec <- filter(dec_t1$team2==dec_t2$team1)
print(dec)
#install.packages('tinytex')
#: tinytex::install_tinytex()
team_colors <- c("Mumbai Indians" = "#004ba0",
                 "Royal Challengers Bangalore" = "#CF142B",
                 "Kolkata Knight Riders" = "#3a225d",
                 "Kings XI Punjab" = "#ff000a",
                 "Chennai Super Kings" = "#FFDD11",
                 "Delhi Daredevils" = "#004c93",
                 "Rajasthan Royals" = "#ff00d3",
                 "Sunrisers Hyderabad" = "#ff7203",
                 "Deccan Chargers" = "#1f1f1f",
                 "Pune Warriors" = "#c69c6d",
                 "Delhi Capitals" = "#004eff",
                 "Gujarat Lions" = "#fb643e",
                 "Rising Pune Supergiant" = "#a62457",
                 "Kochi Tuskers Kerala" = "#f68b30")


# Create a bar plot with team names on y-axis and match count on x-axis
ggplot(total_match_sorted, aes(x = match, y = reorder(Team, -match))) +
  geom_bar(stat = "identity", fill = team_colors,)+
  scale_fill_manual(values = team_colors) +
  labs(title = "Total Matches Played by Teams in IPL (2008-2020)",
       x = "Match Count", y = "Team",)+geom_text(aes(label = match), hjust = 1.5, vjust = 0.6, , color = "white") +
  theme_minimal()
# Create plot for how many match win by a team
ggplot(match_wins_sorted, aes(x = n, y =reorder( winner,-n))) +
  geom_bar(stat = "identity", fill = team_colors) +
  scale_fill_manual(values = team_colors) +
  labs(title = "Total Matches Win by Teams in IPL (2008-2020)",
       x = "Match Count", y = "Team") +
  geom_text(aes(label = n), hjust = 1.5, vjust = 0.6, color = "white") +
  theme_minimal()
# create bar chart
ggplot(win_percentage_df_sorted, aes(x = Win.Percentage , y =reorder( Team,- Win.Percentage))) +
  geom_bar(stat = "identity", fill = team_colors) +
  scale_fill_manual(values = team_colors) +
  labs(title = "Win Percentage of Teams in IPL (2008-2020)",
       x = "Match Count", y = "Team") +
  geom_text(aes(label = paste0(c(Win.Percentage),"%")), hjust = 1.5, vjust = 0.6, color = "white") +
  theme_minimal()
# toss win
ggplot(toss_sorted, aes(x = Toss_Wins, y =reorder( toss_winner,-Toss_Wins))) +
  geom_bar(stat = "identity", fill = team_colors) +
  scale_fill_manual(values = team_colors) +
  labs(title = "Total toss Win by Teams in IPL (2008-2020)",
       x = "Match Count", y = "Team") +
  geom_text(aes(label = Toss_Wins), hjust = 1.5, vjust = 0.6, color = "white") +
  theme_minimal()
# toss win percentage
ggplot(toss_percentage_df, aes(x = toss_percentage, y =reorder( Team,-toss_percentage))) +
  geom_bar(stat = "identity", fill = team_colors) +
  scale_fill_manual(values = team_colors) +
  labs(title = "Toss Win Pecentage of Every Teams in IPL (2008-2020)",
       x = "Percentage", y = "Team") +
  geom_text(aes(label = paste0(c(toss_percentage),"%")), hjust = 1.5, vjust = 0.6, color = "white") +
  theme_minimal()
# toss winner choose bat
ggplot(bat_first_sorted, aes(x = n, y =reorder( toss_winner,-n))) +
  geom_bar(stat = "identity", fill = team_colors) +
  scale_fill_manual(values = team_colors) +
  labs(title = "Team choose bat after winning toss in IPL (2008-2020)",
       x = "match count", y = "Team") +
  geom_text(aes(label = n), hjust = 1.5, vjust = 0.6, color = "white") +
  theme_minimal()
# Toss winner choose bat in percent
ggplot(toss_bat_percentage, aes(x = bat_percentage, y =reorder( Team,-bat_percentage))) +
  geom_bar(stat = "identity", fill = team_colors) +
  scale_fill_manual(values = team_colors) +
  labs(title = "Team choose bat after winning toss in IPL (2008-2020)",
       x = "Percentage", y = "Team") +
  geom_text(aes(label = paste0(c(bat_percentage),"%")), hjust = 1.5, vjust = 0.6, color = "white") +
  theme_minimal()
# toss winner choose fielding
ggplot(field_first_sorted, aes(x = n, y =reorder( toss_winner,-n))) +
  geom_bar(stat = "identity", fill = team_colors) +
  scale_fill_manual(values = team_colors) +
  labs(title = "Team choose field after winning toss in IPL (2008-2020)",
       x = "match count", y = "Team") +
  geom_text(aes(label = n), hjust = 1.5, vjust = 0.6, color = "white") +
  theme_minimal()
# Toss winner choose fielding in percent
ggplot(toss_field_percentage, aes(x = field_percentage, y =reorder( Team,-field_percentage))) +
  geom_bar(stat = "identity", fill = team_colors) +
  scale_fill_manual(values = team_colors) +
  labs(title = "Team choose field after winning toss in IPL (2008-2020)",
       x = "Percentage", y = "Team") +
  geom_text(aes(label = paste0(c(field_percentage),"%")), hjust = 1.5, vjust = 0.6, color = "white") +
  theme_minimal()
# toss and match both win by team
ggplot(toss_and_match_winner_sorted, aes(x = n, y =reorder( winner,-n))) +
  geom_bar(stat = "identity", fill = team_colors) +
  scale_fill_manual(values = team_colors) +
  labs(title = "Team Won toss and match both in IPL (2008-2020)",
       x = "match count", y = "Team") +
  geom_text(aes(label = n), hjust = 1.5, vjust = 0.6, color = "white") +
  theme_minimal()
# toss and match both win by team percentage
ggplot(t_a_m_percentage, aes(x = Tm_percentage, y =reorder( Team,-Tm_percentage))) +
  geom_bar(stat = "identity", fill = team_colors) +
  scale_fill_manual(values = team_colors) +
  labs(title = "Team Won toss and match both in IPL (2008-2020)",
       x = "Percentage", y = "Team") +
  geom_text(aes(label = paste0(c(Tm_percentage),"%")), hjust = 1.5, vjust = 0.6, color = "white") +
  theme_minimal()
# Create a color palette for the players
player_colors <- rainbow(nrow(potm))
#potm
ggplot(potm, aes(x = reorder(`player_of_match`, -n), y = n)) +
  geom_col(fill = player_colors) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 15 Players with Most Player of the Match Awards",
       x = "Player", y = "Number of Awards")