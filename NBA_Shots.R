#It is done by Estapraq Kahlil / www.estapraq.com
# Follow me on tweeter @EstapraqAlani / Meduim @estapraqkahlilalani

########################################Libraries#########################################
#ddply library: this library is for the aggegation 
library(plyr)
################################Download SHOT CHART DETAILS DATA#############################
#URL data for Oklahoma Thunder
ThunderTeamID <- "1610612760" 
MyURL<-paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2015-16&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=0&PlusMinus=N&Position=&RankN&RookieYear=&Season=2015-16&SeasonSegment=&SeasonType=Regular+Season&TeamID=",ThunderTeamID,"&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")

#Get the data 
#The result out of this data is with assumed header names
ShotChartDetail<-fromJSON(file= MyURL, method ="C")


# unlist: to produce a vector\ contains all the atomic component in the 
#R object(type:list) Mydata, and convert it to data frame. 
#'ncolum' comes from the list# in the environment window
ShotChartDetailFrame <- data.frame(matrix(unlist(ShotChartDetail$resultSets[[1]][[3]]), ncol=21, byrow = TRUE))

# assign the columns names
colnames(ShotChartDetailFrame) <- ShotChartDetail$resultSets[[1]][[2]]

# coerces numeric mode columns to numeric (i.e double type): For example -.1 -> -0.1 and "A"->"NA"

ShotChartDetailFrame$LOC_X <- as.numeric(as.character(ShotChartDetailFrame$LOC_X))
ShotChartDetailFrame$LOC_Y <- as.numeric(as.character(ShotChartDetailFrame$LOC_Y))
ShotChartDetailFrame$SHOT_DISTANCE <- as.numeric(as.character(ShotChartDetailFrame$SHOT_DISTANCE))

# view the data 
View(ShotChartDetailFrame)

#Write as s .csv file 
write.csv(ShotChartDetailFrame, file = "NBA-shotchartdetails2015-16.csv")


##############################Download the NBA-Common Team Roster###########################

#Thunder team id number
ThunderTeamID <- "1610612760"
MyURL2<-paste("http://stats.nba.com/stats/commonteamroster?Season=2015-16&TeamID=",ThunderTeamID, sep = "")
#CFID=33&CFPARAMS=2015-16&ContextFilter=&
#get the data 
#The result out of this data is with assumed header names
CommonTeamRoster<-fromJSON(file= MyURL2, method ="C")


# unlist: to produce a vector\ contains all the atomic 
#component in the R object(type:list) Mydata, and convert it to data frame
CommonTeamRosterFrame <- data.frame(matrix(unlist(CommonTeamRoster$resultSets[[1]][[3]]), ncol=13, byrow = TRUE))

# assign the columns names
colnames(CommonTeamRosterFrame) <- CommonTeamRoster$resultSets[[1]][[2]]
View(CommonTeamRosterFrame)

# Write the data to a .csv file
write.csv(CommonTeamRosterFrame, file = "NBA-CommonTeamRoster.csv")

#######################DOWNLOAD THE PLAYER CAREER STATS FRAME ################################

# k: is a variable to run through the players names in the roster
k <-1 

#A list of lists to save the data for each player
PlayerCareerStatsList <- list()

#For loop to run through the Thunder players id numbers 
for(id in CommonTeamRosterFrame$PLAYER_ID){
MyURL3<-paste("http://stats.nba.com/stats/playercareerstats?PlayerID=",id,"&PerMode=Totals", sep ="") 
#get the data 
#The result out of this data is with assumed header names
PlayerCareerStats<-fromJSON(file= MyURL3, method ="C")

# unlist: to produce a vector\ contains all the atomic 
#component in the R object(type:list) Mydata, and convert it to data frame
PlayerCareerStatsFrame<- data.frame(matrix(unlist(PlayerCareerStats$resultSets[[1]][[3]]), ncol=27, byrow = TRUE))

# assign the columns names
colnames(PlayerCareerStatsFrame) <- PlayerCareerStats$resultSets[[1]][[2]]

#Assign the information in the list of lists 
PlayerCareerStatsList[[id]]<-PlayerCareerStatsFrame

#write to a .csv format. We are using 'k' to name the files
write.csv(PlayerCareerStatsList[[id]], file=paste(CommonTeamRosterFrame$PLAYER[k],"csv", sep="."))
#increase it by one 
k <-k+1
}#end of for loop

#############################clean the PlayerCareerStatsList#############################
# Delet the rows that represnt the "total" of the shoots for all seasons through the one year 
#List of lists
PlayerCareerStatsList_NEW <- list()

# k: To run through the datafor each player 
k <- 1
# m: To run through the players names roster. It uses to name the csv files 
m<-1

# id runs through the Thunder players id numbers
for(id in CommonTeamRosterFrame$PLAYER_ID){ 
        #assign it in a new list to make sure you have a backup copy
        PlayerCareerStatsList_NEW[[id]] <- PlayerCareerStatsList[[id]]
        # del: to count the delet rows
        del <- 0 
        #While loop: is to run through each list for each player and clean it
        while(k<=length(PlayerCareerStatsList[[id]]$TEAM_ID)){
                #to catch the zero values
                if(PlayerCareerStatsList[[id]]$TEAM_ID[k]=='0'|PlayerCareerStatsList[[id]]$TEAM_ID[k]== "\t 0"|PlayerCareerStatsList[[id]]$TEAM_ID[k]=="0\t"){ 
                        #delet the row that has zerow
                        delrow= k-del#the row that will be deleted
                        PlayerCareerStatsList_NEW[[id]]= PlayerCareerStatsList_NEW[[id]][-delrow, ]
                        del <- del+1
                }#end of if
                k<-k+1
        }# end of while loop
      #reset k to one
      k<-1 
      #write .csv file
      write.csv(PlayerCareerStatsList_NEW[[id]], file=paste(CommonTeamRosterFrame$PLAYER[m],"NEW",".csv", sep=""))
m<-m+1 # to save the csv files
      }#end of for loop

##############################Aggregate data in PlayerCareerStatsList_NEW#####################
#Aggregate the variables with the "season_ID" variable, since they play few games in a sereie 
# during the year

# create another list of lists for the aggregation
PlayerCareerStatsList_NEW_aggregate <- list()

# For loop: to run through the Thunder player id numbers 
for(id in CommonTeamRosterFrame$PLAYER_ID){
#The sum function doesn't work on factor, you have to change it to numeric
PlayerCareerStatsList_NEW[[id]]$AST <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$AST))
PlayerCareerStatsList_NEW[[id]]$STL <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$STL))
PlayerCareerStatsList_NEW[[id]]$PLAYER_AGE <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$PLAYER_AGE))
PlayerCareerStatsList_NEW[[id]]$GP <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$GP))
PlayerCareerStatsList_NEW[[id]]$GS <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$GS))
PlayerCareerStatsList_NEW[[id]]$MIN <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$MIN))
PlayerCareerStatsList_NEW[[id]]$FGM <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$FGM))
PlayerCareerStatsList_NEW[[id]]$FGA <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$FGA))
PlayerCareerStatsList_NEW[[id]]$FG_PCT <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$FG_PCT))
PlayerCareerStatsList_NEW[[id]]$FG3M <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$FG3M))

PlayerCareerStatsList_NEW[[id]]$FG3A <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$FG3A))
PlayerCareerStatsList_NEW[[id]]$FG3_PCT <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$FG3_PCT))
PlayerCareerStatsList_NEW[[id]]$FTM <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$FTM))
PlayerCareerStatsList_NEW[[id]]$FTA <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$FTA))
PlayerCareerStatsList_NEW[[id]]$FT_PCT <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$FT_PCT))
PlayerCareerStatsList_NEW[[id]]$OREB <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$OREB))
PlayerCareerStatsList_NEW[[id]]$DREB <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$DREB))
PlayerCareerStatsList_NEW[[id]]$REB <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$REB))
PlayerCareerStatsList_NEW[[id]]$BLK <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$BLK))
PlayerCareerStatsList_NEW[[id]]$TOV <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$TOV))
PlayerCareerStatsList_NEW[[id]]$PF <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$PF))
PlayerCareerStatsList_NEW[[id]]$PTS <- as.numeric(as.character(PlayerCareerStatsList_NEW[[id]]$PTS))

#PlayerCareerStatsList_NEW[[id]]$SEASON_ID <- as.numeric(as.character(PlayerCareerStatsList_NEW[["1737"]]$SEASON_ID))
        #newdata<- aggregate(PlayerCareerStatsList_NEW[["1737"]]$SEASON_ID, 
        
# aggregate: the aggregate function is not useful here, you should use the ddply
PlayerCareerStatsList_NEW_aggregate[[id]] <- ddply(PlayerCareerStatsList_NEW[[id]], .(SEASON_ID), summarise, AST=sum(AST),
                                                  number_of_season= length(SEASON_ID), STL=sum(STL),OREB=sum(OREB),DREB=sum(DREB),
                                                  REB=sum(REB),BLK=sum(BLK),TOV=sum(TOV), PF=sum(PF), PTS=sum(PTS),FG3M=sum(FG3M),
                                                  FG3A=sum(FG3A),FG3_PCT=sum(FG3_PCT), FTM=sum(FTM), FTA=sum(FTA), 
                                                  FT_PCT=sum(FT_PCT),PLAYER_AGE=sum(PLAYER_AGE), GP=sum(GP), GS=sum(GS), 
                                                  MIN=sum(MIN),FGM=sum(FGM), FGA=sum(FGA),FG_PCT=sum(FG_PCT))
}#end of the for loop



###################regex the season_ID in PlayerCareerStatsList_NEW_aggregate #####################
#Replace the period of year by the year 

#For loop: to run through the list of players id numbers
for(j in CommonTeamRosterFrame$PLAYER_ID){
        #create a vector p
        p <- c()
        # len: is the length of the season_ID for each player
        len <- length(PlayerCareerStatsList_NEW_aggregate[[j]]$SEASON_ID)
        #For loop: to run through the vector of the season_ID for each player
        for(i in 1:len){
                #assign the season_ID to "p" as a subset
                p[i] <-sub(" *-..", "", PlayerCareerStatsList_NEW_aggregate[[j]]$SEASON_ID[i])
               }# end of the second for loop
        #assign the vector "p" back in the 'SEASON_ID' for each player 
        PlayerCareerStatsList_NEW_aggregate[[j]]$SEASON_ID <- p
        #destroy the vector 'p'
        remove(p)
}# end of the first for loop

############################# Exploring some plots for Russell Westbrook#########################
##  Open a new default device. Red dots in the plot means it is a series 
get( getOption( "device" ) )()

##  Split the screen into two rows and one column, defining screens 1 and 2.
split.screen( figs = c( 2, 1 ) )

##  Split screen 1 into one row and three columns, defining screens 3, 4, and 5.
split.screen( figs = c( 1, 3), screen = 1 )

##  Split screen 2 into one row and two columns, defining screens 6 and 7.
split.screen( figs = c( 1, 2 ), screen = 2 )

##  The first plot is located in screen 3:
screen( 3 )
plot( x, y, xlab = "Season", ylab = "Assist_shot", pch = 19, col = seasonnum )
title("", "NBA_Thunder")

##  The second plot is located in screen 4:
screen( 4 )
plot( x, PlayerCareerStatsList_NEW_aggregate[["201566"]]$STL, xlab = "Season", ylab = "Steal_shot", pch = 19, col = seasonnum )
title("", "NBA_Thunder")
##  The third plot is located in screen 5:
screen( 5 )
plot( y, PlayerCareerStatsList_NEW_aggregate[["201566"]]$OREB, xlab = "Steal_shot", ylab = "OREB_shot", pch = 19, col = seasonnum )
title("Red: total of games", "NBA_Thunder")

##  The fourth plot is located in screen 6:
screen( 6 )
plot( x,PlayerCareerStatsList_NEW_aggregate[["201566"]]$OREB , xlab = "Season", ylab = "OREB", pch = 19, col = seasonnum )
title("", "NBA_Thunder")

##  The fifth plot is located in screen 7:
screen( 7 )
plot( PlayerCareerStatsList_NEW_aggregate[["201566"]]$PLAYER_AGE,PlayerCareerStatsList_NEW_aggregate[["201566"]]$REB, xlab = "Player_age", ylab = "REB_shot", pch = 19, col = seasonnum )
title("Red: total of games", "NBA_Thunder")

##  Close all screens.
close.screen( all = TRUE )

################################Exploring the plots of age vs stealing shoots################

get( getOption( "device" ) )()

##  Split the screen into two rows and one column, defining screens 1 and 2.

split.screen( figs = c( 2, 1 ) )

##  Split screen 1 into one row and three columns, defining screens 3, 4, and 5.

split.screen( figs = c( 1, 3 ), screen = 1 )

##  Split screen 2 into one row and two columns, defining screens 6 and 7.

split.screen( figs = c( 1, 2 ), screen = 2 )

##  The first plot is located in screen 3:
#Roberson
screen( 3 )
plot( PlayerCareerStatsList_NEW_aggregate[["203460"]]$PLAYER_AGE,PlayerCareerStatsList_NEW_aggregate[["203460"]]$STL, xlab = "Roberson_age", ylab = "STL_shot", pch = 19, col = seasonnum )
title("Red: total of games", "NBA_Thunder")

##  The second plot is located in screen 4:
#Adam
screen( 4 )
plot( PlayerCareerStatsList_NEW_aggregate[["203500"]]$PLAYER_AGE,PlayerCareerStatsList_NEW_aggregate[[" 203500"]]$STL, xlab = "Adam_age", ylab = "STL_shot", pch = 19, col = seasonnum )
title("Black:a game a year", "NBA_Thunder")

##  The third plot is located in screen 5:
#Kanter
screen( 5 )
plot( PlayerCareerStatsList_NEW_aggregate[["202683"]]$PLAYER_AGE,PlayerCareerStatsList_NEW_aggregate[["202683"]]$STL, xlab = "Kanter_age", ylab = "STL_shot", pch = 19, col = seasonnum )
title("", "NBA_Thunder")

##  The fourth plot is located in screen 6:
#Kevin
screen( 6 )
plot( PlayerCareerStatsList_NEW_aggregate[["201142"]]$PLAYER_AGE,PlayerCareerStatsList_NEW_aggregate[["201142"]]$STL, xlab = "Kevin_age", ylab = "STL_shot", pch = 19, col = seasonnum )
title("", "NBA_Thunder")

##  The fifth plot is located in screen 7:
#Russel 
screen( 7 )
plot( PlayerCareerStatsList_NEW_aggregate[["201566"]]$PLAYER_AGE,PlayerCareerStatsList_NEW_aggregate[["201566"]]$STL, xlab = "Russel_age", ylab = "STL_shot", pch = 19, col = seasonnum )
title("", "NBA_Thunder")

###############################Exploring correlations##########################################

# correlation between STL and player_age
#For loop to run through the player id numbers 
for(id in CommonTeamRosterFrame$PLAYER_ID){
        #name: to run through the list of the players names 
        name<-CommonTeamRosterFrame$PLAYER[CommonTeamRosterFrame$PLAYER_ID==id]
        #print player's name
        print(name)
        #c: to sighn the correlation result
        c<- cor(PlayerCareerStatsList_NEW_aggregate[[id]]$STL, PlayerCareerStatsList_NEW_aggregate[[id]]$PLAYER_AGE)
       #print correlation
        print(c)

       }

# correlation between PTS and player_age 
#For loop to run through the player id numbers
for(id in CommonTeamRosterFrame$PLAYER_ID){
        #name: to run through the list of the players names 
        name<-CommonTeamRosterFrame$PLAYER[CommonTeamRosterFrame$PLAYER_ID==id]
        #print player's name
        print(name)
        #c: to sighn the correlation result
        c<- cor(PlayerCareerStatsList_NEW_aggregate[[id]]$PTS , PlayerCareerStatsList_NEW_aggregate[[id]]$PLAYER_AGE)
        #print correlation
        print(c)
}



################################Russell Westbrook Average shots Attempt#######################
#check the variable type
mode(PlayerCareerStatsList_NEW_aggregate[["201566"]]$SEASON_ID)
#converted from character to numeric 
PlayerCareerStatsList_NEW_aggregate[["201566"]]$SEASON_ID <- as.numeric(as.character(PlayerCareerStatsList_NEW_aggregate[["201566"]]$SEASON_ID))

#Find the range of the variables SEASON_ID
xrange <- range(PlayerCareerStatsList_NEW_aggregate[["201566"]]$SEASON_ID)
# length of SEASON_ID for Russell
ln <- length(PlayerCareerStatsList_NEW_aggregate[["201566"]]$SEASON_ID)



####per 3- points
#AveForThree_Russell is a list to store the average 2-points shots attempt
AveForThree_Russell<- list()
#For loop to run through Russell's data 
for(i in 1:ln){
        #p is to store the avarage 2-points shots attempt
        p <- (3* PlayerCareerStatsList_NEW_aggregate[["201566"]]$FG3M[i])/ PlayerCareerStatsList_NEW_aggregate[["201566"]]$FG3A[i]
        #assign p value to v
        AveForThree_Russell[i] <- p
}# end of for loop

# The range for the avarage shots vector 
yrange_three <- range(AveForThree_Russell)


####per 2 points 

AveForTwo_Russell<- list()
for(i in 1:ln){
        p <- (2* PlayerCareerStatsList_NEW_aggregate[["201566"]]$FGM[i])/ PlayerCareerStatsList_NEW_aggregate[["201566"]]$FGA[i]
        AveForTwo_Russell[i] <- p
        print(p)
}

#The range of AveForTwo_Russell
yrange_two <- range(AveForTwo_Russell)

###############################Kevin Durant Average shots Attempt###################

####per 3 points 

# length of SEASON_ID for Kevin 
ln_kevin <- length(PlayerCareerStatsList_NEW_aggregate[["201142"]]$SEASON_ID)

#Create a list to store the avarage values 
AveForThree_Kevin<- list()

#For loop to count the avarage for 3-points shot attempts 
for(i in 1:ln_kevin){
        #p_k to store the avarage 
        p_k <- (3* PlayerCareerStatsList_NEW_aggregate[["201142"]]$FG3M[i])/ PlayerCareerStatsList_NEW_aggregate[["201142"]]$FG3A[i]
        #assign the avarage value to the vector
        AveForThree_Kevin[i] <- p_k
}#for loop ends 

#The range of the SEASON_ID
xrange_K <- range(PlayerCareerStatsList_NEW_aggregate[["201142"]]$SEASON_ID)

# the range of AveForThree_Kevin
yrange_threek <- range(AveForThree_Kevin)


####per 2 points 

#Create a list to store the avarage values for two-points shot attempt 
AveForTwo_Kevin<- list()

#For loop to count the avarage for 2-points shot attempts 
for(i in 1:ln_kevin){
        #count the avarage
        p <- (2* PlayerCareerStatsList_NEW_aggregate[["201142"]]$FGM[i])/ PlayerCareerStatsList_NEW_aggregate[["201142"]]$FGA[i]
        #assign it to the vector 
        AveForTwo_Kevin[i] <- p
}

# the range of AveForTwo_Kevin
yrange_twok <- range(AveForTwo_Kevin)



##########################Plots for avarage 3-points shot attempts########################## 
##  Open a new default device
get( getOption( "device" ) )()

##  Split the screen into two rows and one column, defining screens 1 and 2.
split.screen( figs = c( 2, 1 ) )


##  The first plot is located in screen 3:
screen( 1 )
#Kevin/ three points
# set up the plot 
plot(xrange_K, yrange_threek, type="n", xlab=" Season_ID (years)",
     ylab="Average spoints per 3-point-shots attempt" ) 

# add lines 
lines(PlayerCareerStatsList_NEW_aggregate[["201142"]]$SEASON_ID,AveForThree_Kevin, type="b") 

# add a title and subtitle 
title("Kevin Durant average spoints per 3-point-shots attempt ", "Plyer tracking shots")


##  The second plot is located in screen 4:
screen( 2 )
# Russell/three-point
# set up the plot 
plot(xrange, yrange_three, type="n", xlab=" Season_ID (years)",
     ylab="Avarage Shot attempt" ) 

# add lines 
lines(PlayerCareerStatsList_NEW_aggregate[["201566"]]$SEASON_ID, AveForThree_Russell, type="b") 
# add a title and subtitle 
title("Russell Westbrook Average shots Attempt ", "Plyer tracking shots")

##########################Plots for avarage 2-points shot attempts##########################
##  Open a new default device
get( getOption( "device" ) )()

##  Split the screen into two rows and one column, defining screens 1 and 2.

split.screen( figs = c( 2, 1 ) )

##  Split screen 2 into one row and two columns, defining screens 6 and 7.

#split.screen( figs = c( 1, 2 ), screen = 1 )


screen( 1 )
#Kevin/two points
# set up the plot 
plot(xrange_K, yrange_twok, type="n", xlab=" Season_ID (years)",
     ylab="Average spoints per 2-point-shots attempt" ) 

# add lines 
lines(PlayerCareerStatsList_NEW_aggregate[["201142"]]$SEASON_ID, AveForTwo_Kevin, type="b") 
# add a title and subtitle 
title("Kevin Durant average spoints per 2-point-shots attempt ", "Plyer tracking shots")

screen( 2 )
# Russell/two-point
# set up the plot 
plot(xrange, yrange_two, type="n", xlab=" Season_ID (years)",
     ylab="Average spoints per 2-point-shots attempt" ) 

# add lines 
lines(PlayerCareerStatsList_NEW_aggregate[["201566"]]$SEASON_ID, AveForTwo_Russell, type="b") 
# add a title and subtitle 
title("Russell Westbrook average spoints per 2-point-shots attempt ", "Plyer tracking shots")

PlayerCareerStatsList_NEW_aggregate[["201142"]]$SEASON_ID
AveForTwo_Russell
