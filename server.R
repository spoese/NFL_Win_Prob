library(shiny)
library(httr)
library(XML)
library(lubridate)
library(ggplot2)
shinyServer(function(input, output) {
        
        #All Pro Football Reference boxscore tables start with this url
        games.URL.stem = "http://www.pro-football-reference.com/years/"
        
        scores <- reactive({
                first <- input$start
                last <- input$end
                
                #Grab boxscores for every game between the desired years
                for (year in first:last)
                {
                        #Create url for year
                        URL = paste(games.URL.stem, year, "/games.htm", sep="")
                        
                        #Get boxscores
                        games <- GET(URL)
                        games <- readHTMLTable(rawToChar(games$content), 
                                               stringsAsFactors = F)
                        dfThisSeason = games[[1]]
                        
                        #Clean up table
                        names(dfThisSeason)[c(9,10)] <- c("PtsW","PtsL")
                        dfThisSeason = subset(dfThisSeason, Week!="Week")
                        dfThisSeason = subset(dfThisSeason, Week!="")
                        dfThisSeason$Date = as.character(dfThisSeason$Date)
                        dfThisSeason$GameDate = mdy(paste(dfThisSeason$Date, year))
                        year(dfThisSeason$GameDate) = with(dfThisSeason, 
                                                           ifelse(month(GameDate) <=6, year(GameDate)+1, year(GameDate)))
                        
                        #Create cumulative list of boxscores
                        if (year == first)
                        {
                                dfAllSeasons = dfThisSeason
                        } else {
                                dfAllSeasons = rbind(dfAllSeasons, dfThisSeason)
                        }
                        
                }
                
                dfAllSeasons = dfAllSeasons[,c(15, 1, 5, 7, 9, 10, 11, 13, 12, 14)]
                colnames(dfAllSeasons) = c("GameDate", "Week", "Winner", "Loser", "WinnerPoints", "LoserPoints", "WinnerYards", "LoserYards", "WinnerTO", "LoserTO")
                
                #Convert factors into numerics
                dfAllSeasons$Winner = as.character(dfAllSeasons$Winner)
                dfAllSeasons$Loser = as.character(dfAllSeasons$Loser)
                dfAllSeasons$WinnerPoints = as.integer(as.character(dfAllSeasons$WinnerPoints))
                dfAllSeasons$LoserPoints = as.integer(as.character(dfAllSeasons$LoserPoints))
                dfAllSeasons$WinnerYards = as.integer(as.character(dfAllSeasons$WinnerYards))
                dfAllSeasons$LoserYards = as.integer(as.character(dfAllSeasons$LoserYards))
                dfAllSeasons$WinnerTO = as.integer(as.character(dfAllSeasons$WinnerTO))
                dfAllSeasons$LoserTO = as.integer(as.character(dfAllSeasons$LoserTO))
                
                return(dfAllSeasons)
        })
        
        output$plot1 <- renderPlot({
                #Make desired plots
                if(input$stat=="Points"){
                        p <- qplot(scores()$WinnerPoints,
                                   main = paste("Histogram of Points Scored by Winning Team (",
                                                input$start,"-",input$end,")",sep=""),
                                   xlab = "Points Scored",
                                   fill=I("blue"),col=I("black"))
                        print(p)}
                else if(input$stat=="Yards"){
                        p <- qplot(scores()$WinnerYards,
                                   main = paste("Histogram of Yards Gained by Winning Team (",
                                                input$start,"-",input$end,")",sep=""),
                                   xlab = "Yards Gained",
                                   fill=I("green"),col=I("black"))
                        print(p)}
                else if(input$stat=="Turnovers"){
                        p <- qplot(scores()$WinnerTO,
                                   main = paste("Histogram of Turnovers Committed by Winning Team (",
                                                input$start,"-",input$end,")",sep=""),
                                   xlab = "Turnovers",
                                   fill=I("red"),col=I("black"))
                        print(p)}
        })
        
        output$plot2 <- renderPlot({
                if (input$stat == "Points"){
                        #Create table showing win percentage at each point value
                        winnerTable <- plyr::count(scores(),vars="WinnerPoints")
                        loserTable <- plyr::count(scores(),vars="LoserPoints")
                        minScore <- with(scores(),min(c(WinnerPoints,LoserPoints)))
                        maxScore <- with(scores(),max(c(WinnerPoints,LoserPoints)))
                        propTable <- data.frame(Points = 1:(maxScore+1), PctWin = 1:(maxScore+1))
                        #Control for 0's in the denominator
                        for (i in minScore:maxScore) {
                                propTable$Points[i+1] <- i
                                ifelse(sum(winnerTable$WinnerPoints==i)>0,
                                       numWin <- winnerTable[which(winnerTable$WinnerPoints==i),]$freq,
                                       numWin <- 0)
                                ifelse(sum(loserTable$LoserPoints==i)>0,
                                       numLoss <- loserTable[which(loserTable$LoserPoints==i),]$freq,
                                       numLoss <- 0)
                                ifelse(numWin+numLoss==0,propWin <- NA,propWin <- numWin/(numWin+numLoss))
                                propTable$PctWin[i+1] <- propWin
                        }
                        #Create a logistic model
                        model <- glm(PctWin ~ Points, data = propTable, family=binomial)
                        glm.probs <- predict(model,propTable)
                        glm.probs <- exp(glm.probs)/(1+exp(glm.probs))
                        forgg <- cbind(propTable,fit=glm.probs)
                        #Create plot
                        p <- ggplot(forgg,aes(Points,PctWin)) +
                                geom_point() +
                                geom_line(aes(Points,fit),col="blue") +
                                labs(title="Win Percentage vs. Points Scored",
                                     x="Points Scored",y="Win Percentage")
                        p
                }
                else if (input$stat == "Yards"){
                        #Create table showing win percentage for different
                        #intervals of yards.
                        winYds <- scores()$WinnerYards
                        loseYds <- scores()$LoserYards
                        minYds <- with(scores(),min(c(WinnerYards,LoserYards)))
                        maxYds <- with(scores(),max(c(WinnerYards,LoserYards)))
                        cuts <- seq(minYds,maxYds,by=(maxYds-minYds)/25)
                        cutNames <- 1:25
                        for (i in 1:(length(cuts)-1)) {
                                cutNames[i] <- mean(c(cuts[i],cuts[i+1]))
                        }
                        winYds <- table(cut2(winYds,cuts=cuts))
                        loseYds <- table(cut2(loseYds,cuts=cuts))
                        propYds <- winYds/(winYds+loseYds)
                        names(propYds) <- cutNames
                        pctYds <- data.frame(Yards = as.numeric(names(propYds)),PctWin = as.vector(propYds))
                        #Create a logistic model
                        modelYds <- glm(PctWin ~ Yards, data = pctYds, family = binomial)
                        glm.yards <- predict(modelYds,pctYds)
                        glm.yards <- exp(glm.yards)/(1+exp(glm.yards))
                        forgg <- cbind(pctYds,fit=glm.yards)
                        #Create plot
                        p <- ggplot(forgg,aes(Yards,PctWin)) +
                                geom_point() +
                                geom_line(aes(Yards,fit),col="green") +
                                labs(title="Win Percentage vs. Yards Gained",
                                     x="Yards Gained",y="Win Percentage")
                        p
                }
                else if (input$stat == "Turnovers"){
                        #Create table showing win percentage at each number of
                        #turnovers.
                        winnerTable <- plyr::count(scores(),vars="WinnerTO")
                        for (i in (dim(winnerTable)[1]+1):13){
                                winnerTable[i,]<-c(i-1,0)
                        }
                        loserTable <- plyr::count(scores(),vars="LoserTO")
                        for (i in (dim(loserTable)[1]+1):13){
                                loserTable[i,]<-c(i-1,0)
                        }
                        propTable <- as.data.frame(
                                cbind(winnerTable[,1],
                                      winnerTable[,2]/(winnerTable[,2]+loserTable[,2])))
                        names(propTable) <- c("TO","PctWin")
                        propTableMod <- propTable[which(propTable$PctWin>0),]
                        #Create an exponential model.
                        model <- lm(log(PctWin) ~ TO,data=propTableMod)
                        lm.probs <- predict(model,newdata=propTable)
                        forgg <- cbind(propTable,fit=lm.probs)
                        #Create plot.
                        p <- ggplot(forgg,aes(TO,PctWin)) +
                                geom_point() +
                                geom_line(aes(TO,exp(fit)),col="red") +
                                labs(title="Win Percentage vs. No. of Turnovers",
                                     x="No. of Turnovers",y="Win")
                        p
                }
        })
})