#' @title Summary Statistics
#' @description Uses the age data from the read function and performs appropriate
#' analysis and generates graphics associated with the data.
#' @aliases stat
#' @aliases summary
#' @param type The kind of analysis to be done. The possible options are \code{summary}, \code{timeplot}, \code{histogram}, \code{fullhist}, \code{comparison} and \code{fullcomp}.
#'    \code{summary} displays a 5 number summary of ages accross all years. \code{timeplot} displays the averages and second and fourth quartiles of ages for all the years. \code{histogram}
#'    shows the histogram of ages for the latest year. \code{fullhist} shows the histograms for all the different years for which the data was colected. \code{comparison} shows the density
#'    plots with a four year interval to inspect the change in distribution. \code{fullcomp} shows the same comparison for all the different years.
#' @return Results from the kind of analysis specified by \code{type}.
#' @usage
#' statsummary(type)

#' @import ggplot2
#' @export
statsummary <- function(type){

  #load all age data in list
  age <- list("2001"=readBAyear("list_2001.txt", 2001, 2001, 1940)[,2], "2002"=readBAyear("list_2002.txt", 2002, 2002, 1940)[,2],"2003"=readBAyear("list_2003.txt", 2003, 2003, 1940)[,2],"2004"=readBAyear("list_2004.txt", 2004, 2004, 1940)[,2], "2005"=readBAyear("list_2005.txt", 2005, 2005, 1940)[,2],"2006"=readBAyear("list_2006.txt", 2006, 2006, 1940)[,2],"2007"=readBAyear("list_2007.txt", 2007, 2007, 1940)[,2], "2008"=readBAyear("list_2008.txt", 2008, 2008, 1940)[,2],"2009"=readBAyear("list_2009.txt", 2009, 2009, 1940)[,2],"2010"=readBAyear("list_2010.txt", 2010, 2010, 1940)[,2], "2011"=readBAyear("list_2011.txt", 2011, 2011, 1940)[,2],"2012"=readBAyear("list_2012.txt", 2012, 2011, 1940)[,2],"2013"=readBAyear("list_2013.txt", 2013, 2011, 1940)[,2],"2014"=readBAyear("list_2014.txt", 2014, 2011, 1940)[,2])

  #Create matrix with the summary data
  stat<-matrix(nrow=14, ncol=7)
  for(i in 1:14){
    for(j in 1:5){
      stat[i,j] <- quantile(age[[i]])[j]
    }
    stat[i,6] <- mean(age[[i]])
    stat[i,7] <- sd(age[[i]])
  }


  #Construct dataframe with all the summary information
  output_1 <- data.frame(Year=2001:2014, Mean = stat[,6], SD = stat[,7], Minimum=stat[,1], Second = stat[,2], Median=stat[,3], Fourth=stat[,4], Maximum=stat[,5])

  #Create dataframe with raw data
  dat<-matrix(nrow=389, ncol=14)
  for(i in 1:14){
    dat[1:length(age[[i]]),i]<-age[[i]]
    dat<-as.data.frame(dat)
    names(dat)<-c("2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")
  }

  if(type=="summary"){
    output_1
  }

  else if(type=="timeplot"){
    #Construct a timeplot
    ggplot(data = output_1)+geom_point(aes(x = Year, y = Mean, col="Mean"), size = 5, alpha=0.7)+
      geom_smooth(aes(x = Year, y = Mean))+
      geom_point(aes(x = Year, y = Second, col="Second Quartile"), alpha = 0.7, shape = 2)+
      geom_point(aes(x = Year, y = Fourth, col= "Fourth Quartile"), alpha = 0.7, shape = 2)+
      ylab("Age")+ggtitle("Timeplot of Mean, Second and Fourth Quantile of Ages")+
      scale_color_discrete(name="Legend")
  }

  else if(type=="histogram"){
    ggplot(dat, aes(x=`2014`))+geom_histogram(aes(y=..density..), binwidth = 4, fill = "orange", alpha = 0.7, col = "black")+
      geom_density(alpha=0.4, fill="#FF6666")+labs(y = "Density", x = "Age", title = "Estimated Ages of Williams College Faculty in 2014")
  }

  else if(type=="fullhist"){
    par(mfrow=c(3,4))
    hist(dat$`2001`, main="2001",xlab="", ylab="")
    hist(dat$`2002`, main="2002",xlab="", ylab="")
    hist(dat$`2003`, main="2003",xlab="", ylab="")
    hist(dat$`2004`, main="2004",xlab="", ylab="")
    hist(dat$`2005`, main="2005",xlab="", ylab="Frequency of Ages")
    hist(dat$`2006`, main="2006",xlab="", ylab="")
    hist(dat$`2007`, main="2007",xlab="", ylab="")
    hist(dat$`2008`, main="2008",xlab="", ylab="")
    hist(dat$`2009`, main="2009",xlab="", ylab="")
    hist(dat$`2010`, main="2010",xlab="Ages of Faculty", ylab="")
    hist(dat$`2011`, main="2011",xlab="", ylab="")
    hist(dat$`2012`, main="2012",xlab="", ylab="")
    par(mfrow=c(1,1))
  }

  else if(type=="comparison"){
    ggplot(dat)+geom_density(aes(x=`2001`, fill="2001"), alpha=0.5)+
      geom_density(aes(x=`2005`, fill="2005"), alpha=0.5)+
      geom_density(aes(x=`2009`, fill="2009"), alpha=0.5)+
      geom_density(aes(x=`2014`, fill="2014"), alpha=0.5)+
      scale_fill_discrete(name="Year")+
      labs(x="Age", y="Density", title="Change in Age Distribution accross Years")
  }

  else if(type=="fullcomp"){
    ggplot(dat)+geom_density(aes(x=`2001`, fill="2001"), alpha=0.4)+geom_density(aes(x=`2002`, fill="2002"), alpha=0.4)+
      geom_density(aes(x=`2003`, fill="2003"), alpha=0.4)+geom_density(aes(x=`2004`, fill="2004"), alpha=0.4)+
      geom_density(aes(x=`2005`, fill="2005"), alpha=0.4)+geom_density(aes(x=`2006`, fill="2006"), alpha=0.4)+
      geom_density(aes(x=`2007`, fill="2007"), alpha=0.4)+geom_density(aes(x=`2008`, fill="2008"), alpha=0.4)+
      geom_density(aes(x=`2009`, fill="2009"), alpha=0.4)+geom_density(aes(x=`2010`, fill="2010"), alpha=0.4)+
      geom_density(aes(x=`2011`, fill="2011"), alpha=0.4)+geom_density(aes(x=`2012`, fill="2012"), alpha=0.4)+
      geom_density(aes(x=`2013`, fill="2013"), alpha=0.4)+geom_density(aes(x=`2014`, fill="2014"), alpha=0.4)+
      scale_fill_discrete(name="Year")+
      labs(x="Age", y="Density", title="Change in Age Distribution accross Years")
  }

  else{
    warning("The specified type does not exist. Enter ?statsummary or help(statsummary) to view all the summary options available.")
  }
}
