####carefully enter input variables in lines 7,11,12, and 15-19
rm(list=ls())
library(plotly)
library(zoo)

####path to file location
filepath <- "path to file location (not to the file itself)"

####enter start and end dates in the following two####
####lines in the same format as this eg: 20 Apr 20####
start.date <- as.Date('6 Aug 18', format = '%d %b %y')
end.date <- as.Date('23 Aug 18', format = '%d %b %y')

####enter the input variables####
setwd("path where plots need to be saved") #enter path of destination output folder
monitor <- "Monitor20" #enter name of monitor file
modulo_tau <- 24
current.bin = 1 #this is the value of bins in which recording was done
desired.bin = 30 #this is the value of bins in which you would like to plot the actograms


#################################################
#################################################
nplot = 2
days <- as.numeric(end.date - start.date)
s_per_day <- (60/desired.bin)*modulo_tau

####read raw Monitor files, subset accordingly####
####and make plots################################
df <- read.delim(file = paste(filepath,monitor,".txt", sep = ""),
                 header = F)
colnames(df) <- c("x","date","time",seq(1, (length(df[1,])-3), by = 1))
df[,"date"] <- as.Date(df[,"date"], format = '%d %b %y')
d <- subset(df, date > start.date & date < end.date + 1)
raw <- subset(d,
              select = c("8","9","10","11","12","13","14","15","16","17","18",
                         "19","20","21","22","23","24","25","26","27","28","29",
                         "30","31","32","33","34","35","36","37","38","39")
)

ind <- seq(1, length(raw[,1]), by = desired.bin/current.bin)
binned <- matrix(NA, nrow = (s_per_day*days), ncol = length(raw[1,]))

for (i in 2:length(ind)){
  for (j in 1:length(raw[1,])){
    binned[1,j] <- raw[1,j]
    binned[i,j] <- sum(raw[ind[i-1]:(ind[i]-1),j])
  }
}

dummy <- matrix(0, nrow=s_per_day*(nplot-1), ncol=32)
data <- rbind(dummy, binned, dummy)

plots <- list()
p <- list()

f1 <- list(
  family = "Arial, sans-serif",
  size = 24,
  color = "black"
)
f2 <- list(
  family = "Arial, sans-serif",
  # size = 18,
  color = "black"
)

for (i in 1:length(data[1,])){
  a <- t(as.matrix(rollapply(data[,i],
                             width = s_per_day*nplot,
                             by = s_per_day,as.numeric)))
  for (j in 1:length(a[1,])){
    p[[j]] <- plot_ly(
      # x = seq(1, length(a[,j])),
      x = seq(0, ((length(a[,j])*(desired.bin/60))-(desired.bin/60)), by = desired.bin/60),
      y = a[,j]/max(a[,j]),
      type = "bar",
      fillcolor= "black",
      marker = list(
        color = "black",
        line = list(
          color = "black"
        )
      ),
      line = list(
        color = "black"
      )
      # mode = "lines",
      # fill = "tozeroy",
      # fillcolor = rgb(0,0,0,0.8),
      # line = list(color = rgb(0,0,0,1))
    )%>%
      layout(
        barmode = 'overlay',
        bargap = 0,
        yaxis = list(
          showticklabels = F,
          showline = T,
          showgrid = F,
          linecolor = "black"
        ),
        xaxis  = list(
          showgrid = F,
          showline = T,
          titlefont = f1,
          tickfont = f2,
          title = "",
          linecolor = "black",
          # linewidth = 4,
          # mirror = TRUE,
          autotick = FALSE,
          ticks = "outside",
          tick0 = 0,
          dtick = 12,
          ticklen = 7,
          tickcolor = toRGB("black"),
          # tickwidth = 4,
          range = c(0,49)
        )
      )
  }
  plots[[i]] <- subplot(
    p,
    nrows = length(a[1,]),
    shareX = T,
    margin = 0.0
  )%>%
    layout(
      yaxis = list(
        showticklabels = F,
        showline = T,
        showgrid = F,
        linecolor = "black"
      ),
      xaxis = list(
        showline = T,
        showgrid = F,
        titlefont = f1,
        tickfont = f2,
        title = "",
        linecolor = "black",
        # linewidth = 4,
        # mirror = TRUE,
        autotick = FALSE,
        ticks = "outside",
        tick0 = 0,
        dtick = 12,
        ticklen = 7,
        tickcolor = toRGB("black"),
        # tickwidth = 4,
        range = c(0,49)
      )
    )
}
ann1 <- list(x = -0.4, y = 0.65,
             text = "[1..8]", showarrow = F,
             textangle = -90,
             xref='paper', yref='paper',
             font = f1
)
ann2 <- list(x = -0.4, y = 0.55,
             text = "[9..16]", showarrow = F,
             textangle = -90,
             xref='paper', yref='paper',
             font = f1
)
ann3 <- list(x = -0.4, y = 0.5,
             text = "[17..24]", showarrow = F,
             textangle = -90,
             xref='paper', yref='paper',
             font = f1
)
ann4 <- list(x = -0.4, y = 0.3,
             text = "[25..32]", showarrow = F,
             textangle = -90,
             xref='paper', yref='paper',
             font = f1
)

plots[[1]] <- plots[[1]]%>%
  layout(
    annotations = ann1
  )
plots[[9]] <- plots[[9]]%>%
  layout(
    annotations = ann2
  )
plots[[17]] <- plots[[17]]%>%
  layout(
    annotations = ann3
  )
plots[[25]] <- plots[[25]]%>%
  layout(
    annotations = ann4
  )

final <- subplot(
  plots,
  nrows = 4
)%>%
  layout(showlegend = F,
         autosize = F,
         height = 900,
         width = 1300,
         yaxis = list(
           showticklabels = F,
           showgrid = F,
           showline = T,
           linecolor = "black"
         ),
         xaxis = list(
           showgrid = F,
           showline = T,
           titlefont = f1,
           tickfont = f2,
           title = "",
           linecolor = "black",
           # linewidth = 4,
           # mirror = TRUE,
           autotick = FALSE,
           ticks = "outside",
           tick0 = 0,
           dtick = 12,
           ticklen = 7,
           tickcolor = toRGB("black"),
           # tickwidth = 4,
           range = c(0,49)
         )
  )
orca(final, paste(monitor,".pdf", sep = ""))
