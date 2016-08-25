##I saw a few events in the news, Hillary often wakes up to horrible events that happened overnight
## like Orlando, rather than being waken up by them. I was wondering if the data of her released emails
## could give insight to her sleep schedule. Data obtained from Kaggle.com

##Read Data
emails <- read.csv("~/Hildog/output/Emails.csv")
aliases <- read.csv("~/Hildog/output/aliases.csv")
emailreceivers <- read.csv("~/Hildog/output/EmailReceivers.csv")
persons <- read.csv("~/Hildog/output/Persons.csv")

##Preparing date and ordering chronologically
emails$ExtractedDateSent <- strptime(emails$ExtractedDateSent, format = "%A, %B %d, %Y %I:%M %p")
emails <- emails[order(emails$ExtractedDateSent),]

##Looking at hours
times <- emails$ExtractedDateSent[which(emails$MetadataTo != "H")]
hframe <- as.data.frame(table(times$hour))

##Plotting function, frequencies fill in a "wedge" of the clock.
clock.plot <- function (x, col = rainbow(n), xlab, ...) {
  if( min(x)<0 ) {x <- x - min(x)}
  if( max(x)>1 ) {x <- x/max(x)}
  n <- length(x)
  if(is.null(names(x))) names(x) <- 0:(n-1)
  m <- 1.05
  plot(0, type = 'n', xlim = c(-m,m), ylim = c(-m,m), axes = F, xlab = xlab, ylab = '', ...)
  a <- pi/2 - 2*pi/200*0:200
  polygon( cos(a), sin(a) )
  v <- .02
  a <- pi/2 - 2*pi/n*0:n
  segments( (1+v)*cos(a), (1+v)*sin(a), (1-v)*cos(a), (1-v)*sin(a) )
  segments( cos(a), sin(a),0, 0, col = 'light grey', lty = 3) 
  ca <- -2*pi/n*(0:50)/50
  for (i in 1:n) {
    a <- pi/2 - 2*pi/n*(i-1)
    b <- pi/2 - 2*pi/n*i
    polygon( c(0, x[i]*cos(a+ca), 0), c(0, x[i]*sin(a+ca), 0), col=col[i] )
    v <- .1
    text((1+v)*cos(a), (1+v)*sin(a), names(x)[i])
  }
}

##Creating Clock Plot
clock.plot(hframe$Freq)

##Clock Plot, but per year (in and out)
tframe <- as.data.frame(cbind(times$year, times$hour))
colnames(tframe) <- c("year", "hour")
yearplots <- table(tframe)

layout(matrix(0,nrow = 3, ncol = 2)))
for (i in 1:6) {
  if (i != 3) {clock.plot(yearplots[i,])}
}

#More to do?
#rate of reply
#emails in vs emails out 