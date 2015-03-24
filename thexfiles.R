# R Code by Christophe Cariou, March 2015
# Visualization : Binge-Watching Guide: The X-Files
# Without the legend
# http://chcariou.fr/post/113771580897


# Libraries
	
	library(scales)
	library(extrafont)

# Font

	# Family font by default : Open Sans
	# http://www.google.com/fonts/specimen/Open+Sans

	font_viz <- "Open Sans"

	# Family font for title of viz : TeX Gyre Schola
	# http://www.fontsquirrel.com/fonts/TeX-Gyre-Schola

	font_title <- "TeX Gyre Schola"
	
	# Family font for the bug X : The X-Files
	# http://www.dafont.com/the-x-files.font

	font_X <- "The X-Files"


# Data for the visualization

	xfiles <- read.table("http://lab.chcariou.fr/data/data_thexfiles.csv",header=TRUE,sep=";")
	head(xfiles)
	dim(xfiles)	

	xfiles <- xfiles[order(xfiles$serie,decreasing=FALSE),]


# IMDb Ratings : find episodes with higher ratings > median + define two equivalents groups (50 and 51)

	xfiles$Rating_Q <- 0

	rating_75 <- quantile(xfiles$Rating,0.75)
	id <- subset(xfiles,Rating>rating_75)[,1]
	xfiles$Rating_Q[id] <- 1

	rating_50 <- quantile(xfiles$Rating,0.5)
	id <- subset(xfiles,Rating>=rating_50 & Rating<=rating_75)[,1]
	xfiles$Rating_Q[id] <- 2


# Angle by Episode > Trigo : cosinus and sinus

	n <- dim(xfiles)[1]
	r <- 16
	angle <- 360/n

	xfiles$Angle_X <- cos(angle*xfiles[,1]/180*pi)
	xfiles$Angle_Y <- sin(angle*xfiles[,1]/180*pi)

	# Adjust Text Angle by season

	xfiles$Episode_A <- angle*xfiles[,1]
	id <- subset(xfiles[,1:2],season>2 & season<8)
	id$Episode_A <- angle*id[,1]+180
	xfiles$Episode_A[id$serie] <- id$Episode_A

	
# Page for visualisation

	col_bg <- "#FEFEFE"

	dev.off()
	quartz(width=14,height=14,bg=col_bg)
	par(mar=c(0,0,0,0),oma=c(0,0,0,0),family=font_viz)
	plot(0,0,type="n",asp=1,xlim=c(-30,30),axes=FALSE,xlab=NA,ylab=NA,xaxs="i",yaxs="i")


# Titles : get width

	xfiles$Episode_W <- strwidth(xfiles$Episode,family="Open Sans",cex=0.5)

# Layer 1 : Taglines / Opening sequences : needles > texts

	col_g <- "#000000"

	towatch <- subset(xfiles,Tagline!="")
	ntowatch <- dim(towatch)[1]
	towatch$tag <- seq(1, ntowatch,1)

	text_width <- strwidth(towatch$Tagline,family="Open Sans",cex=0.5)/1.9
	text_height <- strheight(towatch$Tagline,family="Open Sans",cex=0.5)/1.9

	x1towatch <- (r+11-text_width)*towatch$Angle_X
	y1towatch <- (r+11-text_width)*towatch$Angle_Y
	
	x2towatch <- (r+6)*towatch$Angle_X
	y2towatch <- (r+6)*towatch$Angle_Y
	id <- subset(towatch,Rating_Q==0)$tag
	x2towatch[id] <- (r+towatch$Episode_W[id]/1.9)*towatch$Angle_X[id]
	y2towatch[id] <- (r+towatch$Episode_W[id]/1.9)*towatch$Angle_Y[id]

	segments(x1towatch, y1towatch, x2towatch, y2towatch,col=col_g,lwd=0.5)
	
	txtowatch <- (r+11)*towatch$Angle_X
	tytowatch <- (r+11)*towatch$Angle_Y

	# Loop because of srt in text function
	ntowatch <- dim(towatch)[1]
	for (i in 1: ntowatch) {
		text(txtowatch[i], tytowatch[i],towatch$Tagline[i],srt=towatch$Episode_A[i],cex=0.5,col=col_g)
	}
	
# Layer 2 : IMDb ratings

	col_g <- "#C82C37"

	towatch <- subset(xfiles,Rating_Q==1)
	x1towatch <- (r+6)*towatch$Angle_X
	y1towatch <- (r+6)*towatch$Angle_Y
	x2towatch <- (r+towatch$Episode_W/1.9)*towatch$Angle_X
	y2towatch <- (r+towatch$Episode_W/1.9)*towatch$Angle_Y
	segments(x1towatch, y1towatch, x2towatch, y2towatch,col=col_g,lwd=0.75)
	points(x1towatch, y1towatch,pch=21,bg="#fefefe",col=col_g,lwd=3)

	towatch <- subset(xfiles,Rating_Q==2)
	x1towatch <- (r+6)*towatch$Angle_X
	y1towatch <- (r+6)*towatch$Angle_Y
	x2towatch <- (r+towatch$Episode_W/1.9)*towatch$Angle_X
	y2towatch <- (r+towatch$Episode_W/1.9)*towatch$Angle_Y
	segments(x1towatch, y1towatch, x2towatch, y2towatch,col=col_g,lwd=0.5,lend="square",lty="dotted")
	points(x1towatch, y1towatch,pch=21,bg="#fefefe",col=col_g,lwd=1)


# Layer 3 : Episodes' Titles + Mythology

	col_g <- "#000000"

	towatch <- subset(xfiles,is.na(Mythology))
	txtowatch <- r*towatch$Angle_X
	tytowatch <- r*towatch$Angle_Y

	x1towatch <- (r-towatch$Episode_W/2)*towatch$Angle_X
	y1towatch <- (r-towatch$Episode_W/2)*towatch$Angle_Y
	x2towatch <- (r+towatch$Episode_W/2)*towatch$Angle_X
	y2towatch <- (r+towatch$Episode_W/2)*towatch$Angle_Y

	segments(x1towatch, y1towatch, x2towatch, y2towatch,col=col_bg,lwd=10)

	# Loop because of srt in text function
	ntowatch <- dim(towatch)[1]
	for (i in 1: ntowatch) {
		text(txtowatch[i], tytowatch[i],towatch$Episode[i],srt=towatch$Episode_A[i],cex=0.5,col=col_g)
	}


	towatch <- subset(xfiles,!is.na(Mythology))
	txtowatch <- r*towatch$Angle_X
	tytowatch <- r*towatch$Angle_Y

	x1towatch <- (r-towatch$Episode_W/2)*towatch$Angle_X
	y1towatch <- (r-towatch$Episode_W/2)*towatch$Angle_Y
	x2towatch <- (r+towatch$Episode_W/2)*towatch$Angle_X
	y2towatch <- (r+towatch$Episode_W/2)*towatch$Angle_Y

	segments(x1towatch, y1towatch, x2towatch, y2towatch,col=col_g,lwd=10)

	# Loop because of srt in text function
	ntowatch <- dim(towatch)[1]
	for (i in 1: ntowatch) {
		text(txtowatch[i], tytowatch[i],towatch$Episode[i],srt=towatch$Episode_A[i],cex=0.5,col=col_bg)
	}


# Layer 4.1 : Grid for Episodes

	col_g <- "#000000"

	tofollow <- subset(xfiles,episode!=1)
	x1tofollow <- (r-6.2)*tofollow$Angle_X
	y1tofollow <- (r-6.2)*tofollow$Angle_Y
	x2tofollow <- (r-tofollow$Episode_W/2.1)*tofollow$Angle_X
	y2tofollow <- (r-tofollow$Episode_W/2.1)*tofollow$Angle_Y
	segments(x1tofollow, y1tofollow, x2tofollow, y2tofollow,col=col_g,lwd=0.3,lty="dotted")
	
	Episode_N <- paste("0",xfiles$episode,sep="")
	id <- subset(xfiles,episode>9)[,1]
	Episode_N[id] <- as.character(xfiles$episode[id])
	
	Episode_NX <- (r-6.5)*xfiles$Angle_X
	Episode_NY <- (r-6.5)*xfiles$Angle_Y

	# Loop because of srt in text function
	for (i in 1:n) {
		text(Episode_NX[i],Episode_NY[i],Episode_N[i],srt=xfiles$Episode_A[i],cex=0.5,col=col_g)

	}

	
# Layer 4.2 : Grid for Seasons

	col_g <- "#000000"

	tofollow <- subset(xfiles,episode==1)
	x1tofollow <- (r-6.2)*tofollow$Angle_X
	y1tofollow <- (r-6.2)*tofollow$Angle_Y
	x2tofollow <- (r-tofollow$Episode_W/2.1)*tofollow$Angle_X
	y2tofollow <- (r-tofollow$Episode_W/2.1)*tofollow$Angle_Y
	segments(x1tofollow, y1tofollow, x2tofollow, y2tofollow,col=col_g,lwd=0.3,lty=1)
	
	x1tofollow <- (r-6.8)*tofollow$Angle_X
	y1tofollow <- (r-6.8)*tofollow$Angle_Y
	x2tofollow <- (r-10)*tofollow$Angle_X
	y2tofollow <- (r-10)*tofollow$Angle_Y
	segments(x1tofollow, y1tofollow, x2tofollow, y2tofollow,col=col_g,lwd=0.3,lty=1)

	midtofollow <- aggregate(xfiles$episode, by=list(xfiles$season),FUN=mean)[,2]+tofollow$serie
	x1tofollow <- (r-9)*cos(angle*midtofollow/180*pi)
	y1tofollow <- (r-9)*sin(angle*midtofollow/180*pi)

	# Loop because of srt in text function
	ntofollow <- dim(tofollow)[1]
	for (i in 1: ntofollow) {
		text(x1tofollow[i], y1tofollow[i],paste("S0",i,sep=""),srt=0,cex=0.75,col=col_g)
	}


# Layer 5 : width of 0.5 and space of 0.2

# Layer 5.1 : Episodes with The Smoking Man

	col_g <- "#000000"

	towatch <- subset(xfiles,!is.na(The_Smoking_Man))
	x1towatch <- (r-4)*towatch$Angle_X
	y1towatch <- (r-4)*towatch$Angle_Y
	x2towatch <- (r-3.5)*towatch$Angle_X
	y2towatch <- (r-3.5)*towatch$Angle_Y
	segments(x1towatch, y1towatch, x2towatch, y2towatch,col=col_g,lwd=3)

# Layer 5.2 : Episodes with The Lone Gunmen

	col_g <- "#51A7F9"

	towatch <- subset(xfiles,!is.na(The_Lone_Gunmen))
	x1towatch <- (r-4.7)*towatch$Angle_X
	y1towatch <- (r-4.7)*towatch$Angle_Y
	x2towatch <- (r-4.2)*towatch$Angle_X
	y2towatch <- (r-4.2)*towatch$Angle_Y
	segments(x1towatch, y1towatch, x2towatch, y2towatch,col=col_g,lwd=3)

# Layer 5.3 : Episodes without the duo Fox Mulder & Dana Scully

	col_g <- "#F5D328"

	towatch <- subset(xfiles,!is.na(WO_Mulder_Scully))
	x1towatch <- (r-5.4)*towatch$Angle_X
	y1towatch <- (r-5.4)*towatch$Angle_Y
	x2towatch <- (r-4.9)*towatch$Angle_X
	y2towatch <- (r-4.9)*towatch$Angle_Y
	segments(x1towatch, y1towatch, x2towatch, y2towatch,col=col_g,lwd=3)

#  Layer 5.4 : Episodes with Vince Gilligan as writer

	col_g <- "#4EB28B"

	towatch <- subset(xfiles,!is.na(Vince_Gilligan))
	x1towatch <- (r-5.6)*towatch$Angle_X
	y1towatch <- (r-5.6)*towatch$Angle_Y
	x2towatch <- (r-6.1)*towatch$Angle_X
	y2towatch <- (r-6.1)*towatch$Angle_Y
	segments(x1towatch, y1towatch, x2towatch, y2towatch,col=col_g,lwd=3)


#  Layer 5.5 : Episodes from Wired' Binge Watching

	# Can skip
	col_g <- "#D5D4C4"

	towatch <- subset(xfiles,!is.na(Wired) & Wired<0)
	x1towatch <- (r-7.4)*towatch$Angle_X
	y1towatch <- (r-7.4)*towatch$Angle_Y
	x2towatch <- (r-6.9)*towatch$Angle_X
	y2towatch <- (r-6.9)*towatch$Angle_Y
	segments(x1towatch, y1towatch, x2towatch, y2towatch,col=col_g,lwd=3)

	# Can't skip
	col_g <- "#727167"

	towatch <- subset(xfiles,!is.na(Wired) & Wired>0)
	x1towatch <- (r-7.4)*towatch$Angle_X
	y1towatch <- (r-7.4)*towatch$Angle_Y
	x2towatch <- (r-6.9)*towatch$Angle_X
	y2towatch <- (r-6.9)*towatch$Angle_Y
	segments(x1towatch, y1towatch, x2towatch, y2towatch,col=col_g,lwd=3)

# Layer 6.1 : X in background
# Code # for the good X !

	text(0,0,"#",cex=12,family="The X-Files",font=1,col=alpha("#A2A296",0.3))

# Layer 6.2 : Title
	
	text(0,2.5, "The X-Files",cex=2,family=font_title,font=2)


# Layer 6.3 : Resume

	S <- paste(max(xfiles$season),"seasons",sep=" ")
	text(0,0,S,cex=0.7,family=font_title)

	E <- paste(max(xfiles$serie)+1,"episodes",sep=" ")
	text(0,-1,E,cex=0.7,family=font_title)

	Days_n <- round((max(xfiles$serie)+1)*43/60/24,0)
	D <- paste(Days_n,"days w/o break",sep=" ")
	text(0,-2,D,cex=0.7,family=font_title)


# Save in pdf for high-res
	
