###############################
#
# Program to analyze Yelp
# dataset
#
##############################

require(parallel)
require(qvalue)
require(ggplot2)
require(combinat)
# Set these parameters to 1 for single threaded analysis (slower)
# or the number of threads available (faster)
options(mc.cores=15)
options(cores=15)

setwd("~/path/to/yelp/")

########## Section 1: Service hurts but doesn't help

##### Part 1: Load in the data

# business, user, swscore (negative if no service word)
worddatain = read.table("/path/to/yelp_dataset_challenge_academic_dataset/finalSet/restReviewsSW3.txt",header=FALSE,stringsAsFactors=FALSE)
colnames(worddatain) = c("business","user","swscore")
worddata = worddatain
worddatares = lm(abs(worddatain[,3])~worddatain[,4])$residuals
worddata[,3]=(worddatares+min(worddatares)*sign(min(worddatares)))*sign(worddatain[,3])
busmetadata = read.table("/path/to/yelp_dataset_challenge_academic_dataset/finalSet/bizAttributesNew.txt",header=FALSE,stringsAsFactors=FALSE,sep="\t")
colnames(busmetadata) = c("business","allswscores","cats","dollars","state","city")
busnamedata = read.table("/path/to/yelp_dataset_challenge_academic_dataset/finalSet/code2name.txt",header=FALSE,stringsAsFactors=FALSE,sep="\t",quote="",comment.char="")

##### Part 2: Perform analysis

# Add columns for score and presence of service word
hasword = (worddata$swscore>0)
# Having a service word is a 1 not having is a 0
worddatan = data.frame(worddata,score=abs(worddata$swscore),sw=as.numeric(hasword))

## Look at each business

# Average score for each business, stratified by having service word
busavgscoresw = aggregate(score ~ business + sw, data=worddatan, mean)
# Number of samples for each business, stratified by having service word
busnumsampsw = aggregate(score ~ business + sw, data=worddatan, length)
# Subset list to businesses that have at least 10 reviews with and without service words
busnumsampswthresh = busnumsampsw[(busnumsampsw$score>=10),]
busnumsampswthreshnames = busnumsampswthresh[duplicated(busnumsampswthresh$business),"business"]
# Perform paired t-test for difference in means in reviews with and without service words across all businesses
busnumsampswthreshwsw = busnumsampswthresh[busnumsampswthresh$sw==1,]
busnumsampswthreshwosw = busnumsampswthresh[busnumsampswthresh$sw==0,]
busavgscoreswwsw = busavgscoresw[busavgscoresw$sw==1,]
busavgscoreswwosw = busavgscoresw[busavgscoresw$sw==0,]
buswsw = merge(busnumsampswthreshwsw,busavgscoreswwsw,by.x="business",by.y="business")
colnames(buswsw)[c(3,5)] = c("sampwsw","scorewsw")
buswosw = merge(busnumsampswthreshwosw,busavgscoreswwosw,by.x="business",by.y="business")
colnames(buswosw)[c(3,5)] = c("sampwosw","scorewosw")
busall = merge(buswsw,buswosw,by.x="business",by.y="business")
pairedttest = t.test(x=busall$scorewsw,busall$scorewosw,alternative="two.sided",paired=TRUE)
print(pairedttest$p.value); print(mean(busall$scorewsw)); print(mean(busall$scorewosw))
# Perform two sample t-tests for difference in means in reviews with and without service words for each business individually
allbusttest <- function(x) {
	curname = busnumsampswthreshnames[x]
	matchnames = worddatan[worddatan$business==curname,]
	pvalue = t.test(matchnames[matchnames$sw==0,"score"],matchnames[matchnames$sw==1,"score"],alternative="two.sided")$p.value
	return(pvalue)
}
allbusttestres = unlist(mclapply(1:length(busnumsampswthreshnames),allbusttest))
allbusttestqvals = qvalue(allbusttestres)$qvalues
bussigtests = (allbusttestqvals < 0.05)

## Look at each individual

# Average score for each user, stratified by having service word
useravgscoresw = aggregate(score ~ user + sw, data=worddatan, mean)
# Number of samples for each user, stratified by having service word
usernumsampsw = aggregate(score ~ user + sw, data=worddatan, length)
# Subset list to users that have at least 10 reviews with and without service words
usernumsampswthresh = usernumsampsw[(usernumsampsw$score>=10),]
usernumsampswthreshnames = usernumsampswthresh[duplicated(usernumsampswthresh$user),"user"]
# Perform paired t-test for difference in means in reviews with and without service words across all users
usernumsampswthreshwsw = usernumsampswthresh[usernumsampswthresh$sw==1,]
usernumsampswthreshwosw = usernumsampswthresh[usernumsampswthresh$sw==0,]
useravgscoreswwsw = useravgscoresw[useravgscoresw$sw==1,]
useravgscoreswwosw = useravgscoresw[useravgscoresw$sw==0,]
userwsw = merge(usernumsampswthreshwsw,useravgscoreswwsw,by.x="user",by.y="user")
colnames(userwsw)[c(3,5)] = c("sampwsw","scorewsw")
userwosw = merge(usernumsampswthreshwosw,useravgscoreswwosw,by.x="user",by.y="user")
colnames(userwosw)[c(3,5)] = c("sampwosw","scorewosw")
userall = merge(userwsw,userwosw,by.x="user",by.y="user")
userpairedttest = t.test(x=userall$scorewsw,userall$scorewosw,alternative="two.sided",paired=TRUE)
print(userpairedttest$p.value); print(mean(userall$scorewsw)); print(mean(userall$scorewosw))
# Perform two sample t-tests for difference in means in reviews with and without service words for each user individually
alluserttest <- function(x) {
	curname = usernumsampswthreshnames[x]
	matchnames = worddatan[worddatan$user==curname,]
	pvalue = t.test(matchnames[matchnames$sw==0,"score"],matchnames[matchnames$sw==1,"score"],alternative="two.sided")$p.value
	return(pvalue)
}
alluserttestres = unlist(mclapply(1:length(usernumsampswthreshnames),alluserttest))
alluserttestqvals = qvalue(alluserttestres)$qvalues
usersigtests = (alluserttestqvals < 0.05)

# Look at distribution of percent reviews with service words for each individual
userswpercent = vector("numeric",length=nrow(userall))
userswdiff = vector("numeric",length=nrow(userall))
for(i in 1:length(userswpercent)) {
	userswpercent[i] = userall[i,"sampwsw"]/(userall[i,"sampwsw"] + userall[i,"sampwosw"])*100
	userswdiff[i] = userall[i,"scorewosw"] - userall[i,"scorewsw"]
}
mean(userswpercent); median(userswpercent)
# Also correlation between percent above and service word review score difference
cor.test(userswpercent,userswdiff)

## Look more closely at businesses with significant service word score differences
sigbusindex = which(bussigtests)
sigbusnames = busall[sigbusindex,"business"]
sigbusnamesm = match(sigbusnames,busmetadata$business)
sigbuscats = busmetadata[sigbusnamesm,"cats"]
sigbuscatsdist = table(unlist(strsplit(sigbuscats,split=",")))
sigbuscatsdistc = sigbuscatsdist[!(sigbuscatsdist<3)]
# Random permutations for categories
numperms=1000
permnamesall=vector("list",numperms)
for(i in 1:numperms) {
permnamesall[[i]] = busall[sample(1:nrow(busall),length(sigbusindex)),"business"]
}
catpermute <- function(x) {
	permnamesm = match(permnamesall[[x]],busmetadata$business)
	permcats = busmetadata[permnamesm,"cats"]
	permcatsdist = table(unlist(strsplit(permcats,split=",")))
	permcatsdistc = permcatsdist[!(permcatsdist<3)]
return(permcatsdistc)
}
catperms = mclapply(1:numperms,catpermute)
buscatpval = vector("numeric",length(sigbuscatsdistc))
for(i in 1:length(sigbuscatsdistc)) {
	curcat = names(sigbuscatsdistc)[i]
	orignum = sigbuscatsdistc[i]
	matchname = sapply(sapply(catperms,names),"%in%",curcat)
	matchnameindex = sapply(matchname,sum)
	matchnameindexc = which(matchnameindex==1)
	curnums = vector("numeric",length(matchnameindexc))
	for(j in 1:length(matchnameindexc)) {
		curnums[j] = catperms[[matchnameindexc[j]]][which(matchname[[matchnameindexc[j]]])]
	}
	buscatpval[i] = sum(orignum>curnums)/length(curnums)
}
sigbuscatsdistc[which(buscatpval<(0.05/length(buscatpval)))]
# Is there correlation between dollar score and service word difference pvalue?
matchdollar = match(busall$business,busmetadata$business)
pulldollar = busmetadata[matchdollar,"dollars"]
cor.test(-log(allbusttestres),pulldollar)

##### Part 3: Plot the data

# Association between review score and review length
pdf("./plots/reviewscorelengthassoc.pdf")
ggplot(aes(x=length,y=score),data=data.frame(length=worddatain[,4],score=abs(worddatain[,3]))) + ylim(c(0,5)) + geom_point(alpha=.2) + theme_bw() + theme(axis.text = element_text(size=rel(1.5)),panel.border=element_blank()) + geom_smooth(method = "lm",formula=y~x,fullrange=TRUE,weight=rel(2),color=pastelred)
dev.off()
# Businesses with significant shifts in review scores
pdf("./plots/revscoreswvnow.pdf")
pastelred="#ff5148"
bussigfac = (as.numeric(bussigtests)+1)
busallnonsig = busall[bussigfac==1,]
busallsig = busall[bussigfac==2,]
ggplot(aes(x=scorewsw,y=scorewosw),data=busallnonsig) + geom_point(alpha=.5) + scale_shape_identity() + theme_bw() + geom_point(aes(x=scorewsw,y=scorewosw),data=busallsig,size=5,col=pastelred,shape=18,alpha=.9) + xlab("Review Score with Service Words") + ylab("Review Score without Service Words") + theme(axis.text = element_text(size=rel(1.5),color="dark grey"),axis.ticks=element_line(colour="dark grey"), panel.border=element_blank()) + geom_abline(intercept=0,slope=1,linetype=2,alpha=.35,col="grey",size=2) + xlim(c(0,4.5)) + ylim(c(0,4.5))
dev.off()
# Also output to file
busoutput = data.frame(busallsig,pvalues=allbusttestres[bussigfac==2])
matchbustometa = match(busoutput$business,busmetadata$business)
bussideind = sign(busoutput$scorewosw-busoutput$scorewsw)
busoutput2 = data.frame(busoutput,dir=bussideind,state=busmetadata$state[matchbustometa],city=busmetadata$city[matchbustometa])
write.table(busoutput2,file="./sigbusinessdetails.txt")
# And output business data file for interactive visualization
businteract1 = merge(busall,busnamedata,by.x="business",by.y="V1")
totsamps = with(businteract1,sampwsw+sampwosw)
totrevscore = with(businteract1,(scorewsw*sampwsw+scorewosw*sampwosw)/(sampwsw+sampwosw))
businteract2 = data.frame(businteract1,totalsamples=totsamps,totalreviewscore=totrevscore,pvalues=allbusttestres,qvalues=allbusttestqvals)
businteract = businteract2[,c("business","V2","sampwsw","scorewsw","sampwosw","scorewosw","totalsamples","totalreviewscore","pvalues","qvalues","V5","V6","V3","V4")]
colnames(businteract)[c(2,11:14)] = c("fullname","city","state","lat","long")
write.table(businteract,file="./fullbusinessdetails.txt")
# Users with significant shifts in review scores
pdf("./plots/revscoreswvnowusers.pdf")
usersigfac = (as.numeric(usersigtests)+1)
userallnonsig = userall[usersigfac==1,]
userallsig = userall[usersigfac==2,]
ggplot(aes(x=scorewsw,y=scorewosw),data=userallnonsig) + geom_point(alpha=.5) + scale_shape_identity() + theme_bw() + geom_point(aes(x=scorewsw,y=scorewosw),data=userallsig,size=5,col=pastelred,shape=18,alpha=.9) + xlab("Review Score with Service Words") + ylab("Review Score without Service Words") + theme(axis.text = element_text(size=rel(1.5),color="dark grey"),axis.ticks=element_line(colour="dark grey"), panel.border=element_blank()) + geom_abline(intercept=0,slope=1,linetype=2,alpha=.35,col="grey",size=2) + xlim(c(0,4.5)) + ylim(c(0,4.5))
dev.off()
# Histogram of percent reviews with service words
pdf("./plots/percentreviewwsw.pdf")
ggplot(aes(x=userswpercent),data=data.frame(userswpercent)) + geom_histogram(breaks=seq(0,100,2),alpha=.75,fill="light blue",col="grey") + theme_bw() + theme(axis.text = element_text(size=rel(1.5)),panel.border=element_blank()) + geom_vline(xintercept=54.55,linetype=1,alpha=.6,col=pastelred,size=1.5)
dev.off()
# Look at correlation between percent of reviews with service words and difference between reviews with and without service words
pdf("./plots/percentreviewvsnoswmsw.pdf")
ggplot(aes(x=userswpercent,y=userswdiff),data=data.frame(userswpercent,userswdiff)) + ylim(c(-2,2)) + geom_point(alpha=.2) + theme_bw() + theme(axis.text = element_text(size=rel(1.5)),panel.border=element_blank()) + xlim(c(0,100)) + geom_smooth(method = "lm",formula=y~x,fullrange=TRUE,weight=rel(2),color=pastelred)
dev.off()

########## Section 2: Associating words with score

##### Part 1: Load in the data

# word, number of times with 1 star, 2 stars, etc.
ratingpresencedata = read.table("/path/to/yelp_dataset_challenge_academic_dataset/finalSet/wordRating.txt",header=FALSE,stringsAsFactors=FALSE)
totalreviews = as.numeric(ratingpresencedata[1,-1])
ratingpresencedatarn = ratingpresencedata[,1]
ratingpresencedatac = ratingpresencedata[-1,-1]
colnames(ratingpresencedatac) = c("one","two","three","four","five")
rownames(ratingpresencedatac) = ratingpresencedatarn[-1]
# word, number of times with 1 dollar, 2 dollar, etc.
dollarpresencedata = read.table("/path/to/yelp_dataset_challenge_academic_dataset/finalSet/wordDollar.txt",header=FALSE,stringsAsFactors=FALSE)
totalreviewsdollar = as.numeric(dollarpresencedata[1,-1])
dollarpresencedatarn = dollarpresencedata[,1]
dollarpresencedatac = dollarpresencedata[-1,-1]
colnames(dollarpresencedatac) = c("one","two","three","four")
rownames(dollarpresencedatac) = dollarpresencedatarn[-1]

##### Part 2: Perform analysis

# Perform correlations between word frequency and trait
wordcoranalysis <- function(x,datanorm) {
	curcor = cor(datanorm[x,],1:ncol(datanorm))
return(curcor)
}
# Geometric mean function from http://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
gm_mean = function(x, na.rm=TRUE){
	  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
# Perform analysis of word frequency and trait
wordanalysis <- function(data) {
	# Threshold data to only words with at least 50 mentions
	datam = (data>50)
	totmentions = apply(datam,1,sum) 
	totmentionsthresh = which(totmentions==ncol(data))
	datat = data[totmentionsthresh,]
	# Normalize the data by centering the geometric means
	datatmeds = apply(datat,2,median)
	geommeans = apply(datat,1,gm_mean)
	geommeansmed = median(geommeans)
	geommednorm = geommeansmed/datatmeds
	datanorm = scale(datat,center=FALSE,scale=(1/geommednorm))
	# Calculate correlations between frequency and column location
	wordcors = unlist(mclapply(1:nrow(datanorm),wordcoranalysis,datanorm))
return(list(wordcors=wordcors,normdata=datanorm))
}
# Analysis on review stars
wordreviewstars = wordanalysis(ratingpresencedatac)
# Permuting data for significance calls
allperms = permn(1:5)
permstores = vector("list",length(allperms))
for(i in 1:length(allperms)) {
    print(i)
    curperm = ratingpresencedatac[,allperms[[i]]]
    permstores[[i]] = wordanalysis(curperm)$wordcors
}
combineperms = unlist(permstores)
permpvals = vector("numeric",length(wordreviewstars$wordcors))
permpvals[wordreviewstars$wordcors>=0] = 2*(1-ecdf(combineperms)(wordreviewstars$wordcors[wordreviewstars$wordcors>=0]))
permpvals[wordreviewstars$wordcors<0] = 2*ecdf(combineperms)(wordreviewstars$wordcors[wordreviewstars$wordcors<0])
bonfpvals = p.adjust(permpvals,method="bonferroni")
bhpvals = p.adjust(permpvals,method="BH")
which(bhpvals < 0.05)
head(which(bhpvals < 0.05))
rownames(wordreviewstars$normdata)[which(bhpvals < 0.05)]

wordrank = rank(wordreviewstars$wordcors)
wordranko = order(wordrank,decreasing=TRUE)
topreviewstars = rownames(wordreviewstars$normdata)[wordranko][1:50]
bottomreviewstars = rev(rownames(wordreviewstars$normdata)[wordranko])[1:50]
write.table(data.frame(top=topreviewstars,bottom=bottomreviewstars),file="./topwordsreviewstars.txt")

# Analysis on dollar signs
worddollarsigns = wordanalysis(dollarpresencedatac)
# Permuting data for significance calls
allpermsdollar = permn(1:4)
permstoresdollar = vector("list",length(allpermsdollar))
for(i in 1:length(allpermsdollar)) {
    print(i)
    curperm = dollarpresencedatac[,allpermsdollar[[i]]]
    permstoresdollar[[i]] = wordanalysis(curperm)$wordcors
}
combinepermsdollar = unlist(permstoresdollar)
permpvalsdollar = vector("numeric",length(worddollarsigns$wordcors))
permpvalsdollar[worddollarsigns$wordcors>=0] = 2*(1-ecdf(combinepermsdollar)(worddollarsigns$wordcors[worddollarsigns$wordcors>=0]))
permpvalsdollar[worddollarsigns$wordcors<0] = 2*ecdf(combinepermsdollar)(worddollarsigns$wordcors[worddollarsigns$wordcors<0])
bonfpvalsdollar = p.adjust(permpvalsdollar,method="bonferroni")
bhpvalsdollar = p.adjust(permpvalsdollar,method="BH")
which(bhpvalsdollar < 0.05)
head(which(bhpvalsdollar < 0.05))
rownames(worddollarsigns$normdata)[which(bhpvalsdollar < 0.05)]

wordrankdollar = rank(worddollarsigns$wordcors)
wordrankdollaro = order(wordrankdollar,decreasing=TRUE)
topdollarsigns = rownames(worddollarsigns$normdata)[wordrankdollaro][1:50]
bottomdollarsigns = rev(rownames(worddollarsigns$normdata)[wordrankdollaro])[1:50]
write.table(data.frame(top=topdollarsigns,bottom=bottomdollarsigns),file="./topwordsdollarsigns.txt")

##### Part 3: Plot the data

# Plot change in rank over stars for top 10 correlations in each direction
pastelblue="#779ecb"
upcolors = colorRampPalette(c("dark blue","blue"))(10)
upwidths = seq(3,1.5,length.out=10)
downcolors = colorRampPalette(c("red","dark red"))(10)
downwidths = seq(3,1.5,length.out=10)
top10up = wordranko[1:10]
top10down = rev(wordranko)[1:10]
pdf("./plots/wordrankchanges.pdf")
plot(50,50,xlim=c(.5,5.5),ylim=c(0,4851),xlab="Star Rating",ylab="Rank",bty="n",yaxt="n")
plotrankdata = apply(wordreviewstars$normdata,2,rank)
for(i in 1:length(top10down)) {
	lines(1:5,plotrankdata[top10down[i],],col=pastelred,lwd=downwidths[i])
	text(5.5,plotrankdata[top10down[i],][5],rownames(plotrankdata)[top10down[i]],srt=-20)
}
for(i in 1:length(top10up)) {
	lines(1:5,plotrankdata[top10up[i],],col=pastelblue,lwd=upwidths[i])
	text(.65,plotrankdata[top10up[i],][1],rownames(plotrankdata)[top10up[i]],srt=10)
}
dev.off()
top10updollars = wordrankdollaro[1:10]
top10downdollars = rev(wordrankdollaro)[1:10]
pdf("./plots/wordrankchangesdollars.pdf")
plot(50,50,xlim=c(.5,4.5),ylim=c(0,3871),xlab="Dollar Signs",ylab="Rank",bty="n",yaxt="n")
plotrankdatadollars = apply(worddollarsigns$normdata,2,rank)
for(i in 1:length(top10down)) {
	lines(1:4,plotrankdatadollars[top10downdollars[i],],col=pastelred,lwd=downwidths[i])
	text(4.5,plotrankdatadollars[top10downdollars[i],][4],rownames(plotrankdatadollars)[top10downdollars[i]],srt=-20)
}
for(i in 1:length(top10up)) {
	lines(1:4,plotrankdatadollars[top10updollars[i],],col=pastelblue,lwd=upwidths[i])
	text(.65,plotrankdatadollars[top10updollars[i],][1],rownames(plotrankdatadollars)[top10updollars[i]],srt=10)
}
dev.off()
# Distribution of word and review star correlations
pdf("./plots/wordreviewstarcors.pdf")
hist(wordreviewstars$wordcors,breaks=100,xlim=c(-1,1))
dev.off()
# Distribution of word and review star correlations
pdf("./plots/worddollarsignscors.pdf")
hist(worddollarsigns$wordcors,breaks=100,xlim=c(-1,1))
dev.off()
# Double checking distributions of word frequencies
pdf("./plots/alldists.pdf")
hist(wordreviewstars$normdata[,5],col=adjustcolor("red",alpha.f=.5),xlim=c(0,210000),breaks=seq(0,210000,length.out=100),ylim=c(0,100))
par(new=T)
hist(wordreviewstars$normdata[,4],col=adjustcolor("blue",alpha.f=.5),xlim=c(0,210000),breaks=seq(0,210000,length.out=100),ylim=c(0,100))
par(new=T)
hist(wordreviewstars$normdata[,3],col=adjustcolor("green",alpha.f=.5),xlim=c(0,210000),breaks=seq(0,210000,length.out=100),ylim=c(0,100))
par(new=T)
hist(wordreviewstars$normdata[,2],col=adjustcolor("orange",alpha.f=.5),xlim=c(0,210000),breaks=seq(0,210000,length.out=100),ylim=c(0,100))
par(new=T)
hist(wordreviewstars$normdata[,1],col=adjustcolor("yellow",alpha.f=.5),xlim=c(0,210000),breaks=seq(0,210000,length.out=100),ylim=c(0,100))
dev.off()
