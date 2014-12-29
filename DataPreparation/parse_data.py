#!/usr/bin/env python
'''
Written by Ryo Kita
Parses the yelp dataset for easier analysis in R
#Definition: swscore = { if service word in review: rating;   if no service word in review: rating*-1}
'''

import json, numpy, scipy.stats, string, sys

def main():

    #Get raw data and initial parse
    bizDict = biz_parse()
    rev_parse(bizDict)
    
    #Search for service word
    sw = load_service_words()
    search_data_create_swscore_file(sw)

    #Parse data some more
    create_bizAttributes_file(bizDict)
    create_wordList()
    create_word_rating_file()
    create_word_dollar_file(bizDict)




#########################
def biz_parse():
    ''' Parses yelp BUSINESS dataset - Extracting only restaurants. '''
    print "Parsing Business Data.  Getting business_id, review_count, name"
    jsonData = open("yelp_academic_dataset_business.json")
    bizData = {}
    categoryList = list()
    for line in jsonData:
    	data = json.loads(line)
    	business_id = data["business_id"]
    	review_count = data["review_count"]
    	name = data["name"]
    	categories = data["categories"]
	state = data["name"]
    	city = data["city"]
    	latitude = data["latitude"]
    	longitude = data["longitude"]
	attributes =  data["attributes"]
    	
    	if len(categories)> 0:
    	    if "Restaurants" in categories:
		try: priceRange = attributes["Price Range"]
                except KeyError: priceRange = "NA"
		bizData[business_id] = [review_count, name, categories, state, city, latitude, longitude, priceRange]
    	        categoryList += categories
    print len(bizData), "businesses added"
    return bizData

#########################
def rev_parse(bizDict):
    ''' Parses yelp REVIEW dataset - extracting only the keys in the input dictionary (keys are businesses) '''
    print "Parsing Review Data.  Only getting reviews for businessness from the input dictionary"
    jsonData = open("yelp_academic_dataset_review.json")
    fileOUT =open("restReviews.txt", "w")
    for line in jsonData:
    	data = json.loads(line)
    	business_id = data["business_id"]
    	stars = data["stars"]
    	text = data["text"]	
        user_id = data["user_id"]
    	if business_id in bizDict:
    	    outLine = str(business_id) + "\t" + str(user_id) + "\t" +  str(stars) + "\t" + " ".join(text.encode('ascii', 'ignore').split("\n")) + "\n"
    	    fileOUT.write(outLine)
    fileOUT.close()

########################
def load_service_words():
    '''  Loads service words into a dictionary '''
    fileIN = open("serviceWords.txt")
    servicewords = list()
    for i in fileIN:
    	servicewords.append(i.rstrip())
    fileIN.close()	
    print "Number of service words loaded: ", len(servicewords)
    return servicewords
	
#########################
def search_data_create_swscore_file(sw):
    ''' Searches review data for service word, and creates a file with the swscore '''
    fileIN = open("restReviews.txt")
    fileOUT = open("restReviewsSW3.txt", "w")
    for i in fileIN:
    	line = i.split("\t")
    	review = line[3].lower()
    	success = 0
    	match = ""
        numWords = len(review.split())
    	for word in sw: 
    	    if word in review: 
    		success +=1
    		break
        sign = "NA"
        if success==0: 
            swscore = int(line[2])*-1
            sign = "-1"
        else: 
            swscore = int(line[2])
            sign = "1"
    	outLine = line[0] + "\t" + line[1] +"\t" + str(swscore) + "\t" + str(numWords) + "\t" + line[2] + "\t" + sign +  "\n"
    	fileOUT.write(outLine)
    fileOUT.close()

#########################
def create_wordList():
    '''Creates a list of unique words present across all reviews '''
    fileIN = open("restReviews.txt")
    textSet = set()
    for i in fileIN:
        i =i.rstrip().split("\t")
        if len(i)!=4: continue
        text = i[3].split()
        puncRemovedText =  [x.translate(string.maketrans("",""), string.punctuation) for x in text] #Remove punctuation
        for x in puncRemovedText:
            if x.lower() not in textSet: textSet.add(x.lower())

    fileOUT = open("wordList.txt", "w")
    fileOUT.write("\n".join(list(textSet)))
    fileOUT.close()

#########################
def create_word_rating_file():
    '''	Parses how many reviews with a particular rating a word is found in.  '''
    fileIN = open("wordList.txt")
    wordDict = dict() #Holds the number of 1,2,3,4,5 star
    for i in fileIN:
        i = i.rstrip()
        if len(i)>0: wordDict[i] = [0,0,0,0,0]
    fileIN.close()

    fileIN = open("restReviews.txt")
    ratingCount = [0,0,0,0,0]
    for i in fileIN: 
        i =i.rstrip().split("\t")
        if len(i)!=4: continue
        text = i[3].split()

        puncRemovedText =  list(set([x.translate(string.maketrans("",""), string.punctuation).lower() for x in text])) #Remove punctuation
        ratingIdx = int(i[2])-1  #Subtract one because 0 index
        ratingCount[ratingIdx] +=1
        for x in puncRemovedText:
            if len(x)>0: wordDict[x.lower()][ratingIdx] +=1
    fileIN.close()

    fileOUT =open("wordRating.txt", "w")
    headerLine = "TOTALREVIEWS" + "\t" + "\t".join([str(x) for x in ratingCount]) + "\n"
    fileOUT.write(headerLine)
    for i in wordDict:
        outLine = i + "\t" +  "\t".join([str(x) for x in wordDict[i]]) + "\n"
        fileOUT.write(outLine)
    fileOUT.close()

#########################
def create_word_dollar_file(bizDict):
    '''Parses how many reviews with a particular dollar sign a word is found in.'''
    fileIN = open("wordList.txt")
    wordDict = dict() #Holds the number of 1,2,3,4 dollar signs
    for i in fileIN:
        i = i.rstrip()
        if len(i)>0: wordDict[i] = [0,0,0,0]
    fileIN.close()

    fileIN = open("restReviews.txt")
    dollarCount = [0,0,0,0]
    for i in fileIN: 
        i =i.rstrip().split("\t")
        if len(i)!=4: continue
        bizId = i[0]
        dollarVal = bizDict[bizId][7]
        if dollarVal == "NA": continue
        dollarIdx = int(dollarVal)-1
        text = i[3].split()
        dollarCount[dollarIdx] +=1
        
        puncRemovedText =  list(set([x.translate(string.maketrans("",""), string.punctuation).lower() for x in text])) #Remove punctuation
        for x in puncRemovedText:
            if len(x)>0: wordDict[x.lower()][dollarIdx] +=1

    fileOUT =open("wordDollar.txt", "w")
    headerLine = "TOTALREVIEWS" + "\t" + "\t".join([str(x) for x in dollarCount]) + "\n"
    fileOUT.write(headerLine)
    for i in wordDict:
        outLine = i + "\t" +  "\t".join([str(x) for x in wordDict[i]]) + "\n"
        fileOUT.write(outLine)
    fileOUT.close()

#########################
def create_bizAttributes_file(bizDict):
    '''Creates a flat file with tab-delimited columns: Business, swscore (sep by comma), restaurant categories (sep by comma), dollar score'''
    
    fileIN = open("restReviewsSW3.txt")
    data = list()
    for i in fileIN:
        i =i.rstrip().split("\t")
        data.append(i)
    fileIN.close()

    fileOUT = open("bizAttributes.txt", "w")
    counter = 0
    for bizId in bizDict:
        counter +=1
        print "\rParsing: ", counter, " out of ", len(bizDict),
        matchingBizs = [x for x in data if x[0]==bizId]
        swscores = [x[2] for x in matchingBizs]
        outLine = bizId +"\t" +  ",".join(swscores) + "\t" +  ",".join(bizDict[bizId][2]) + "\t" + str(bizDict[bizId][7]) + "\n"
        fileOUT.write(outLine)
    fileOUT.close()

#########################
def biz_print(bizDict):

    fileOUT = open("code2name.txt", "w")
    for i in bizDict:
        data = bizDict[i]
        outLine = i + "\t" + data[1].encode('ascii', 'ignore') +"\t" + str(data[5]) + "\t" + str(data[6]) +  "\t" + str(data[4].encode('ascii', 'ignore')) +  "\t" + str(data[3].encode('ascii', 'ignore')) + "\n"
        fileOUT.write(outLine)
    fileOUT.close()

	
#########################
if __name__=="__main__":
	main()
#########################
