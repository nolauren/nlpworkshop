
# An Introduction to Natural Language Processing

These notes accompany the more detailed slides and talk given in the workshop.
It is highly encouraged but not required to follow along with the code snippets.
With few exceptions you should be able to simply copy and paste the code boxes
directly into R, though feel free to experiment with code as we go along!

## Tokenization

### Simple example

A simple example from the opening lines of Albert Camus's *L’Étranger*:
```{r}
sIn <- "Mother died today. Or, maybe, yesterday; I can't be sure."
```
We can do a simple version of tokenization using spaces:
```{r}
strsplit(sIn, split=" ")
```

### Loading data and code

Before we apply more sophisticated techniques, we'll load in a set of functions
into R that help us work with the NLP pipeline. This can be done by the calling
the following inside of R (internet connection required):
```{r}
source("https://raw.githubusercontent.com/nolauren/nlpworkshop/master/corenlp_funs.R")
```
Next, download the zip file containing all of the data in today's workshop:
[zip file](https://github.com/nolauren/nlpworkshop/raw/master/annotations.zip).
Unzip the archive and place it on your desktop. Then, on a Mac run the following
inside R to set the working directory:
```{r}
setwd("~/Desktop/annotations")
```
Or on windows:
```{r}
setwd("C:/Users/your_user_name/Desktop/annotations")")
```
With your username filled in in place of `your_user_name`.

### Simple example again

We'll now look at what we call the *annotation* of the string `sIn`. This is the
result of applying the entire NLP pipeline to the string of text. At the end of
the workshop we will show you how to do the annotation yourself, but for now we
will just load in pre-annotated text in the sake of time:
```{r}
annotation <- readRDS("simple_annotation.Rds")
annotation
```
Notice the information that is displayed when we print the annotation (objects
in R are printed when we write the object's name on its own line and hit enter).

To actually see the tokenization from the annotation, we run the following:
```{r}
getToken(annotation)$token
```
We can also see how these tokens are split into sentences by this code snippet:
```{r}
getToken(annotation)$sentence
```
We will look at other aspects of annotation objects throughout the workshop.

### Other languages

Now let's take the original French version of *L’Étranger*:
```{r}
strIn <- "Aujourd'hui, maman est morte. Ou peut-être hier, je ne sais pas. J'ai reçu un télégramme de l'asile."
```
If we produce a standard English annotation the tokenization does a reasonably
close approximation, but makes a lot of mistakes:
```{r}
annoEnglish <- readRDS("annoEnglish.Rds")
getToken(annoEnglish)$token
```
It is possible to load a French-specific NLP pipeline that corrects all of these
issues.
```{r}
annoFrench <- readRDS("annoFrench.Rds")
getToken(annoFrench)$token
```
At the end when we show how to actually run the annotations, there will be a description of how
to select the desired language as well.

### A longer application

Now, read in the entire annotation from the Sherlock Holmes' short story
'A Scandel in Bohemia':
```{r}
anno <- readRDS("holmes/01_a_scandal_in_bohemia.Rds")
anno
```
Notice how many tokens and sentences there are. We can visualize the lengths of
sentences using a histogram:
```{r}
sentLen <- table(getToken(anno)$sentence)
hist(sentLen, breaks=30)
```
We are already getting a sense of the writing style in the text.

## Lemmatization and Part of Speech Tagging

Now, look at the lemmatization and part of speech tagging from the second
sentence in our short story:
```{r}
anno <- readRDS("holmes/01_a_scandal_in_bohemia.Rds")
token <- getToken(anno)
token[token$sentence==2,c(1:4,7)]
```

### Universal tagset

Converting to the universal tagset, note the distribution of parts of
speech:
```{r}
ut <- universalTagset(token$POS)
table(ut)
```
See which Penn parts of speech are associated with nouns and verbs:
```{r}
unique(token$POS[ut == "NOUN"])
unique(token$POS[ut == "VERB"])
```

### Some data analysis

Now, let's do a bit of data analysis and try to understand the distribution
of parts of speech within a given sentence
```{r}
nounCnt <- tapply(ut == "NOUN", token$sentence, sum)
pronCnt <- tapply(ut == "PRON", token$sentence, sum)
adjCnt <- tapply(ut == "ADJ", token$sentence, sum)
verbCnt <- tapply(ut == "VERB", token$sentence, sum)
posDf <- data.frame(nounCnt,pronCnt,adjCnt,verbCnt)
head(posDf)
```
Does the second lind of `posDf` match the part of speech codes we saw at the
top of this section? Let's visualize this now:
```{r}
par(mfrow=c(1,2))
plot(nounCnt + pronCnt, adjCnt, pch=19, cex=1, col=rgb(0,0,1,0.02))
plot(nounCnt + pronCnt, verbCnt, pch=19, cex=1, col=rgb(0,0,1,0.02))
```

### Most frequent lemmas

Take a look at the most frequntly used Nouns:
```{r}
index <- which(ut=="NOUN")
tab <- table(token$lemma[index])
head(sort(tab,decreasing=TRUE),25)
```
And verbs:
```{r}
index <- which(token$POS == "NNP")
tab <- table(token$lemma[index])
head(sort(tab,decreasing=TRUE),25)
```
Do the results surprise you or reveal anything particular abotu the story?

## Dependencies

Now, let's re-load our text (just in case it get's modified or deleted for 
some reason) and extract both the tokens and the dependencies:
```{r}
anno <- readRDS("holmes/01_a_scandal_in_bohemia.Rds")
token <- getToken(anno)
dep <- getDependency(anno)
```
Now look at the dependencies from the first sentence:
```{r}
dep[dep$sentence == 1,]
```
Try to change the `1` to a different sentence. Is the structure of the
dependencies starting to make some sense?

### Verb usage by gender

An interesting application of dependencies is to see which verbs are
associated with certain characters. Here is a code snippet that finds
all dependencies between the pronoun `he` and a verb associated with
`he`. 
```{r}
index <- which(token$lemma[dep$depIndex] == "he" &
      universalTagset(token$POS[dep$govIndex]) == "VERB")
depSelf <- dep[index,]
depSelf <- depSelf[depSelf$type == "nsubj",]
sort(table(depSelf$governor),decreasing=TRUE)[1:10]
```
Take note of all the verbs associated with the male pronoun. Now repeat
the analysis with the pronoun `she`:
```{r}
index <- which(token$lemma[dep$depIndex] == "she" &
      universalTagset(token$POS[dep$govIndex]) == "VERB")
depSelf <- dep[index,]
depSelf <- depSelf[depSelf$type == "nsubj",]
sort(table(depSelf$governor),decreasing=TRUE)[1:10]
```
How do the verbs associated with female characters seem to differ from the
male characters? Do you find this surprising?

### Finding compound nouns

Another usage of dependencies is to construct compound nouns from multiple
words, such as *New York City* and *Sherlock Holmes*. The following code
snippet finds all dependencies which link two compound, proper nouns 
(the `toupper` lines are particular to this text to remove several words
that were transcribed in all capital letters):
```{r}
index <- which(dep$type == "nn" &
          token$POS[dep$govIndex] == "NNP" &
          token$POS[dep$depIndex] == "NNP" &
          (toupper(token$word) != token$word)[dep$govIndex] &
          (toupper(token$word) != token$word)[dep$depIndex])
nnDep <- dep[index,]
```
We can look at the `nnDep` object now, but there is still some work to do
to actually paste the compound nouns together:
```{r}
nnDep
```
The code to collapse multiple words together is below; the main reason for
being a bit complicated is because it needs to handle cases, like *New York City*,
that have more than just 2 words put together:
```{r}
pname <- startIndex <- endIndex <- NULL
for (g in unique(nnDep$govIndex)) {
  these <- c(which(nnDep$depIndex == g),
            which(nnDep$govIndex == g))
  these <- range(c(nnDep$depIndex[these],nnDep$govIndex[these]))
  out <- paste(token$word[these[1]:these[2]],collapse=" ")
  pname <- c(pname, out)
  startIndex <- c(startIndex, these[1])
  endIndex <- c(endIndex, these[2])
}
pnames <- data.frame(pname,startIndex,endIndex,
                    stringsAsFactors=FALSE)
```
Now looking at the unique compound, proper nouns we get a sense of the
people and places in the text:
```{r}
unique(pnames$pname)
```
Notice that there is a ways to go to creating a complete character list, but
at least this is a good start!

## Named Entity Recognition

```{r}
token <- getToken(anno)
table(token$NER)
```

```{r}
unique(token$lemma[token$NER=="LOCATION"])
```

```{r}
token[485:490,]
```

```{r}
token[6991:6994,]
```

```{r}
these <- which(token$NER == "PERSON")
pnames <- pnames[which(pnames$endIndex %in% these),]
these <- these[-which(these %in% pnames$endIndex)]
newPnames <- data.frame(pname=token$word[these],
                  startIndex=these,
                  endIndex=these,
                  stringsAsFactors=FALSE)
pnames <- rbind(pnames,newPnames)
pnames <- pnames[toupper(pnames$pname) != pnames$pname,]
unique(pnames$pname)
```

## Coreferences

```{r}
coref <- getCoreference(anno)
head(coref)
```

```{r}
table(token$word[coref$startIndex[coref$corefId == 1]])
```

```{r}
index <- match(as.numeric(coref$endIndex), pnames$endIndex)
head(index)
```


```{r}
temp <- tapply(pnames$pname[index[!is.na(index)]],
              coref[!is.na(index),1],
              function(v) {names(rev(sort(table(v))))[1]} )
perMap <- data.frame(corefId=names(temp),perName=temp)
perMap
```

```{r}
tab <- table(coref[,1])
index <- match(perMap$corefId, names(tab))
perMap$count <- tab[index]
charImport <- sort(tapply(perMap$count, perMap$perName, sum),TRUE)
charImport
```

## Sherlock Holmes Short Stories

```{r}
output = c()
outputGraphics = list()
iter = 1
for (f in dir("holmes/",full.names=TRUE)) {
  anno = readRDS(f)
  token = getToken(anno)
  dep = getDependency(anno)
  coref = getCoreference(anno)

  # Dependency part
  index = which(dep$type == "nn" &
            token$POS[dep$govIndex] == "NNP" &
            token$POS[dep$depIndex] == "NNP" &
            (toupper(token$word) != token$word)[dep$govIndex] &
            (toupper(token$word) != token$word)[dep$depIndex])
  nnDep = dep[index,]

  pname = startIndex = endIndex = NULL
  for (g in unique(nnDep$govIndex)) {
    these = c(which(nnDep$depIndex == g),
              which(nnDep$govIndex == g))
    these = range(c(nnDep$depIndex[these],nnDep$govIndex[these]))
    index = these[1]:these[2]
    words = token$word[index][token$POS[index] != "." &
                              token$NER[index] %in% c("O","PERSON")]
    out = paste(words,collapse=" ")
    pname = c(pname, out)
    startIndex = c(startIndex, these[1])
    endIndex = c(endIndex, these[2])
  }
  pnames = data.frame(pname,startIndex,endIndex,stringsAsFactors=FALSE)

  # NER
  these = which(token$NER == "PERSON")
  pnames = pnames[which(pnames$endIndex %in% these),]
  these = these[-which(these %in% pnames$endIndex)]
  newPnames = data.frame(pname=token$word[these],
                    startIndex=these,
                    endIndex=these,
                    stringsAsFactors=FALSE)
  pnames = rbind(pnames,newPnames)
  pnames = pnames[toupper(pnames$pname) != pnames$pname,]
  length(unique(pnames$pname))

  pnames$pname = gsub("Mister", "Mr.", pnames$pname)
  for (j in 1:nrow(pnames)) {
    matchString = gsub(" ", "[^.]*", pnames$pname[j])
    these = grep(matchString,pnames$pname)
    pnamesSet = pnames$pname[these]
    pnames$pname[j] = pnamesSet[which.max(nchar(pnamesSet))]
  }

  index = match(as.numeric(coref$endIndex), pnames$endIndex)
  temp = tapply(pnames$pname[index[!is.na(index)]],
                coref[!is.na(index),1],
                function(v) {names(rev(sort(table(v))))[1]} )
  perMap = data.frame(corefId=names(temp),perName=temp)

  tab = table(coref[,1])
  index = match(perMap$corefId, names(tab))
  perMap$count = tab[index]
  charImport = sort(tapply(perMap$count, perMap$perName, sum),TRUE)
  removeThese = c(grep("Sherlock", names(charImport)),
                 grep("Holmes", names(charImport)),
                 grep("Watson", names(charImport)))
  if (length(removeThese)) charImport = charImport[-removeThese]
  output = c(output, names(charImport)[1])

  crefIds = perMap$corefId[perMap$perName == names(charImport)[1]]
  places = coref$startIndex[coref$corefId %in% crefIds]
  outputGraphics[[iter]] = places / nrow(token)
  iter = iter + 1
}
output = gsub(" ,", ",", output)
```

```{r}
output
```

```{r}
par(mar=c(0,0,0,0))
par(mfrow=c(1,1))
plot(0,0,col="white",ylim=c(1,56),xlim=c(0,1),axes=FALSE)
for (j in 1:length(outputGraphics)) {
  points(outputGraphics[[j]],rep(57-j,length(outputGraphics[[j]])),
         pch=19, cex=0.4)
  abline(h=j)
}
rect(-100,-100,0.005,100,bg="white",density=NA,col="white")
rect(0.995,-100,100,100,bg="white",density=NA,col="white")
text(-.015,1:56,sprintf("%02d",56:1),cex=0.5)
box()
```
