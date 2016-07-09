# An Introduction to Natural Language Processing

## Tokenization

```{r}
sIn <- "Mother died today. Or, maybe, yesterday; I can't be sure."
```

```{r}
strsplit(sIn, split=" ")
```

```{r}
install.packages("coreNLP")
library(coreNLP)
```

Or

```{r}
source("https://github.com/nolauren/")
```

```{r}
annotation <- readRDS("simple_annotation.Rds")
annotation
```

```{r}
getToken(annotation)$token
```

```{r}
getToken(annotation)$sentence
```

```{r}
strIn <- "Aujourd'hui, maman est morte. Ou peut-être hier, je ne sais pas. J'ai reçu un télégramme de l'asile."
```

```{r}
annoEnglish <- readRDS("annoEnglish.Rds")
getToken(annoEnglish)$token
```

```{r}
annoFrench <- readRDS("annoFrench.Rds")
getToken(annoFrench)$token
```

```{r}
anno <- readRDS("holmes/01_a_scandal_in_bohemia.Rds")
anno
```

```{r}
sentLen <- table(getToken(anno)$sentence)
hist(sentLen, breaks=30)
```



## Lemmatization and Part of Speech Tagging

```{r}
token <- getToken(anno)
token[token$sentence==2,c(1:4,7)]
```

```{r}
ut <- universalTagset(token$POS)
table(ut)
unique(token$POS[ut == "NOUN"])
unique(token$POS[ut == "VERB"])
```

```{r}
nounCnt <- tapply(ut == "NOUN", token$sentence, sum)
pronCnt <- tapply(ut == "PRON", token$sentence, sum)
adjCnt <- tapply(ut == "ADJ", token$sentence, sum)
verbCnt <- tapply(ut == "VERB", token$sentence, sum)
posDf <- data.frame(nounCnt,pronCnt,adjCnt,verbCnt)
head(posDf)
```

```{r}
par(mfrow=c(1,2))
plot(nounCnt + pronCnt, adjCnt, pch=19, cex=1, col=rgb(0,0,1,0.02))
plot(nounCnt + pronCnt, verbCnt, pch=19, cex=1, col=rgb(0,0,1,0.02))
```

```{r}
index <- which(ut=="NOUN")
tab <- table(token$lemma[index])
head(sort(tab,decreasing=TRUE),25)
```

```{r}
index <- which(token$POS == "NNP")
tab <- table(token$lemma[index])
head(sort(tab,decreasing=TRUE),25)
```

## Dependencies

```{r}
dep <- getDependency(anno)
dep[dep$sentence == 1,]
```

```{r}
index <- which(token$lemma[dep$depIndex] == "I")
depSelf <- dep[index,]
depSelf <- depSelf[depSelf$type == "nsubj",]
sort(table(depSelf$governor),decreasing=TRUE)[1:10]
```

```{r}
index <- which(dep$type == "nn" &
          token$POS[dep$govIndex] == "NNP" &
          token$POS[dep$depIndex] == "NNP" &
          (toupper(token$word) != token$word)[dep$govIndex] &
          (toupper(token$word) != token$word)[dep$depIndex])
nnDep <- dep[index,]
```

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

```{r}
unique(pnames$pname)
```


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
