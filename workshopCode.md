
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
source(paste0("https://raw.githubusercontent.com",
              "/nolauren/nlpworkshop/master/corenlp_funs.R"))
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
setwd("C:/Users/your_user_name/Desktop/annotations")
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
Do the results surprise you or reveal anything particular about the story?

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
          (toupper(token$token) != token$token)[dep$govIndex] &
          (toupper(token$token) != token$token)[dep$depIndex])
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
  out <- paste(token$token[these[1]:these[2]],collapse=" ")
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

Now, lets took at the results of `getToken` in more detail. There are a number 
of columns that we ignored previously that contain the named entity recognition
information:
```{r}
anno <- readRDS("holmes/01_a_scandal_in_bohemia.Rds")
token <- getToken(anno)
table(token$NER)
```
Take note of all the different categories that are supplied. We will look at a 
few of these today.

### Location

The location NER in this story picks up a number of places that are referenced
or visited in the story:
```{r}
unique(token$lemma[token$NER=="LOCATION"])
```

### Date and numbers

The date entity recognition can be seen in action in sentence 18, where it converts
a long form data into a standard ISO-8601 format:
```{r}
head(token[which(token$NER=="DATE"),])
```
And likewise the number entity converts written forms of numbers into numeric values:
```{r}
head(token[which(token$NER=="NUMBER"),])
```

### Person

In order to detect characters, we can make use of the person named entity tag.
The following code snippet uses this information to filter out `pnames` list
of characters to remove proper names that do not appear to be people:
```{r}
these <- which(token$NER == "PERSON")
pnames <- pnames[which(pnames$endIndex %in% these),]
these <- these[-which(these %in% pnames$endIndex)]
newPnames <- data.frame(pname=token$token[these],
                  startIndex=these,
                  endIndex=these,
                  stringsAsFactors=FALSE)
pnames <- rbind(pnames,newPnames)
pnames <- pnames[toupper(pnames$pname) != pnames$pname,]
unique(pnames$pname)
```
Note that this code block will only run correctly if you ran the code in the previous
section to construct the original `pnames` dataset.

## Coreferences

The final step in the NLP pipeline that we will look at today can be extracted by
using the `getCoreference` function. We read all of the data back in, again just 
in case something gets modified or deleted:
```{r}
anno <- readRDS("holmes/01_a_scandal_in_bohemia.Rds")
token <- getToken(anno)
coref <- getCoreference(anno)
head(coref)
```
Which tokens are associated with `corefId` number 1? 
```{r}
table(token$token[coref$startIndex[coref$corefId == 1]])
```
We can alternatively look at the set of lemmas associated with this id:
```{r}
table(token$lemma[coref$startIndex[coref$corefId == 1]])
```
What character seems to be associated with id 1?

### Mapping ID to Name

To more methodically map `corefId` to a character, we first see which coreferences
seem to directly map to our table of characters:
```{r}
index <- match(as.numeric(coref$endIndex), pnames$endIndex)
```
We then create the mapping from ID to character name:
```{r}
temp <- tapply(pnames$pname[index[!is.na(index)]],
              coref[!is.na(index),1],
              function(v) {names(rev(sort(table(v))))[1]} )
perMap <- data.frame(corefId=names(temp),perName=temp)
perMap
```

### Most important characters

We can now combine corefID's that appear to map to the same character (remember,
coreference resolution is an active field of research and far from perfect). A
rought count of how often each character is mentioned is then given by:
```{r}
tab <- table(coref[,1])
index <- match(perMap$corefId, names(tab))
perMap$count <- tab[index]
charImport <- sort(tapply(perMap$count, perMap$perName, sum),TRUE)
charImport
```
This should seem reasonable, but is again far from perfect. Dr. Watson for instance
is almost certainly under counted, as he also the narrator of the story and should
be associated with any use of first person pronouns outside of quotes.

## Generalize to other stories (time permitting)

We have up to now only looked at one of the Sherlock Holmes short stories. How would
our analysis of the NLP pipeline look different with another one of the stories? We
can evaluate this by replacing the code snippet `anno <- readRDS("holmes/01_a_scandal_in_bohemia.Rds")`,
with any of the following:
```{r}
anno <- readRDS("holmes/01_a_scandal_in_bohemia.Rds")
anno <- readRDS("holmes/02_the_red_headed_league.Rds")
anno <- readRDS("holmes/03_a_case_of_identity.Rds")
anno <- readRDS("holmes/04_the_boscombe_valley_mystery.Rds")
anno <- readRDS("holmes/05_the_five_orange_pips.Rds")
anno <- readRDS("holmes/06_the_man_with_the_twisted_lip.Rds")
anno <- readRDS("holmes/07_the_blue_carbuncle.Rds")
anno <- readRDS("holmes/08_the_speckled_band.Rds")
anno <- readRDS("holmes/09_the_engineers_thumb.Rds")
anno <- readRDS("holmes/10_the_noble_bachelor.Rds")
anno <- readRDS("holmes/11_the_beryl_coronet.Rds")
anno <- readRDS("holmes/12_the_copper_beeches.Rds")
anno <- readRDS("holmes/13_silver_blaze.Rds")
anno <- readRDS("holmes/14_the_yellow_face.Rds")
anno <- readRDS("holmes/15_the_stock_brokers_clerk.Rds")
anno <- readRDS("holmes/16_the_gloria_scott.Rds")
anno <- readRDS("holmes/17_the_musgrave_ritual.Rds")
anno <- readRDS("holmes/18_the_reigate_puzzle.Rds")
anno <- readRDS("holmes/19_the_crooked_man.Rds")
anno <- readRDS("holmes/20_the_resident_patient.Rds")
anno <- readRDS("holmes/21_the_greek_interpreter.Rds")
anno <- readRDS("holmes/22_the_naval_treaty.Rds")
anno <- readRDS("holmes/23_the_final_problem.Rds")
anno <- readRDS("holmes/24_the_empty_house.Rds")
anno <- readRDS("holmes/25_the_norwood_builder.Rds")
anno <- readRDS("holmes/26_the_dancing_men.Rds")
anno <- readRDS("holmes/27_the_solitary_cyclist.Rds")
anno <- readRDS("holmes/28_the_priory_school.Rds")
anno <- readRDS("holmes/29_black_peter.Rds")
anno <- readRDS("holmes/30_charles_augustus_milverton.Rds")
anno <- readRDS("holmes/31_the_six_napoleons.Rds")
anno <- readRDS("holmes/32_the_three_students.Rds")
anno <- readRDS("holmes/33_the_golden_pince_nez.Rds")
anno <- readRDS("holmes/34_the_missing_three_quarter.Rds")
anno <- readRDS("holmes/35_the_abbey_grange.Rds")
anno <- readRDS("holmes/36_the_second_stain.Rds")
anno <- readRDS("holmes/37_the cardboard box.Rds")
anno <- readRDS("holmes/38_wisteria lodge.Rds")
anno <- readRDS("holmes/39_the bruce-partington.Rds")
anno <- readRDS("holmes/40_the devils foot.Rds")
anno <- readRDS("holmes/41_the red circle.Rds")
anno <- readRDS("holmes/42_the disappearance of lady frances.Rds")
anno <- readRDS("holmes/43_the dying.Rds")
anno <- readRDS("holmes/44_his last bow.Rds")
anno <- readRDS("holmes/45_the_illustrious_client.Rds")
anno <- readRDS("holmes/46_the_blanched_soldier.Rds")
anno <- readRDS("holmes/47_the_mazarin_stone.Rds")
anno <- readRDS("holmes/48_the_three_gables.Rds")
anno <- readRDS("holmes/49_the_sussex_vampire.Rds")
anno <- readRDS("holmes/50_the_three_garridebs.Rds")
anno <- readRDS("holmes/51_the_problem_of_thor_bridge.Rds")
anno <- readRDS("holmes/52_the_creeping_man.Rds")
anno <- readRDS("holmes/53_the_lion's_mane.Rds")
anno <- readRDS("holmes/54_the_veiled_lodger.Rds")
anno <- readRDS("holmes/55_shoscombe_old_place.Rds")
anno <- readRDS("holmes/56_the_retired_colourman.Rds")
```
Pick a new story and rerun all of the analysis that we have done. How well
does our code do on picking up interesting features of this new story? Are
there any news errors that crop up? 

## Sherlock Holmes Short Stories

### Detecting main characters

Let's put all that we have learned today together to extract the most important
character (other than Sherlock or Dr. Watson) from all the 56 short stories.
The code chunk to do it is below. Don't be overwhelmed by its complexity; just 
copy, run, and analyze the results at first. It is really just all of the code
we have already seen just put into a single block:
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
            (toupper(token$token) != token$token)[dep$govIndex] &
            (toupper(token$token) != token$token)[dep$depIndex])
  nnDep = dep[index,]

  pname = startIndex = endIndex = NULL
  for (g in unique(nnDep$govIndex)) {
    these = c(which(nnDep$depIndex == g),
              which(nnDep$govIndex == g))
    these = range(c(nnDep$depIndex[these],nnDep$govIndex[these]))
    index = these[1]:these[2]
    words = token$token[index][token$POS[index] != "." &
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
  newPnames = data.frame(pname=token$token[these],
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
The list of characters is then given by:
```{r}
output
```

### Plotting character locations

We can now make a plot of when each of these characters is mentioned in each
story:
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
How well do these explain the narrative arc of each short story?

## Annotating your own text

### Getting set-up

To annotate text yourself (that is, to apply the NLP pipeline to new text) you
will need to install the **coreNLP** package that we wrote. This needs to be done
just once, by running following code in R:
```{r}
install.packages('coreNLP')
```
Choose a mirror site and the installation should run in less a couple of minutes.

You then need to download the java files from the coreNLP website. This should
also be done from within R, with the following command:
```{r}
coreNLP::downloadCoreNLP()
```
The file is about 350MB; this could take a while depending on your internet connection
speed.

Now, any time you want to annotate text, run the following lines from within R:
```{r}
library(coreNLP)
initCoreNLP()
```
As long as the last line spits out a bunch of text that suggests various things are
being loaded, you are ready to go. If you get an error, please ask or e-mail us directly
for assistance, or see the discussion on:
[https://github.com/statsmaths/coreNLP](https://github.com/statsmaths/coreNLP). Problems
almost always have to do with setting up Java correctly, and are rarely a function of
issues with the **coreNLP** package directly.

### Running the annotation

Once you have set up the package correctly and loaded and initalized the annotation
engine:
```{r}
library(coreNLP)
initCoreNLP()
```
You can annotate text by using the `annotateString` function:
```{r}
sIn <- "Mother died today. Or, maybe, yesterday; I can't be sure."
anno <- annotateString(sIn)
anno
```
The `anno` object can be used the exact same way the annotation objects we loaded
directly during the workshop. If you have a lot of text, you can also call `annotateFile`
and point it directly at a file on our computer.

### Other languages

In order to run the NLP pipeline on other languages, you need to create a text file
that contains a list of properties that change the default behavior of the annotator.
For example, if you create a file `french.properties` (in a text editor, not a word
processor!) with the following:
```
annotators = tokenize, ssplit
tokenize.language = fr
```
And then in R run:
```{r}
library(coreNLP)
initCoreNLP(parameterFile="path/to/french.properties")
```
Then, subsequent calls to `annotateString` and `annotateFile` should apply the
French parser to the code. We are working on a way of making this easier in a
future version of the package.

