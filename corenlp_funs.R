print.annotation = function(x, ...) {
  cat("\nA CoreNLP Annotation:\n")
  cat("  num. sentences:", x$token[nrow(x$token),1], "\n")
  cat("  num. tokens:", nrow(x[[1]]), "\n")
  cat("\n")
  invisible(x)
}

getToken = function(annotation) {
  annotation$token
}

getParse = function(annotation) {
  annotation$parse
}

getDependency = function(annotation, type=c("CCprocessed","basic","collapsed")) {
  type = match.arg(type)
  dep <- if (type == "basic") annotation$basicDep
  else if (type == "collapsed") annotation$collapsedDep
  else if (type == "CCprocessed") annotation$collapsedProcDep

  dep$govIndex = match(paste0(dep$sentence,"-",dep$governorIdx),
                     paste0(annotation$token$sentence,"-",annotation$token$id))
  dep$depIndex = match(paste0(dep$sentence,"-",dep$dependentIdx),
                     paste0(annotation$token$sentence,"-",annotation$token$id))
  dep
}


getSentiment = function(annotation) {
  annotation$sentiment
}

getCoreference = function(annotation) {
  coref = annotation$coref[,-grep("text",names(annotation$coref))]
  coref$startIndex = match(paste0(coref$sentence,"-",coref$start),
                     paste0(annotation$token$sentence,"-",annotation$token$id))
  coref$endIndex = match(paste0(coref$sentence,"-",as.numeric(coref$end)-1),
                     paste0(annotation$token$sentence,"-",annotation$token$id))
  coref
}

loadXMLAnnotation = function(file, encoding="unknown") {
  if (is.character(file)) {
    file = file(file, "r")
    on.exit(close(file))
  }
  x = readLines(file)
  tryCatch(output <- parseAnnoXML(x),
           error = function(e) stop("Not a valid CoreNLP XML file."))
  output
}

universalTagset = function(pennPOS) {
  mtab = structure(c("!", "#", "$", "''", "(", ")", ",", "-LRB-",
      "-RRB-",  ".", ":", "?", "CC", "CD", "CD|RB", "DT", "EX", "FW", "IN",
      "IN|RP",  "JJ", "JJR", "JJRJR", "JJS", "JJ|RB", "JJ|VBG", "LS", "MD", "NN",
      "NNP", "NNPS", "NNS", "NN|NNS", "NN|SYM", "NN|VBG", "NP", "PDT",  "POS", "PRP",
      "PRP$", "PRP|VBP", "PRT", "RB", "RBR", "RBS", "RB|RP",  "RB|VBG", "RN", "RP", "SYM",
      "TO", "UH", "VB", "VBD", "VBD|VBN",  "VBG", "VBG|NN", "VBN", "VBP", "VBP|TO",
      "VBZ", "VP", "WDT",  "WH", "WP", "WP$", "WRB", "``", ".", ".", ".", ".", ".",
      ".",  ".", ".", ".", ".", ".", ".", "CONJ", "NUM", "X", "DET", "DET",  "X",
      "ADP", "ADP", "ADJ", "ADJ", "ADJ", "ADJ", "ADJ", "ADJ",  "X", "VERB", "NOUN",
      "NOUN", "NOUN", "NOUN", "NOUN", "NOUN",  "NOUN", "NOUN", "DET", "PRT", "PRON",
      "PRON", "PRON", "PRT",  "ADV", "ADV", "ADV", "ADV", "ADV", "X", "PRT", "X",
      "PRT", "X",  "VERB", "VERB", "VERB", "VERB", "VERB", "VERB", "VERB", "VERB",
      "VERB", "VERB", "DET", "X", "PRON", "PRON", "ADV", "."), .Dim = c(68L,  2L))

  index = match(pennPOS, mtab[,1])
  output = rep("X", length(pennPOS))
  output[!is.na(index)] = mtab[index[!is.na(index)],2]
  output
}

fillDF = function(df, nameVec) {
  if (nrow(df) == 0L) return(df)
  index = match(nameVec, names(df))
  df[,nameVec[is.na(index)]] = NA
  return(df)
}

