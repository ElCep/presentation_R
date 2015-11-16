#auteur : Etienne DELAY laboratoire GeoLab, université de Limoges
# cette foinction fait appel à la librairie korpus et au moteur de lemmatisation 
# treetagger : http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/

require(koRpus)

lemmatisation <- function(my.df){
  ##my.df est un objet Corpus issu de du chargement du corpus avec tm
  print(my.df)
  dictionnaire <- data.frame()
  for(i in 1 : length(my.df)){
    lemma <- treetag(corp[[i]][[1]], treetagger = "manual", format = "obj", TT.tknz = FALSE, 
                     lang = "fr", TT.options = list(path = "treetagger", preset = "fr-utf8"))
    dictionnaire <- rbind(dictionnaire, lemma@TT.res )
  }
  return(unique(dictionnaire))
}