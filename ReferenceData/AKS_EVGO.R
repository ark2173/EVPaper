GO_Search_EVs <-function(SigTab,AllGenes_Names){
  library(topGO)
  expressed.genes<-rownames(SigTab)
  # define geneList as 1 if gene is in expressed.genes, 0 otherwise
  geneList <- ifelse(AllGenes_Names %in% expressed.genes, 1, 0)
  names(geneList) <- AllGenes_Names

  # Create topGOdata object
  GOdata_BP <- new("topGOdata",
                   ontology = "BP", # use biological process ontology
                   allGenes = geneList,
                   geneSelectionFun = function(x)(x == 1),
                   annot = annFUN.org, mapping = "org.Mm.eg.db", ID = "symbol")
  
  resultFisher_BP <- runTest(GOdata_BP, algorithm = "classic", statistic = "fisher")
  pval_BP<-as.data.frame(score(resultFisher_BP))
  colnames(pval_BP)<-"PVal"
  pval_BP$GO.ID<-rownames(pval_BP)
  
  GO_Fisher_BP<-GenTable(GOdata_BP, Fisher = resultFisher_BP, numChar = 1000,topNodes=resultFisher_BP@geneData["SigTerms"])
  GO_Fisher_BP$Group<-"Biological Process"
  

  ###KS Stat###
  # define geneList as 1 if gene is in expressed.genes, 0 otherwise
  geneList <- ifelse(AllGenes_Names %in% expressed.genes, SigTab$PValue, 0)
  names(geneList) <- AllGenes_Names
  
  # Create topGOdata object
  GOdata_BP <- new("topGOdata",
                   ontology = "BP", # use biological process ontology
                   allGenes = geneList,
                   geneSelectionFun = function(x)(x == 1), annot = annFUN.org, mapping = "org.Mm.eg.db", ID = "symbol")
  
  resultKS_BP <- runTest(GOdata_BP, algorithm = "classic", statistic = "ks")
  pval_BP<-as.data.frame(score(resultKS_BP))
  colnames(pval_BP)<-"PVal"
  pval_BP$GO.ID<-rownames(pval_BP)
  
  GO_KS_BP<-GenTable(GOdata_BP, KS = resultKS_BP, topNodes=resultKS_BP@geneData["SigTerms"], numChar = 1000)
  GO_KS_BP$Group<-"Biological Process"
  
  tokeep<-intersect(GO_KS_BP$GO.ID,GO_Fisher_BP$GO.ID)
  Results_KS<-GO_KS_BP[GO_KS_BP$GO.ID %in% tokeep,]
  Fisher_Results<-GO_Fisher_BP[GO_Fisher_BP$GO.ID %in% tokeep,]
  Results<-merge(Fisher_Results,Results_KS,by=c("GO.ID","Term","Annotated","Group"))
  return(Results)
}