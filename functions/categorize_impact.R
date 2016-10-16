#### Categorizando e medindo impacto em VD ####


#### 1. treeCat (Categorizando usando árvores) ####
treeCat <- function(x, y, maxCats = 20, maxdepth = 3){
  # x <- dfGB$int_rate
  # y <- dfGB$loan_status
  #Carregando pacote para encaixar modelo de árvores condicionais
  require(partykit)
  
  #Encaixando modelo
  fitTree <- ctree(y~x, control = ctree_control(maxdepth = maxdepth))
  
  nNodes <- length(nodeids(fitTree))
  
  #Avaliando se não houve splits ou se houve splits demais
  if(nNodes == 1){
    return("Sem splits")
  } else if (nNodes > maxCats){
    return(paste("Muitas categorias: ", nNodes))
  }
  
  # plot(fitTree)
  
  #Inicianco vars do loop para identificar onde estão os splits
  treeSplits <- numeric()
  count <- 1
  for(i in (1:nNodes)[-nodeids(fitTree, terminal = TRUE)]){
    treeSplits[count] <- split_node(node_party(fitTree[[i]]))$breaks
    count <- count+1
  }
  
  #Definindo breaks para func cut()
  breaks <- unique(c(min(x, na.rm = TRUE)-1, 
                     sort(treeSplits),
                     max(x, na.rm = TRUE)+1))
  
  #Categorizando com base nos breaks
  xCat <- cut(x, breaks = breaks, right = TRUE, dig.lab = 10)
  
  return(xCat)
}

#### 2. crossImpact() impacto ao cruzar VD com vI categórica ####
crossImpact <- function(vi, vd){
  # vi <- dfCat$loan_amnt
  # vd <- dfGB$loan_status
  
  minN <- min(table(vi))
  maxN <- max(table(vi))
  
  propsVD <- prop.table(table(vd))
  propsCross <- table.desc(vi, vd)[1:length(levels(vi)),c(4:(4+(length(levels(vd))-1)))]*.01
  
  
  out <- round(sqrt(mean((sweep(propsCross, 2, propsVD))^2))*100, 2)
  
  return(c(impact = out, minN = minN, maxN = maxN, nLevels = length(levels(vi))))
}