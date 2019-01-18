tree <- read.tree(text = 
"(E.-Bantu(
  Shona,
  S.-Bantu-Makua(
    Chopi,
    Nguni-Tsonga(
      Nguni(
        Sumayela-Ndebele,
        S.-Ndebele(
          Zimbabwe-Ndelebe,
          Zulu-Xhosa(
          Zulu,
          Xhosa))))
    Sotho-Makua-Venda(
      Venda,
      Sotho-Makua(
        Sotho-Tswana(
          Sotho,
          Tswana)
        N.-Mozambique-Bantu)))));"
)

plot(tree)

#using data.tree
library(data.tree)

EaBa = Node$new("East Bantu")
  Shona = EaBa$AddChild("Shona")
  SoBaMa = EaBa$AddChild("S. Bantu-Makua")
    Chopi = SoBaMa$AddChild("Chopi")
    NgTs = SoBaMa$AddChild("Nguni-Tsonga")
      Nguni = NgTs$AddChild("Nguni")
        SuNd = Nguni$AddChild("SUMAYELA NDEBELE")
        SoNd = Nguni$AddChild("S. Ndebele")
          ZiNd = SoNd$AddChild("ZIMBABWE NDEBELE")
          ZuXh = SoNd$AddChild("Zulu-Xhosa")
            Zulu = ZuXh$AddChild("ZULU")
            Xhosa = ZuXh$AddChild("XHOSA")
    SoMaVe = SoBaMa$AddChild("Sotho-Makua-Venda")
      Venda = SoMaVe$AddChild("Venda")
      SoMa = SoMaVe$AddChild("Sotho-Makua")
        NMB = SoMa$AddChild("N. Mozambique Bantu")
        SoTs = SoMa$AddChild("Sotho-Tswana")
          Sotho = SoTs$AddChild("SOTHO")
          Tswana = SoTs$AddChild("TSWANA")

plot(EaBa)          
plot(as.phylo(EaBa),direction="downwards",show.node.label = T,cex=0.7,srt=45)
plot(as.igraph(EaBa, directed = TRUE, direction = "climb"))      
d=as.igraph(EaBa, directed = TRUE, direction = "climb")

acme <- Node$new("Acme Inc.")
accounting <- acme$AddChild("Accounting")
software <- accounting$AddChild("New Software")
standards <- accounting$AddChild("New Accounting Standards")
research <- acme$AddChild("Research")
newProductLine <- research$AddChild("New Product Line")
newLabs <- research$AddChild("New Labs")
it <- acme$AddChild("IT")
outsource <- it$AddChild("Outsource")
agile <- it$AddChild("Go agile")
goToR <- it$AddChild("Switch to R")

print(acme)