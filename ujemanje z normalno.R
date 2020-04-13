library(ggplot2)

dolzina_igre <- 1000
stevilo_iger <- 1000

vrednosti <- c(-1, 35)
verjetnosti <- c(36/37, 1/37)
upanje <- as.numeric(vrednosti %*% verjetnosti)
varianca <- as.numeric(vrednosti ** 2 %*% verjetnosti - upanje ** 2)
sd <- sqrt(varianca)
skew <- as.numeric(abs(vrednosti - upanje) ** 3 %*% verjetnosti)



### Naredimo tabelo iger

tabela <- data.frame()
i <- 1
while (i <= stevilo_iger){
  izkupicki <- sample(vrednosti, dolzina_igre, replace = TRUE, prob = verjetnosti)
  tabela_poizkusov <- data.frame(seq_len(dolzina_igre), izkupicki)
  names(tabela_poizkusov) <- c("st.poizkusa", "izkupicek")
  tabela_poizkusov$tekoci_izkupicek <- cumsum(tabela_poizkusov$izkupicek)
  if (i==1){
    tabela <- data.frame(tabela_poizkusov$tekoci_izkupicek)
  }else{
    tabela[,ncol(tabela) + 1] <- tabela_poizkusov$tekoci_izkupicek
  }
  
  i <- i + 1
}
colnames(tabela) <- c(1:stevilo_iger)

##########

#### Analiza povrečji
povprecja <- rowMeans(tabela)
tabela_povprecji <- data.frame(seq(dolzina_igre), povprecja)
names(tabela_povprecji) <- c("n", "povprecje")
ggplot(tabela_povprecji) +
  geom_line(aes(x = n, y = povprecje))
################# 


### STANJE PO ZADNJI PONOVITVI V VSAKI IGRI 
n <- dolzina_igre
vrstica_n <- tabela[n,]
zbrane_verjetnosti <- data.frame("x" = numeric(), "verjetnost" = numeric(), "standardno_normalna" = numeric())
x_ticks <- seq(from = -5, to = 5, by = 0.05)

for (x in x_ticks){
  verjetnost <- length(vrstica_n[(vrstica_n - n * (-1/37)) / sqrt(n*varianca) <= x]) / stevilo_iger
  normalna <- pnorm(x, mean = 0, sd = 1)
  par <- data.frame(x, verjetnost, normalna)
  names(par) <- c("x", "verjetnost", "normalna")
  zbrane_verjetnosti <- rbind(zbrane_verjetnosti, par)
}


ggplot(data = zbrane_verjetnosti) +
  geom_line(aes(x = x, y = normalna, color = 'normalna porazdelitev')) + 
  geom_line(aes(x = x, y = verjetnost, color = 'podatki iz rulete')) + 
  scale_color_manual(values = c(
    "podatki iz rulete" = 'blue', 
    "normalna porazdelitev" = 'orange')) + 
  labs(title = "primerjava vzorca z normalno porazdelitvijo ",
       x = "x", y = "P(X<=x)", color = "")


  
