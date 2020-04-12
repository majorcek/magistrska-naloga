library(ggplot2)

c <- 3.0

vrednosti <- c(-1, 35)
verjetnosti <- c(36/37, 1/37)
upanje <- as.numeric(vrednosti %*% verjetnosti)
varianca <- as.numeric((vrednosti - upanje) ** 2 %*% verjetnosti)
sd <- sqrt(varianca)
koef_asimetrije <- as.numeric(abs(vrednosti - upanje) ** 3 %*% verjetnosti)


x <- 0
n <- 100000

## verjetnost, da nismo v minusu

verjetnost_uspeha <- function(n){
  1 - pbinom(n %/% 36 + as.integer(n %% 36 > 0) - 1, size = n, prob = 1/37) 
}

tabela_uspeha <- data.frame(c(1:n))
names(tabela_uspeha) <- c("krog")
tabela_uspeha$verjetnost <- verjetnost_uspeha(tabela_uspeha$krog)
ggplot(data = tabela_uspeha) + 
  geom_line(aes(x = krog, y = verjetnost))


#### dodamo meji
#zgornja meja
zgornja_meja <- function(n){
  x = sqrt(n) * (-upanje) / sd
  1 - pnorm(x) + (c * koef_asimetrije) / ((n**(1/2)) * (sd**3))
}

spodnja_meja <- function(n){
  x = sqrt(n) * (-upanje) / sd
  1 - pnorm(x) - (c * koef_asimetrije) / ((n**(1/2)) * (sd**3))
}

tabela_uspeha$zg_meja <- zgornja_meja(tabela_uspeha$krog)
tabela_uspeha$sp_meja <- spodnja_meja(tabela_uspeha$krog)

ggplot(data = tabela_uspeha) + 
  geom_line(aes(x = krog, y = verjetnost, color = 'verjetnost')) + 
  geom_line(aes(x = krog, y = zg_meja, color = 'zgornja meja')) + 
  geom_line(aes(x = krog, y = sp_meja, color = 'spodnja meja')) + 
  scale_color_manual(values = c(
    "verjetnost" = 'red', 
    "zgornja meja" = 'darkgreen',
    "spodnja meja" = 'brown')) + 
  labs(color = "")
