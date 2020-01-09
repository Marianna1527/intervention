# Analyse des ventes du produit A (sofdrinks)

# Loading packages
require(tidyverse)
require(openxlsx)
require(jarvis)
require(ggpubr)

# Loading data 
data = read.xlsx("./data/boissons.xlsx")

# Exploration des données 
data = data[-c(1:19),]
data$y = as.factor(rep(1:2, each = 52))
data$t = 1:nrow(data)
# Questions :
# Graphiquedes ventes
g1 = data %>% 
    ggplot(aes(y = Ventes.boisson.A, x = t, col = y)) +
        geom_path()
# Evolution des ventes entre 2011 (septembre 2010 - 2011, 52 semaines glissantes) et 2010 (septembre 2009 - 2010), en poucentage
res = data %>% 
    group_by(y) %>%
    summarise(ventes.A = mean(Ventes.boisson.A)) 
res2 = data %>% 
    group_by(y) %>%
    summarise(ventes.A = mean(Vente.Total.boisson))
pourc = 100-res[1,2]/res[2,2]*100
pourct = 100-res2[1,2]/res2[2,2]*100
pourc ; pourct
# Graphique ventes vs promotions 
g2 = data %>% 
    mutate(
        VentesAscale = Ventes.boisson.A/mean(data$Ventes.boisson.A),
        PromAscale = Promotion.parfum.1.gratuit/mean(data$Promotion.parfum.1.gratuit),
        AvantAscale = Mise.en.avant.parfum.1/mean(data$Mise.en.avant.parfum.1)) %>%
    ggplot(aes(
        y = VentesAscale,
        x = t, col = "Ventes")) +
    geom_path() +
    geom_path(aes(
        y = PromAscale,
        x = t, col = "Promotion")) + 
    geom_path(aes(
        y = AvantAscale,
        x = t, col = "Mise en avant"))
# Graphique ventes vs mise en avant 
    # Regader en haut (un graphique commun)
# Quel est le facteur qui impacte le plus ?
    # Les promotions ?
    # La mise en avant est plus correlé théoriquement
    # A trouver une autre façon à le presenter
# Pour 2011 calculer les coefficients de correlation entre les ventes et la mise en avant
cor_data = data %>% 
    filter(y == 2) %>%
    select(Ventes.boisson.A,
        Promotion.parfum.1.gratuit,
        Mise.en.avant.parfum.1) %>%
    mutate_all(as.numeric)
correlation = cor(cor_data)
    # Une forte correlation positive avec la mise en avant du produit 1
# Graphique ventes vs médias (quel est le média ayant le plus d'effet ?)
g3 = data %>% 
    mutate(
        VentesAscale = Ventes.boisson.A/mean(data$Ventes.boisson.A),
        TV = GRP.TV/mean(data$GRP.TV),
        Presse = GRP.Presse/mean(data$GRP.Presse)) %>%
    ggplot(aes(
        y = VentesAscale,
        x = t, col = "Ventes")) +
    geom_path() +
    geom_path(aes(
        y = TV,
        x = t, col = "GRP TV")) + 
    geom_path(aes(
        y = Presse,
        x = t, col = "GRP Presse"))
# Comment évolue la pression TV et la pression presse entre 2010 et 2011
res = data %>%
    group_by(y) %>%
    summarise(TV = sum(GRP.TV),
        Presse = sum(GRP.Presse))
p.TV = 100-res[1,2]/res[2,2]*100
p.Presse = 100-res[1,3]/res[2,3]*100
p.TV ; p.Presse
    # On observe une diminution significative de GRP TV avec une augmentation de GRP Presse
# Graphique ventes vs méteo (la météo a-t-elle un impact sur les ventes ?)
g4 = data %>% 
    mutate(
        VentesAscale = Ventes.boisson.A/mean(data$Ventes.boisson.A),
        Meteo = Températures/mean(data$Températures)) %>%
    ggplot(aes(
        y = VentesAscale,
        x = t, col = "Ventes")) +
    geom_path() +
    geom_path(aes(
        y = Meteo,
        x = t, col = "Meteo"))
    # Meteo doit jouer le rîle le plus important dans les ventes des soft-drinks
# Calculer la temperature moyenne en 04/04-12/09 de 2010 (et le même pour 2011)
tm10 = mean(data$Températures[30:42])
tm11 = mean(data$Températures[(30+52):(42+52)])
tm10 ; tm11 
    # On observe une température identique
# Graphiques des ventes vs mise en avant la concurrence (MEA CC boisson pour les concurrents)
p1 = data %>% 
    mutate(
        VentesAscale = Ventes.boisson.A/mean(data$Ventes.boisson.A),
        MEA1 = MEA.CC.boisson.1/mean(data$MEA.CC.boisson.1)) %>%
    ggplot(aes(
        y = VentesAscale,
        x = t, col = "Ventes")) +
    geom_path() +
    geom_path(aes(
        y = MEA1,
        x = t, col = "MEA 1"))
p2 = data %>% 
    mutate(
        VentesAscale = Ventes.boisson.A/mean(data$Ventes.boisson.A),
        MEA2 = MEA.CC.boisson.2/mean(data$MEA.CC.boisson.2)) %>%
    ggplot(aes(
        y = VentesAscale,
        x = t, col = "Ventes")) +
    geom_path() +
    geom_path(aes(
        y = MEA2,
        x = t, col = "MEA 2"))
p3 = data %>% 
    mutate(
        VentesAscale = Ventes.boisson.A/mean(data$Ventes.boisson.A),
        MEA3 = MEA.CC.boisson.3/mean(data$MEA.CC.boisson.3)) %>%
    ggplot(aes(
        y = VentesAscale,
        x = t, col = "Ventes")) +
    geom_path() +
    geom_path(aes(
        y = MEA3,
        x = t, col = "MEA 3"))
g5 = ggarrange(p1, p2, p3, nrow = 2, ncol = 2)
# Emettre les hypotheses et les vérifier par la modèlisation

# Important : 
    # Conserver des couleurs identiques pour l'integralité de travail
    # Ne jamais travailler sur les données raw (si on travaille en Excel)
    # Toujours ajouter et respecter les regles de la presentation (titres, axes , etc)

# Saisonnalité
data$s = rep(1:52, times = 2)
sais = data %>% 
    group_by(s) %>%
    arrange(s) %>%
    summarise(VentesS = mean(Vente.Total.boisson)/mean(data$Vente.Total.boisson)) %>%
    mutate(t = 1:52)
sais %>% ggplot(aes(y = VentesS, x = t)) +
    geom_path()
siasX = rep(sais$VentesS, times = 3)
siasSX = siasX %>%
    as.data.frame()
colnames(siasSX) = c("VentesS")
saisonnality = siasSX %>%
    mutate(VentesSM = (VentesS + 
        lag(VentesS) + lag(VentesS, 2) + lag(VentesS, 3) + 
        lead(VentesS) + lead(VentesS, 2) + lead(VentesS, 3)
        )/7) %>% 
    select(VentesSM)
data$saison = rep(saisonnality[53:104,1], times = 2)
data %>% 
    ggplot(aes(y = Vente.Total.boisson/mean(data$Vente.Total.boisson), x = t, col = "Ventes")) +
    geom_path() +
    geom_path(aes(y = saison, x = t, col = "Saison"))

# Modèlisation
    # Comment introduire la saisonnalité une fois calculée ?
    