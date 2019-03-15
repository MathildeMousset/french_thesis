# theses point fr
# rvest

library(RCurl)
library(tidyverse)
library(dplyr)
library(purrr)
library(lubridate)
library(scales)
library(hrbrthemes)

# Ici on indique le nombre de thèse en ligne, 
# divisé par 1000 en arrondissant au millier supérieur. 
# (theses.fr pagine par tranches de 1000)
# i <- 1:2
# i <- i*10
#URL <- paste0("http://www.theses.fr/?q=&fq=dateSoutenance:[2015-01-01T23:59:59Z%2BTO%2B2018-12-31T23:59:59Z]&checkedfacets=&start=", i, "&sort=none&status=&access=&prevision=&filtrepersonne=&zone1=titreRAs&val1=&op1=AND&zone2=auteurs&val2=&op2=AND&zone3=etabSoutenances&val3=&zone4=dateSoutenance&val4a=&val4b=&type=&lng=&checkedfacets=&format=csv")
# on cherche les thèses soutenues après 2015
# il y a environ 86960 thèses (au 1/12/2018)

# 373288 

i <- 1:400
i <- i*1000

URL <- paste0("https://www.theses.fr/?q=&fq=dateSoutenance:[1965-01-01T23:59:59Z%2BTO%2B2018-12-31T23:59:59Z]&checkedfacets=&start=",i, "&sort=none&status=status:soutenue&access=&prevision=&filtrepersonne=&zone1=titreRAs&val1=&op1=AND&zone2=auteurs&val2=&op2=AND&zone3=etabSoutenances&val3=&op3=AND&zone4=dateSoutenance&val4a=&val4b=&type=lng=&checkedfacets=&format=csv")


map(URL, getURL) %>% write.csv(.,"SERP_2.csv")


thesis <- read.csv("SERP_2.csv",
                   sep = ";", quote = "", skip = 1,
                   stringsAsFactors = F)

colnames(thesis) <- c("author", "author_id", "title", 
                 "thesis_advisor1", "thesis_advisor2", "thesis_advisors_id", 
                 "university", "university_id", "discipline", 
                 "status", "data_first_registration", "date_defense", 
                 "language", "thesis_id", 
                 "online", "date_upload", "date_update", "whatever")


# Remove crappy column in the end
thesis <- select(thesis, -whatever)

# The end rows are crap, let's remove them
nrow(thesis)
thesis[369718:369764,]
thesis <- thesis[-c(369718:369764),]

tail(thesis)


# Save
write.table(thesis, "thesis.csv",
            quote = FALSE,
            sep = ";",
            dec = ".",
            row.names = FALSE)













# on garde les thèses en "sociologie" et sciences sociales


socio <- theses %>% filter(grepl("sociologie|Sciences sociales|sc sociales",X..Discipline..,ignore.case=T)) %>%
  filter(X..Date.de.soutenance..!="") %>%
  filter(X..Statut.. == "soutenue")

# dans "socio" on a maintenant 618 identifiants de thèse
# ce sont ces identifiants qui font servir de base
# au scrapping des jurys

library(rvest)
identifiants <- socio$X..Identifiant.de.la.these..
reseau_total <- data_frame(noms_jury="",
                           liens_jury="",
                           these="",
                           directeurs="",
                           liens_directeurs="")
for (i in 1:length(identifiants)) {

  data_theses_socio <- read_html( paste("http://www.theses.fr/",identifiants[i],sep="") )
  #directeurs :
  directeurs <- bind_cols(
    directeurs= data_theses_socio  %>%
      html_nodes("div .donnees-ombre p") %>%
      .[[1]] %>%
      html_nodes("a") %>%
      html_text()
    ,
    liens_directeurs = data_theses_socio  %>%
      html_nodes("div .donnees-ombre p") %>%
      .[[1]] %>%
      html_nodes("a") %>%
      html_attr(name="href")
  ) %>% mutate(  these = identifiants[i] )
  
  
  
  jury <- bind_cols( 
    noms_jury = data_theses_socio %>%
      html_nodes("div .donnees p a") %>%
      html_text()
    ,
    liens_jury = data_theses_socio %>%
      html_nodes("div .donnees p a") %>%
      html_attr(name="href")
  ) %>% mutate(  these = identifiants[i] )
  
  reseau = jury %>% left_join(directeurs,by="these") 
  
  reseau_total <- bind_rows(reseau_total,reseau)
}

write.csv2(reseau_total,"~/Desktop/reseau-theses-2015-2018.csv")

reseau_total <- read.csv2("~/Dropbox/reseau-theses-2015-2018.csv",stringsAsFactors = F)

library(igraph)
library(ggraph)
library(ggrepel)

# composer le réseau des relations de jury à partir de reseau_total
# règles de composition - ponderation
#     - co-direction        = 3
#     - lien directeur-jury = 2
#     - lien jury-jury      = 1 --- il faut pondérer par le nombre de membres du jury
# règles de composition : direction des liens
#     - direction     --> jury
#     - co-direction <--> co-direction
#     - jury         <--> jury

# on enleve certains doublons
reseau_total <- reseau_total %>% select(noms_jury:liens_directeurs) %>% unique() %>%
  filter(noms_jury!="")
#problèmes d'encodage'
reseau_total <- reseau_total %>% 
  mutate(noms_jury = gsub("\\?","é",noms_jury)) %>%
  mutate(directeurs = gsub("\\?","é",directeurs))

# enlever les directeurs qui se trouvent aussi dans les jurys
reseau_total <- reseau_total %>% 
  filter(noms_jury!=directeurs)

nombre_directions <- reseau_total %>% group_by(these,directeurs) %>%
  summarize(N=n()) %>%
  group_by(directeurs) %>% summarize(N=n())

nombre_participations <- reseau_total %>% group_by(these,noms_jury) %>%
  summarize(N=n()) %>%
  group_by(noms_jury) %>% summarize(N=n())


directions_theses <- reseau_total %>% select(these,directeurs)
directions_theses <- directions_theses %>% unique()
directions_theses <-  directions_theses %>% group_by(these) %>% mutate(N=n()) %>%
  filter(N==2) %>% # on ne garde que les codirections avec 2 directeurs
  mutate(rang=rank(directeurs))
directions_theses <- directions_theses %>% 
  spread(key=rang,value=directeurs)
directions_theses <- directions_theses %>% ungroup() %>% select(nom1=`1`,nom2=`2`)
directions_theses <- directions_theses %>% mutate(poids=3)

directions_jury <- reseau_total %>% 
  group_by(these,directeurs) %>% mutate(taille_jury=n()) %>%
  ungroup() %>%
  select(nom1=noms_jury,nom2=directeurs,poids=taille_jury) %>% filter( nom1 != "")
directions_jury <- directions_jury %>% mutate(poids=2)
directions_jury <- directions_jury %>% group_by(nom1,nom2) %>% summarize(poids=sum(poids)) %>%
  filter(nom1!=nom2)


jury_jury <- reseau_total %>% select(noms_jury,these) %>% unique() %>% filter(noms_jury!="") %>%
  group_by(these) %>% mutate(weight=1)
g_j <-  graph_from_data_frame(jury_jury,directed=F)
V(g_j)$type <- V(g_j)$name %in% jury_jury$noms_jury
g_j_1 <- bipartite_projection(g_j,which="true")
jurys <- as_long_data_frame(g_j_1) %>%
  select(nom1=`ver[el[, 1], ]`, nom2=`ver2[el[, 2], ]`, poids=weight)

#jury_jury <- jury_jury %>% group_by(these) %>% mutate(rang=rank(noms_jury)) %>%
#  spread(key=rang,value=noms_jury)





reseau_petit <- bind_rows(directions_theses,directions_jury,jurys)
reseau_petit <- reseau_petit %>% group_by(nom1,nom2) %>% summarize(poids=sum(poids))
  
  
  
#nombre_jury <- reseau_total %>% group_by(noms_jury) %>% summarize(N=n()) %>% filter(N>2)
#nombre_direction <- reseau_total %>% group_by(directeurs) %>% summarize(N=n()) %>% filter(N>6)

#reseau_petit <- reseau_total %>% filter(noms_jury %in% nombre_jury$noms_jury) %>%
#  filter(directeurs %in% nombre_direction$directeurs)

#reseau_petit <- reseau_petit %>% mutate(directeurs = case_when(directeurs==" Michel Wieviorka"~ "Michel Wieviorka",
#                                                               TRUE ~ directeurs))
#g <-  graph_from_data_frame( reseau_petit %>% select(noms_jury,directeurs) %>% filter( noms_jury != ""), directed = F)
g <- graph_from_data_frame(reseau_petit, directed=F)
g <- simplify(g,edge.attr.comb = sum)
V(g)$nombre_theses <- nombre_directions$N[match(V(g)$name,nombre_directions$directeurs)]
V(g)$nombre_jury   <-  nombre_participations$N[match(V(g)$name,nombre_participations$noms_jury)]

V(g)$degres <-  degree(g)

V(g)$label <- gsub("^\\S+\\s+(.+)$","\\1",V(g)$name)
V(g)$communaute <- as.character(cluster_walktrap(g, steps=15)$membership)
#V(g)$communaute <- as.character(cluster_edge_betweenness(g)$membership)
#g <- delete_edges(g, which(E(g)$poids<3) )
V(g)$closeness <- (5*closeness(g))^10
V(g)$btwns <- betweenness(g)
V(g)$eigen_centr <- eigen_centrality(g)$vector

# Comment simplifier le réseau
# garder individus avec degré important
# garder tous les individus qui ont dirigé 3 thèses ou plus
# garder tous les individus qui ont participé à 3 jury ou plus
# garder les liens "poids>4"
# Pour les individus qui sortent du réseau, garder les liens avec les 2 individus avec le plus gros degré

#g <- induced_subgraph(g, V(g)$degres>8)

g <- delete_edges(g, which(E(g)$poids<3) )
V(g)$cluster_number <- clusters(g)$membership
g <- induced_subgraph(g, V(g)$cluster_number== which( max(clusters(g)$csize) == clusters(g)$csize) ) # on garde le gros cluster
E(g)$weight <- 1/E(g)$poids
#plot(g)
V(g)$label <- ifelse(V(g)$degres<20,"",V(g)$label)
#V(g)$communaute <- as.character(cluster_walktrap(g, steps=15)$membership)
V(g)$communaute <- as.character(cluster_walktrap(g, steps=4)$membership)
ggraph(g,layout="igraph",algorithm="nicely") + 
  geom_edge_link(aes(width=.1*poids), alpha=.1, 
                 end_cap = circle(0, 'mm'), 
                 start_cap = circle(0, 'mm')) +
  #  geom_node_point(aes(size=eigen_centr),color="lightblue4") + 
  #  geom_node_point(color="lightblue",size=9) +
  geom_node_point(aes(size=btwns), color="white",alpha=1) +
  geom_node_point(aes(color=communaute,size=btwns), alpha=.5) +
  scale_size_area(max_size = 20) +
  geom_node_text(aes(label=label),size=3,repel=T,box.padding = 0.15) +
  #  scale_size_continuous(range = c(1, 6)) +
  labs(title="Réseaux de sociologues",
       subtitle="Soutenances de thèses entre 2015 et 2018",
       caption="Sources : theses.fr - Réalisation B. Coulmont") +
  theme_graph(foreground = 'white', fg_text_colour = 'white',
              base_family = "Helvetica") +
  theme(legend.position="none",
        text=element_text(size=16,family="Helvetica"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units="line"))

ggsave(filename = "test-reseau.pdf",width = 20,height = 20)
ggsave(filename = "test-reseau.png",width = 10,height = 10)


ggraph(g,layout="igraph",algorithm="fr") + 
  geom_edge_link(aes(width=.1*poids), alpha=.1, 
                 end_cap = circle(5, 'mm'), 
                 start_cap = circle(5, 'mm')) +
#  geom_node_point(aes(size=eigen_centr),color="lightblue4") + 
#  geom_node_point(color="lightblue",size=9) +
  geom_node_point(aes(size=eigen_centr), color="white",alpha=1) +
  geom_node_point(aes(color=communaute,size=eigen_centr), alpha=.5) +
  scale_size_area(max_size = 20) +
  geom_node_text(aes(label=label),size=3,repel=T,box.padding = 0.15) +
#  scale_size_continuous(range = c(1, 6)) +
  labs(title="Réseaux de sociologues",
       subtitle="Soutenances de thèses entre 2015 et 2018",
       caption="Sources : theses.fr - Réalisation B. Coulmont") +
  theme_graph(foreground = 'white', fg_text_colour = 'white',
              base_family = "Helvetica") +
  theme(legend.position="none",
        text=element_text(size=16,family="Helvetica"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units="line"))

ggsave(filename = "test-reseau.pdf",width=20,height = 20)



############
#
#
#' BROUILLON
#' 
#' 


#### Jury
jury <- bind_cols( 
noms=read_html("http://www.theses.fr/2015TOU20114") %>%
  html_nodes("div .donnees p a") %>%

  html_text()
,
liens=read_html("http://www.theses.fr/2015TOU20114") %>%
  html_nodes("div .donnees p a") %>%
  html_attr(name="href")
)

http://www.theses.fr/2017BORD0902

jury <- bind_cols( 
  noms=read_html("http://www.theses.fr/2015BORD0025") %>%
    html_nodes("div .donnees p a") %>%
    html_text()
  ,
  liens=read_html("http://www.theses.fr/2017BORD0902") %>%
    html_nodes("div .donnees p a") %>%
    html_attr(name="href")
)


read_html("http://www.theses.fr/2015BORD0025") %>%
  html_nodes("div .donnees p ") %>%
  html_text()
read_html("http://www.theses.fr/2015BORD0025") %>%
  html_nodes("div .donnees p a") %>%
  html_attr(name="href")


read_html("http://www.theses.fr/2015BORD0025") %>%
  html_nodes("div .donnees p span ") 
