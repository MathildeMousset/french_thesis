
#################### SCRAP ####################
scrap_phd_webpages <- function(my_data){
  
  library(tidyverse)
  library(rvest) # To scrap PhD pages to get committe members
  
  
  thesis_id <- my_data$thesis_id # get PhD ids
  
  total_network <- data.frame(jury_members = "",
                              jury_links = "",
                              these      = "",
                              directeurs = "",
                              advisor_id = "")
  
  
  for (i in 1:length(thesis_id)) {
    
    # get info on current PhD
    data_phd_evolution <- read_html(paste0("http://www.theses.fr/", 
                                           thesis_id[i]) ) 
    
    # get name PhD supervisor for 
    directeurs <- bind_cols(
      directeurs = data_phd_evolution  %>%
        html_nodes("div .donnees-ombre p") %>%
        .[[1]] %>%
        html_nodes("a") %>%
        html_text()
      ,
      advisor_id = data_phd_evolution  %>%
        html_nodes("div .donnees-ombre p") %>%
        .[[1]] %>%
        html_nodes("a") %>%
        html_attr(name="href")
    ) %>% mutate(these = thesis_id[i])
    
    
    # get names of people in commitees
    jury <- bind_cols( 
      jury_members = data_phd_evolution %>%
        html_nodes("div .donnees p a") %>%
        html_text()
      ,
      jury_links = data_phd_evolution %>%
        html_nodes("div .donnees p a") %>%
        html_attr(name="href")
    ) %>% mutate(  these = thesis_id[i] )
    
    # put all together
    network <- jury %>% left_join(directeurs, by = "these") 
    total_network <- bind_rows(total_network, network)
  }
  
  return(total_network)
  
}



#################### SET WEIGHTS ####################

set_weights_aa <- function(the_data) {
  
  library(tidyverse)
  
  # Link supervisor - supervisor
  advisor_advisor <- the_data %>% 
    mutate(directeurs = str_trim(directeurs)) %>% 
    select(these, directeurs) %>% 
    unique() %>% 
    group_by(these) %>% 
    mutate(N = n()) %>%
    
    filter(N == 2) %>% # keep co-supervision w/ 2 supervisors 
    mutate(rang  = rank(directeurs)) %>% 
    spread(key   = rang, 
           value = directeurs) %>% 
    
    ungroup() %>% 
    select(name_1 = `1`, name_2 = `2`) %>% 
    mutate(poids = 3)
  
  advisor_advisor <- advisor_advisor
  
  return(advisor_advisor)
}
set_weights_aj <- function(the_data) {
  
  library(tidyverse)
  library(stringr)
  
  # Link advisor - jury
  advisor_jury <- the_data %>% 
    
    select(name_1 = jury_members,
           name_2 = directeurs) %>% 
    
    mutate(name_1 = str_trim(name_1),
           name_2 = str_trim(name_2)) %>% 
    
    filter( name_1 != "") %>%
    mutate(poids = 2) %>%
    group_by(name_1, name_2) %>% 
    # Sum weight over links
    summarize(poids = sum(poids))
  
  return(advisor_jury)
}
set_weights_jj <- function(the_data) {
  
  library(tidyverse)
  
  # Jury - jury links
  jury_jury <- the_data %>% 
    select(jury_members, these) %>% 
    mutate(jury_members = str_trim(jury_members)) %>% 
    unique() %>% 
    filter(jury_members != "")
  
  return(jury_jury)
}


#################### MAKE GRAPHS ####################

make_jury_network <- function(data_jury) {
  
  library(igraph)
  library(ggraph)
  
  # Make non-directed graph for jur_jury
  g_j <- graph_from_data_frame(data_jury,
                               directed = F)
  
  
  # Create the vertex sequence
  igraph::V(g_j)$type <- V(g_j)$name %in% data_jury$jury_members
  
  g_j_1 <- bipartite_projection(g_j, which = "true")
  
  
  jurys <- as_long_data_frame(g_j_1) %>%
    select(name_1 = `ver[el[, 1], ]`, 
           name_2 = `ver2[el[, 2], ]`, 
           poids  = weight)
  
  return(jurys)
}




make_full_network <- function(data_advisor_advisor,
                              data_advisor_jury,
                              net_jurys) {
  library(tidyverse)
  
  reseau_petit <- bind_rows(data_advisor_advisor,
                            data_advisor_jury,
                            net_jurys) %>%
    group_by(name_1, name_2) %>% 
    summarize(poids = sum(poids)) 
  
  g <- graph_from_data_frame(reseau_petit, 
                             directed = F) 
  
  return(g)
}


clean_network <- function(my_graph,
                          my_waltrap = 10,
                          my_edge = 5,
                          my_degree = 20) {
  
  library(igraph)
  
  my_graph <- simplify(my_graph
                       # edge.attr.comb = list(weight = "sum", 
                       #                       function(x)length(x))
  ) 
  
  V(my_graph)$degres <- degree(my_graph)
  
  # Get surname only
  V(my_graph)$label  <- gsub("^\\S+\\s+(.+)$","\\1",V(my_graph)$name)
  
  
  # determine communities
  # step = the length of the random walk to perform
  V(my_graph)$communaute <- as.character(cluster_walktrap(my_graph,
                                                          steps = my_waltrap)$membership)
  
  V(my_graph)$closeness <- (5*closeness(my_graph))^10
  
  # network metric betweeness
  V(my_graph)$btwns       <- betweenness(my_graph) 
  V(my_graph)$eigen_centr <- eigen_centrality(my_graph)$vector
  
  
  # delete edges with weight < something
  my_graph <- delete_edges(my_graph, which(E(my_graph)$poids < my_edge)) # 5 initiallement
  
  
  
  # to which community you belong
  V(my_graph)$cluster_number <- clusters(my_graph)$membership 
  
  my_graph <- induced_subgraph(my_graph,
                               V(my_graph)$cluster_number == which( max(clusters(my_graph)$csize) == clusters(my_graph)$csize) )
  
  # width of edge proportional to weight
  E(my_graph)$weight <- 1/E(my_graph)$poids 
  
  
  # do not display all names
  V(my_graph)$label <- ifelse(V(my_graph)$degres < my_degree,
                              "",
                              V(my_graph)$label)
  
  return(my_graph)
  
}



plot_my_network <- function(my_graph,
                            my_title,
                            my_subtitle){
  
  library(igraph)
  library(ggraph)
  library(ggrepel)
  
  
  graphe_1 <- ggraph(my_graph,
                     layout = "igraph",
                     algorithm = "fr") + 
    
    geom_edge_link(aes(width = 0.1*poids), 
                   alpha = 0.1, 
                   end_cap   = circle(5, 'mm'), 
                   start_cap = circle(5, 'mm')) +
    
    geom_node_point(aes(size = eigen_centr),
                    color = "white", 
                    alpha = 1) +
    
    geom_node_point(aes(color = communaute,
                        size = eigen_centr), 
                    alpha = 0.5) +
    
    scale_size_area(max_size = 20) +
    
    geom_node_text(aes(label = label),
                   size  = 3, 
                   repel = T,
                   box.padding = 0.15) +
    
    labs(title    = my_title,
         subtitle = my_subtitle,
         caption  = "Sources : theses.fr") +
    
    theme_graph(foreground     = 'white', 
                fg_text_colour = 'white') +
    
    theme(legend.position = "none",
          text            = element_text(size = 16),
          plot.margin     = unit(c(0.2, 0.2, 0.2, 0.2), 
                                 units = "line"))
  
  print(graphe_1)
  
  
}


#################### ALL ####################

# Not scrapping though, because it needs the internet.

make_network_full <- function(my_scrapped_df,
                              my_waltrap = 10,
                              my_edge = 2,
                              my_degree = 20,
                              my_title,
                              my_subtitle) {
  
  # Assign weights
  data_aa <- set_weights_aa(my_scrapped_df)
  data_aj <- set_weights_aj(my_scrapped_df)
  data_jj <- set_weights_jj(my_scrapped_df)
  
  # Make jury's network
  network_jury <- make_jury_network(data_jj)
  
  # Make full network
  network_full <- make_full_network(data_aa,
                                    data_aj,
                                    network_jury)
  
  # Modify the network object a bit before plotting
  network_cleaned <- clean_network(network_full,
                                   my_waltrap,
                                   my_edge,
                                   my_degree)
  
  
  my_plot <- plot_my_network(network_cleaned,
                             my_title,
                             my_subtitle)
  
  return(my_plot)
  
}



#################### TESTS ####################

# thesis_evolution %>% 
#   select(author, author_id, 
#          thesis_id,
#          thesis_advisor1, thesis_advisors_id) %>% 
#   filter(author_id %in% c("224304402",
#                           "126791325",
#                           "164705031",
#                           "22346743X",
#                           "221402276",
#                           "17730426X",
#                           "149391528",
#                           "12922572X",
#                           "221416714")) -> essai
# 
# thesis_evolution %>% 
#   select(author, author_id, 
#          thesis_id,
#          thesis_advisor1, thesis_advisors_id) %>% 
#   filter(author_id %in% c("224304402",
#                           "126791325",
#                           "164705031",
#                           "22346743X",
#                           "221402276")) -> essai_small
# 
# scrape <- scrap_phd_webpages(essai_small)  # ok
# 
# weight_aa <- set_weights_aa(scrape) # ok
# weight_aj <- set_weights_aj(scrape) # ok
# weight_jj <- set_weights_jj(scrape) # ok
# 
# 
# jury_network <- make_jury_network(weight_jj) # ok
# 
# full_graph <- make_full_network(weight_aa,
#                                 weight_aj,
#                                 jury_network) # ok
# 
# full <- make_graph(weight_aa,
#                    weight_aj,
#                    weight_jj) # ok
# 
# get_network_data(essai_small) # ok
# 
# 
# full_cleaned <- clean_network(full,
#                               my_waltrap = 10,
#                               my_edge = 2,
#                               my_degree = 5) # ok
# 
# 
# plot_my_network(full_cleaned,
#                 my_title = "Network evolutionary biologists",
#                 my_subtitle = "In France") # ok
# 
# 
# make_network_full(scrape,
#                   my_waltrap = 10,
#                   my_edge = 2,
#                   my_degree = 20,
#                   my_title = "Network evolutionary biologists",
#                   my_subtitle = "In France") # ok
# 
