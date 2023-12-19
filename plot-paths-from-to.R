library(readxl)

extract_pedigree <- function(indiv_id, data, edges = data.frame(from = character(0), to = character(0))){
  # Check if the indiv_id exists in the dataset
  if(!indiv_id %in% data$Indiv) {
    return(edges)
  }
  
  horse <- data[data$Indiv == indiv_id, ]
  
  if(!is.na(horse$Sire)){
    edges <- rbind(edges, data.frame(from = as.character(horse$Sire), to = as.character(indiv_id)))
    edges <- extract_pedigree(horse$Sire, data, edges)
  }
  
  if(!is.na(horse$Dam)){
    edges <- rbind(edges, data.frame(from = as.character(horse$Dam), to = as.character(indiv_id)))
    edges <- extract_pedigree(horse$Dam, data, edges)
  }
  
  return(edges)
}

# Then, proceed as before:

individual <- "Bresilia2005" # replace with your specific individual ID
edges <- extract_pedigree(individual, data)

# Create the graph
g <- graph_from_data_frame(edges, directed = TRUE)

# Find all simple paths
paths <- all_simple_paths(g, from = "Fairfax_Morocco_Barb_1633", to = "Bresilia2005")

# Convert paths to list of vertex sequences
paths_list <- lapply(paths, function(p) as.integer(p))

# Extract the actual names (horses) from each path
paths_names <- lapply(paths_list, function(p) V(g)$name[p])

# Print paths
for(i in 1:length(paths_names)) {
  cat(paste0("Path ", i, ": ", paste(paths_names[[i]], collapse = " -> "), "\n"))
}

# If you want to visualize a specific path
# Choose a path (e.g., the first one)
selected_path <- paths[[1]]

# Subset the graph to the chosen path
g_sub <- induced_subgraph(g, vids = V(g)[selected_path])

# Visualize
plot(g_sub, edge.arrow.size = 0.5, vertex.label.cex = 0.8, 
     layout = layout_as_tree)

# Extract all descendants of ancestor up to descendant
all_descendants_until_aghil_aga <- function(individual, data) {
  if (individual == "Bresilia2005") return(character(0))
  
  children <- c(data[data$Sire == individual, ]$Indiv, data[data$Dam == individual, ]$Indiv)
  if (length(children) == 0) {
    return(character(0))
  } else {
    grand_children <- unlist(lapply(children, function(child) all_descendants_until_aghil_aga(child, data)))
    return(c(children, grand_children))
  }
}

descendants_of_godolphin <- all_descendants_until_aghil_aga("Fairfax_Morocco_Barb_1633", data)

# Filter edges for Godolphin's descendants up to Aghil Aga
edges_filtered <- edges[edges$from %in% c("Fairfax_Morocco_Barb_1633", descendants_of_godolphin), ]

edges_filtered <- unique(edges_filtered)

# Create the graph
g_filtered <- graph_from_data_frame(edges_filtered, directed = TRUE)

# Modify layout for wider spacing
layout_tree_wide <- function(graph) {
  l <- layout_as_tree(graph)
  l[,1] <- l[,1] * 5   # Adjusting horizontal spacing
  l[,2] <- l[,2] * 2   # Adjusting vertical spacing
  return(l)
}

# Set vertex shapes based on gender
V(g_filtered)$color <- ifelse(V(g_filtered)$name %in% data[data$Sex == "male", ]$Indiv, 
                              "lightblue", "pink")

# Plot the graph

library(ggplot2)
library(ggraph)
                              
                              
# Graph visualization using ggraph
ggraph(g_filtered, layout = 'tree', circular = FALSE) +
  geom_edge_link(arrow = arrow(type = "closed", length = unit(5, "pt")), edge_colour = "grey") +
  geom_node_point(aes(color = color), size = 5) + 
  scale_color_identity() +
  geom_node_text(aes(label = name), hjust = 1, nudge_x = -0.05, check_overlap = TRUE) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank()
  )



install.packages("visNetwork")
library(visNetwork)

# Get nodes and edges data from the graph
nodes <- data.frame(id = V(g_filtered)$name, label = V(g_filtered)$name, 
                    color = ifelse(V(g_filtered)$name %in% data[data$Sex == "male", ]$Indiv, 
                                   "lightblue", "pink"))
edges <- get.data.frame(g_filtered, what="edges")
                                   
# Create an interactive graph
visNetwork(nodes, edges, width = "100%") %>%
visEdges(arrows = "to", color = list(color = "grey")) %>%
visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE),
nodesIdSelection = TRUE) %>%
visLayout(randomSeed = 42)  # ensures consistency in layout
                                   

