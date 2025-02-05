library(DiagrammeR)

# Create the graph with separate clusters on left and right sides
graph <- create_graph() %>%
    add_n_nodes(n = 9, label = paste("Issue", 1:9), type = "left") %>%
  add_edges_w_string("1->2 1->3 1->4 1->5 1->6 1->7 1->8 1->9 2->3 2->4 2->5 2->6 2->7 2->8 2->9 3->4 3->5 3->6 3->7 3->8 3->9 4->5 4->6 4->7 4->8 4->9 5->6 5->7 5->8 5->9 6->7 6->8 6->9 7->8 7->9 8->9") %>%
set_edge_attrs(edge_attr = "color", values = "grey40") %>%
  set_node_attrs(node_attr = "fontcolor", values = "black")


# Render the graph with title and layout
set.seed("97")
#render_graph(graph, title = "Pole A", layout = "nicely")


export_graph(graph, title = "", file_name = "Figures/poleA.svg", file_type = "svg")

graph2 <- create_graph() %>%
add_n_nodes(n = 9, label = paste("Issue", 1:9), type = "right") %>%
  add_edges_w_string("1->2 1->3 2->3 4->5 4->6 5->6 7->8 7->9 8->9") %>%
  set_edge_attrs(edge_attr = "color", values = "grey40") %>%
  set_node_attrs(node_attr = "fontcolor", values = "black") %>%

  
  # Connect nodes across clusters with individual edges set to transparent color
  add_edge(from = 1, to = 4) %>%
  set_edge_attrs(edge_attr = "color", values = "transparent", from = 1, to = 4) %>%
  
  add_edge(from = 1, to = 7) %>%
  set_edge_attrs(edge_attr = "color", values = "transparent", from = 1, to = 7) %>%
  
  add_edge(from = 2, to = 5) %>%
  set_edge_attrs(edge_attr = "color", values = "transparent", from = 2, to = 5) %>%
  
  add_edge(from = 2, to = 8) %>%
  set_edge_attrs(edge_attr = "color", values = "transparent", from = 2, to = 8) %>%
  
  add_edge(from = 3, to = 7) %>%
  set_edge_attrs(edge_attr = "color", values = "transparent", from = 3, to = 7) %>%
  
  add_edge(from = 3, to = 9) %>%
  set_edge_attrs(edge_attr = "color", values = "transparent", from = 3, to = 9) %>%

  add_edge(from = 5, to = 8) %>%
  set_edge_attrs(edge_attr = "color", values = "transparent", from = 5, to = 8)

export_graph(graph2, title = "", file_name = "Figures/poleB.svg", file_type = "svg")
