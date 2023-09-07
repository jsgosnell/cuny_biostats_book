


#probability trees
##below code is unfortunately cumbersome
#from https://daranzolin.github.io/2018-01-07-probability-trees/
#
library(DiagrammeR)

bayes_probability_tree <- function(prior, true_positive, true_negative, label1 = "Prior", 
                                   label2 = "Complimentary Prior", label3 = "True Positive",
                                   label4 = "False Negative", label5 = "False Positive",
                                   label6 = "True Negative") {
  
  if (!all(c(prior, true_positive, true_negative) > 0) && !all(c(prior, true_positive, true_negative) < 1)) {
    stop("probabilities must be greater than 0 and less than 1.",
         call. = FALSE)
  }
  c_prior <- 1 - prior
  c_tp <- 1 - true_positive
  c_tn <- 1 - true_negative
  
  round4 <- purrr::partial(round, digits = 5)
  
  b1 <- round4(prior * true_positive)
  b2 <- round4(prior * c_tp)
  b3 <- round4(c_prior * c_tn)
  b4 <- round4(c_prior * true_negative)
  
  bp <-  round4(b1/(b1 + b3))
  
  labs <- c("X", prior, c_prior, true_positive, c_tp, true_negative, c_tn, b1, b2, b4, b3)
  
  tree <-
    create_graph() %>%
    add_n_nodes(
      n = 11,
      type = "path",
      label = labs,
      node_aes = node_aes(
        shape = "circle",
        height = 1,
        width = 1,
        x = c(0, 3, 3, 6, 6, 6, 6, 8, 8, 8, 8),
        y = c(0, 2, -2, 3, 1, -3, -1, 3, 1, -3, -1))) %>% 
    add_edge(
      from = 1,
      to = 2,
      edge_aes = edge_aes(
        label = label1
      )
    ) %>% 
    add_edge(
      from = 1, 
      to = 3,
      edge_aes = edge_aes(
        label = label2
      )
    ) %>% 
    add_edge(
      from = 2,
      to = 4,
      edge_aes = edge_aes(
        label = label3
      )
    ) %>% 
    add_edge(
      from = 2,
      to = 5,
      edge_aes = edge_aes(
        label = label4
      )
    ) %>% 
    add_edge(
      from = 3,
      to = 7,
      edge_aes = edge_aes(
        label = label5
      )
    ) %>% 
    add_edge(
      from = 3,
      to = 6,
      edge_aes = edge_aes(
        label = label6
      )
    ) %>% 
    add_edge(
      from = 4,
      to = 8,
      edge_aes = edge_aes(
        label = "="
      )
    ) %>% 
    add_edge(
      from = 5,
      to = 9,
      edge_aes = edge_aes(
        label = "="
      )
    ) %>% 
    add_edge(
      from = 7,
      to = 11,
      edge_aes = edge_aes(
        label = "="
      )
    ) %>% 
    add_edge(
      from = 6,
      to = 10,
      edge_aes = edge_aes(
        label = "="
      )
    ) 
  message(glue::glue("The probability of having {label1} after testing {label3} is {bp}"))
  print(render_graph(tree))
  invisible(tree)
}

#first example
bayes_probability_tree(prior = 0.5, true_positive = 0.6, true_negative = 0.9, label1 = "medicine", label2 = "placebo",
                       label3 = "cured", label4 = "not cured",
                       label5 = "cured", label6 = "not cured")

#second example
bayes_probability_tree(prior = 0.0001, true_positive = 0.9, true_negative = 0.999, label1 = "cancer", 
                       label2 = "not cancer",
                       label3 = "positive", 
                       label4 = "negative",
                       label5 = "positive", 
                       label6 = "negative")

#add nodes
.00009 + .00001 + .001 + .9989

(.00009)/(.001+ .00009)

