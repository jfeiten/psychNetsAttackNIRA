cas.att.aggrav <- function(net_obj, dt){

  g <- createGraphFromAdjacencyMatrix(net_obj)

  #numero de repetições
  n <- length(V(g))

  model_net <- net_obj
  g2 <- g
  dt2 <- dt

  #registo dos resultados
  numberofvertices<-integer(n-1)
  clustersizes<-integer(n-1)
  cohesion<-integer(n-1)
  averagepath<-integer(n-1)
  adhesion<-integer(n-1)
  edgedensity <- integer(n-1)
  transitivity <- integer(n-1)
  radius<- integer(n-1)
  density<- integer(n-1)
  centralization<-integer(n-1)
  strength <- integer(n-1)
  vertex <- integer(n-1)
  components <- integer(n-1)



  #remoção dos vértices
  for(i in 1:(n-1)){

    print(i)

    aggravating_simulated_responses <- simulateAggravResponses(model_net)
    aggravating_sum_scores <- customCalculateAggravSumScores(aggravating_simulated_responses)

    node_max <- names(sort(abs(aggravating_sum_scores), decreasing = TRUE))[1]
    strength_max <- sort(abs(aggravating_sum_scores), decreasing = TRUE)[1]
    w <- which(colnames(dt2)==node_max)

    # Testing
    # node_max <- "PhyAct"
    # strength_max <- 10
    # w <- sample(1:ncol(dt2), 1)



    vertex[i] <- node_max
    strength[i] <- strength_max


    # propriedades das redes ----
    #(verificar: algumas parecem ter sempre 0 | edgedensity parece ser a mesma coisa que graph.density)

    numberofvertices[i]<-igraph::gorder(g2)
    clustersizes[i]<-max(igraph::clusters(g2)$csize)
    cohesion[i]<-igraph::cohesion(g2)
    averagepath[i]<-igraph::average.path.length(g2)
    adhesion[i]<- igraph::graph.adhesion(g2)
    edgedensity[i] <-igraph::edge_density(g2)
    transitivity[i]<- igraph::transitivity(g2, type= "global")
    radius[i]<-igraph::radius(g2)
    density[i]<- igraph::graph.density(g2)
    centralization[i]<-igraph::centr_degree(g2)$centralization
    components[i] <- igraph::components(g2)$no


    # remover nó ----
    dt2 <- dt2[, -w] #remover coluna correspondente na tabela de dados

    #if(i == 17){browser()}

    if(length(colnames(dt2)) > 2){
      print("n columns")
      print(length(colnames(dt2)))
      print(colnames(dt2))

      tryCatch(
        expr = {

          set.seed(1234)
          model_net <- estimateNetwork(dt2, default = c("IsingFit"))
          g2 <- createGraphFromAdjacencyMatrix(model_net)

          message("Network successfully estimated.")

        },
        error = function(e){
          message('Caught an error!')
          print(e)
        },
        warning = function(w){
          message('Caught an warning!')
          print(w)
        },
        finally = {
          message('All done, quitting.')
        }
      )


    }
    else{
      print("Matrix with 2 or less columns")
    }


  }

  #tabela com os resultados do ataque
  df <-
    hellno::as.data.frame(cbind(
      c(vertex, NA),
      c(strength, NA),
      c(numberofvertices, NA),
      c(clustersizes, NA),
      c(cohesion, NA),
      c(averagepath, NA),
      c(adhesion, NA),
      c(edgedensity, NA),
      c(transitivity, NA),
      c(radius, NA),
      c(density, NA),
      c(centralization, NA),
      c(components, NA)
    ),
    stringAsFactors = F)
  names(df) <-
    c(
      "cas.att.aggrav.vertex",
      "cas.att.aggrav.value",
      "cas.att.aggrav.number.of.vertices",
      "cas.att.aggrav.maxcsize",
      "cas.att.aggrav.cohesion",
      "cas.att.aggrav.averagepath",
      "cas.att.aggrav.adhesion",
      "cas.att.aggrav.edgedensity",
      "cas.att.aggrav.transitivity",
      "cas.att.aggrav.radius",
      "cas.att.aggrav.density",
      "cas.att.aggrav.centralization",
      "cas.att.aggrav.components"
    )
  return(df)
}
