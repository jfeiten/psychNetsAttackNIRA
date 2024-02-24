cas.att.allev <- function(net_obj, dt){

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

    alleviating_simulated_responses <- simulateAllevResponses(model_net)
    rm(model_net)

    alleviating_sum_scores <- customCalculateAllevSumScores(alleviating_simulated_responses)


    node_max <- names(sort(abs(alleviating_sum_scores), decreasing = TRUE))[1]
    strength_max <- sort(abs(alleviating_sum_scores), decreasing = TRUE)[1]
    w <- which(colnames(dt2)==node_max)


    # testing
    #node_max <- "PhyAct"
    #strength_max <- 10
    #w <- sample(1:ncol(dt2), 1)

    vertex[i] <- node_max
    strength[i] <- strength_max


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

    dt2 <- dt2[, -w] #remover coluna correspondente na tabela de dados


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

          # Get the value returned in the warning
          warning_value <<- w$message


        },
        finally = {

          if (!is.null(warning_value)) {

            message("Forcing network estimation")

            set.seed(1234)
            model_net <- estimateNetwork(dt2, default = c("IsingFit"))
            g2 <- createGraphFromAdjacencyMatrix(model_net)

          }

          message('All done')

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
      "cas.att.allev.vertex",
      "cas.att.allev.value",
      "cas.att.allev.number.of.vertices",
      "cas.att.allev.maxcsize",
      "cas.att.allev.cohesion",
      "cas.allev.deg.averagepath",
      "cas.att.allev.adhesion",
      "cas.att.allev.edgedensity",
      "cas.att.allev.transitivity",
      "cas.att.allev.radius",
      "cas.att.allev.density",
      "cas.att.allev.centralization",
      "cas.att.allev.components"
    )
  return(df)
}
