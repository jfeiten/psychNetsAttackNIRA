cas.att.allev <- function(net_obj, dt){

  g <- createGraphFromAdjacencyMatrix(net_obj)

  #numero de repetições
  n <- length(V(g))

  model_net <- net_obj
  g2 <- g
  dt2 <- dt


  #registro dos resultados
  numberofvertices<-rep(NA, n-1)
  clustersizes<-rep(NA, n-1)
  cohesion<-rep(NA, n-1)
  averagepath<-rep(NA, n-1)
  adhesion<-rep(NA, n-1)
  edgedensity <- rep(NA, n-1)
  transitivity <- rep(NA, n-1)
  radius <- rep(NA, n-1)
  density <- rep(NA, n-1)
  centralization <- rep(NA, n-1)
  strength <- rep(NA, n-1)
  vertex <- rep(NA, n-1)
  components <- rep(NA, n-1)


  #remoção dos vértices
  for(i in 1:(n-1)){

    print(i)


    alleviating_simulated_responses <- simulateAllevResponses(model_net)

    alleviating_sum_scores <- customCalculateAllevSumScores(alleviating_simulated_responses)


    node_max <- names(sort(abs(alleviating_sum_scores), decreasing = TRUE))[1]
    strength_max <- sort(abs(alleviating_sum_scores), decreasing = TRUE)[1]
    w <- which(colnames(dt2)==node_max)

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

    model_net <- NULL
    g2 <- NULL
    warning_value <- NULL
    error_value <- NULL

    tryCatch(
      expr = {
        set.seed(1234)
        model_net <- estimateNetwork(dt2, default = c("IsingFit"))
        g2 <- createGraphFromAdjacencyMatrix(model_net)

        print("ncols")
        print(ncol(dt2))

        message("Network successfully estimated.")

      },

      warning = function(w) {
        message('Caught a warning!')
        print(w)

        # Get the value returned in the warning
        warning_value <<- w$message

        message("Forcing network estimation")

        tryCatch(
          expr = {
            set.seed(1234)
            model_net <<- estimateNetwork(dt2, default = c("IsingFit"))
            g2 <<- createGraphFromAdjacencyMatrix(model_net)
          },
          error = function(e) {
            message('Caught an error!')
            print(e)
            error_value <<- e$message
          }
        )


        print(error_value)


        # Check if there was also an error
        if (!is.null(error_value)) {
          message("Error in network estimation")
        }

      },
      finally = {
        message('All done')

      }
    )

    if (!is.null(error_value)) {
      message("Error in network estimation")
      break
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
