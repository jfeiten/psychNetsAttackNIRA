random.att.nira <- function(net_obj, dt){

  g <- createGraphFromAdjacencyMatrix(net_obj)

  #numero de repetições
  n <- length(V(g))

  #criar tabela
  mat <- matrix(ncol=2,nrow=n, 0)
  mat <- as.data.frame(mat)


  #adicionar o nome dos vértices
  node_names <- igraph::vertex.attributes(g)$`TRUE`

  mat[, 1] <- node_names


  #calcular medida de centralidade
  delete.randoms <- runif(n=n, min=1, max=n)

  #adcionar valores de centralidade à tabela
  mat[,2] <- delete.randoms


  #ordenar vértices pelo valor da centralidade
  matri <- mat[order(mat[,2], decreasing = TRUE),]

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
  components <- integer(n-1)


  for(i in 1:(n-1)){

    print(i)

    # propriedades da rede ----
    #(verificar: algumas parecem ter sempre 0 | edgedensity parece ser a mesma coisa que graph.density)
    numberofvertices[i]<-igraph::gorder(graph = g2)
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

    #g2 <- igraph::delete.vertices(g2, v=which(V(g2)$names==matri[i,1])) #remover vértice pela ordem na tabela


    #w <- which(colnames(dt2)==matri[i,1])
    w <- sample(1:ncol(dt2), 1)

    # remover nó ----
    dt2 <- dt2[, -w] #remover linha correspondente na tabela de dados

    # estimar rede ----
    model_net <- NULL
    g2 <- NULL
    warning_value <- NULL
    error_value <- NULL

    tryCatch(
      expr = {
        set.seed(1234)
        model_net <- estimateNetwork(dt2, default = c("IsingFit"))
        g2 <- createGraphFromAdjacencyMatrix(model_net)


        message("Network successfully estimated.")

      },

      error = function(e) {
        message('Caught an error!')
        print(e)
        error_value <<- e$message
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
  df<-hellno::as.data.frame(cbind(matri, c(numberofvertices, NA), c(clustersizes, NA), c(cohesion, NA), c(averagepath, NA), c(adhesion, NA), c(edgedensity, NA), c(transitivity, NA), c(radius, NA), c(density, NA), c(centralization, NA), c(components, NA)))
  names(df)<-c( "random.att.vertex", "random.att.value","randon.att.number.of.vertices", "random.att.maxcsize", "random.att.cohesion", "random.att.averagepath", "random.att.adhesion", "random.att.edgedensity", "random.att.transitivity", "random.att.radius", "random.att.density", "random.att.centralization", "random.att.components")
  return(df)
}


