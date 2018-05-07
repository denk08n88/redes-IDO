#Aquí describimos los vértices, aristas dirigidas y pesos.
nodos <- c("A","B","C","D","E","F","G","H","I","J")
desde <- c("C","A","A","B","D","E","C","F","F","H","F") 
hasta <- c("A","B","E","D","E","C","F","G","H","I","J")


#Aquí juntamos los datos anteriores en un data frame
red <- data.frame(Desde = desde, Hasta = hasta)
red

#Devuelve una columna con la EX-VECINDAD de un vértice dado-
#ex-vecindad del nodo n <- conjunto de los nodos en los que incide un arco con cola en n
vecindad <- function(desde){
  exv <- as.matrix((subset(red, Desde == desde))["Hasta"])
  return (exv)
}
#Prueba
vecindad("A")
vecindad("C")

#Devuelve un valor de verdad al preguntarle si un vértice está en la EX-VECINDAD de otro
sonVecinos <- function(desde, hasta){
  exv <- sum(vecindad(desde) == hasta) > 0 
  return (exv)
}
#Prueba
sonVecinos("C", "A")
sonVecinos("C", "B")

#Esta función devuelve una ruta entre from y to si es que existe.
ruta <- function(desde, hasta) {
  if(hasta==desde){return(hasta)}
  else if(sonVecinos(desde,hasta)){return(c(desde,hasta))}
  else{
    
    for (i in vecindad(desde)) {
      if (sonVecinos(i, hasta)) {
        return(c(desde, i, hasta))
      }
      
      for (j in vecindad(i)) {
        if (sonVecinos(j, hasta)) {
          return(c(desde, i, j, hasta))
        }
        
        for (k in vecindad(j)) {
          if (sonVecinos(k, hasta)) {
            return(c(desde, i, j, k, hasta))
          }
          
          for (l in vecindad(k)) {
            if (sonVecinos(l, hasta)) {
              return(c(desde, i, j, k, l, hasta))
            }
            
            for (m in vecindad(l)) {
              if (sonVecinos(m, hasta)) {
                return(c(desde, i, j, k, l, m, hasta))
             }
          }
        }
      }
    }
  }
}
}

#######PRUEBA DE LA FUNCIÓN###########

for(i in nodos){
  for(j in nodos){
    cat("Una ruta entre",i,"y",j,"es:\n")
    print(ruta(i,j))
  }
}


