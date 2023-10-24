diversidad <- function(X, indice = "shannon"){
  
  # Octubre de 2023
  # DESCRIPCIÓN: Calcula los indices de diversidad indicados por 
  # el usuario; compara los resultados de tres diferentes indices: shannon, ichao1 y simpson.
  
  # ARGUMENTOS:
  # X = Númerico. Base de datos.
  # indice = Cadena de texto.
  
  # CÓDIGO:
  # Recibe como entrada una matriz de datos con las especies y abundancias,la 
  # base de datos debe de estar con las especies ubicadas en las filas 
  # y los sitios evaluados en las columnas.
  #Ejemplo:
  # dato <- matrix(data = c(1, 4, 5, 0, 0, 3, 0, 1, 2, 9, 2, 0), ncol = 3,
  # nrow = 4, byrow = TRUE)
  # colnames(dato) <- c("sitio1", "sitio2", "sitio3")
  
  # El codigo trabaja con tres indices:
  # shannon, simpson y ichao1.
  
  # Indicar siempre en caso de emplear simpson y ichao1; shannon ya se
  # encuentra como un valor por defecto.
  # diversidad(X, indice = "simpson")
  # diversidad(X, indice = "ichao1")
  # diversidad(X)
  
  ind_diver <- function(x, indice = "shannon"){
    
    x <- x[x > 0] #x debe ser mayor a cero para ejecutar la primera ecuación
    p <- x / sum(x)
    if(indice == "shannon"){
      r <- -sum(p * log(p))
    }else if(indice == "simpson"){
      r <- 1 - sum(p^2) 
    }
    if(indice == "ichao1"){
      riqueza <- length(unique(x))
      n <- sum(x)
      r <- n + (n * riqueza) / (riqueza + 1)
    }
    
    # imprimir 
    r
  }
  a <- ncol(X)
  r <- rep(NA, a)
  
  for (i in 1:a) {r[i] <- ind_diver(x = X[ ,i], indice = indice)
  }
  r
  
  # VALOR:
  # La función debe devolver un solo valor dependiendo de lo que el usuario 
  # indique.  
}