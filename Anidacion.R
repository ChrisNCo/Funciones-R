analisis_anidacion_tortugas <- function(datos_anidacion, decremento = 0.2) {
  
  # Oct 2023
  # DESCRIPCION: Analizar datos de la poblaciones de tortugas marinas, dando un 
  # total de nidos por especie, un promedio por especie y hasta identificar y mostrar el 
  # resultado de cual especie esta en peligro de extincion. 
  
  # Argumentos 
  # datos_anidacion = datos que el usuario tiene que entrar de los nidos por 
  # especie y por los años registrados.
  # decremento = numerico, poner el porcentaje deseado de forma decimal. 
  # Como ejemplo el 20% de decremento en la cantidad de nidos a traves de los años. 
  
  # CODIGO: 
  # Tiene que recibir una tabla con la informacion de nidos y en que años de la 
  # cantidad de especies necesarias. 
  # Calcular el total de nidos por especie
  total_nidos_por_especie <- tapply(datos_anidacion$Nidos, datos_anidacion$Especie, sum)
  
  # Calcular el promedio de nidos por año
  promedio_nidos_por_año <- tapply(datos_anidacion$Nidos, datos_anidacion$Año, function(x) mean(x))
  
  # Ciclo para identificar especies en peligro basadas en decremento
  especies_en_peligro <- character(0)
  for (especie in unique(datos_anidacion$Especie)) {
    datos <- datos_anidacion[datos_anidacion$Especie == especie, ]
    nidos_por_año <- tapply(datos$Nidos, datos$Año, sum)
    max_nidos <- max(nidos_por_año)
    min_nidos <- min(nidos_por_año)
    
    # condicional para sacar la especie en peligro
    if ((max_nidos - min_nidos) / max_nidos >= decremento) {
      especies_en_peligro <- c(especies_en_peligro, especie)
    }
  }
  
  # VALOR
  # Generar los siguientes resultados 
  # Crear lista con los resultados
  resultado <- list(
    total_nidos_por_especie = total_nidos_por_especie,
    promedio_nidos_por_año = promedio_nidos_por_año,
    especies_en_peligro = especies_en_peligro
  )
  
  return(resultado)
}

