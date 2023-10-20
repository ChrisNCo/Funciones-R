atp_ag = function(C, ins = 0){

  #Oct 2023
  #Descripción: 
  #Calcula la cantidad de ATP que genera un acido graso dentro de la fosforilacion 
  #oxidativa, basado en la cantidad de carbonos e instaruaciones(si es que las posee).

  #Argumentos:
  #C = numerico. Numero de carbonos que compone el acido graso.
  #ins = numerico. Numero de insaturaciones que posee el acido graso.

  #Codigo:

  #Error 1: 
  #Revisa si el numero de carbonos esta entre 2 y 24, ya que no
  # existen naturalmente acidos grasos de menor o mayor numero.
  if(C<2 | C>24){
    stop("La cadena no puede ser menor a 2 carbonos o mayor a 24 carbonos")
  }

  #Error 2:
  #Revisa si el acido graso posee menos de 6 insaturaciones, ya que
  # no existen naturalmente acidos grasos con mas de 5 insaturaciones.
  if(ins > 6){
    stop("La cadena no puede poseer mas de 6 insaturaciones")
  }

  #Error 3:
  #Revisa para un acido graso par, si el numero de insaturaciones es 1/2 del numero
  #de carbonos, ya que 2 carbonos continuos no pueden tener ambos una insaturacion.
  if(C %% 2 == 0 & ins > C/2){
    x = C/2
    stop(paste0("Este acido graso insaturado par no puede tener mas de ", x,
                " insaturaciones"))
  }

  #Error 4:
  #Revisa para un acido graso par, si el numero de insaturaciones es 1/2 del numero de
  #carbonos menos 1, ya que 2 carbonos continuos no pueden tener ambos una insaturacion.
  if(C %% 2 > 0 & ins > (C-1)/2){
    x = (C-1)/2
    stop(paste0("Este acido graso insaturado impar no puede tener mas de ", x,
                " insaturaciones"))
  }
  
  #Calculo 1:
  #Ingresa el numero de carbonos (entre 4 y 24) e insaturaciones 
  #(si las posee) y devuelve el numero de ATPs generados.
  if(C > 3){
    if(C %% 2 == 0){
      vueltas = C/2 -1
      ATP = ((vueltas-ins)*1.5 + vueltas*2.5 + (vueltas+1)*11)-2
      print(ATP)
      
    }else{
      vueltas = (C-3)/2
      ATP = ((vueltas-ins)*1.5 + vueltas*2.5 + (vueltas)*11 + 5)-2
      print(ATP)
    }
  }

  #Calculo 2:
  #Especifico para cadenas de 3 carbonos.
  if(C == 3){
    print(5)
  }

  #Calculo 3:
  #Especifico para cadenas de 2 carbonos (con o sin insaturaciones).
  if(C == 2){
    print((2.5+11+(1-ins)*1.5)-2)
  } 
  
}
