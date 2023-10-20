atp_ag = function(C, ins = 0){
  
  if(C<2 | C>24){
    stop("La cadena no puede ser menor a 2 carbonos o mayor a 24 carbonos")
  }
  
  if(ins > 6){
    stop("La cadena no puede poseer mas de 6 insaturaciones")
  }
  
  if(ins > C){
    stop("Un acido graso no puede tener mas insaturaciones que carbonos")
  }
  
  if(C %% 2 == 0 & ins > C/2){
    x = C/2
    stop(paste0("Este acido graso insaturado par no puede tener mas de ", x,
                " insaturaciones"))
  }
  
  if(C %% 2 > 0 & ins > (C-1)/2){
    x = (C-1)/2
    stop(paste0("Este acido graso insaturado impar no puede tener mas de ", x,
                " insaturaciones"))
  }
  
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
  
  if(C == 3){
    print(5)
  }
  
  if(C == 2){
    print((2.5+11+(1-ins)*1.5)-2)
  }
  
}