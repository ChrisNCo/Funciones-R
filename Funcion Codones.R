proteinas <- function(codones){
  
  # Octubre 2023
  # DESCRIPCIÓN: lee una secuencia de codones y le asigna el aminoácido correspondiente
  
  # ARGUMENTOS: 
  # codones: recibe un vector con una secuencia de codones, estos deben de estar entre
  # comillas
  
  # CODIGO:
  
  # Errores:
  # Este error se genera en caso de codón incompleto usando el comando for para crear 
  # un ciclo.
  y = length(codones)
  for(i in 1:y){
    z = nchar(x = codones[i])
    if(!z == 3){
      stop(paste0("El codon ", codones[i]," (codon numero ", i,")", " número de bases nitrogenadas en codón incorrecto"))
    }
  }
  
  # Este error se genera en caso de poner una letra que no corresponde a una base nitrogenada
  codones <- c("AUG", "UAA", "AUR")
  Q <- strsplit(codones, "")
  w <- unlist(Q)
  bases <- c("A", "C", "G", "U")
  a <- w %in% bases
  z <- sum(a) == length(a)
  if(z == FALSE){
    stop("Uno o varios codones no existen en cóodigo genético")
  }
  
  # Realización de función que reconoce codones
  aa <- function(codon){
    # Se establece una variable para cada codón, 
    # cada aminoácido está representado por una letra, por lo que las letras repetidas
    # corresponden a los codones sinónimos
    a <- c("AUG")
    b <- c("UGG")
    c <- c("GAC")
    cc <-  c("GAU")
    d <- c("AAC")
    dd <- c("AAU")
    e <- c("UGC")
    ee <- c("UGU")
    f <- c("UUC")
    ff <- c("UUU")
    g <- c("GAG")
    gg <- c("GAA")
    h <- c("CAA")
    hh <- c("CAG")
    i <- c("CAC")
    ii <- c("CAU") 
    j <- c("AAA")
    jj <- c("AAG")
    k <- c("UAC")
    kk <- c("UAU") 
    l <- c("AUA")
    ll <- c("AUC")
    lll <- c("AUU")
    m <- c("GCA")
    mm <- c("GCC")
    mmm <- c("GCG")
    mmmm <- c("GCU")
    n <- c("GGA")
    nn <- c("GGC")
    nnn <- c("GGG")
    nnnn <- c("GGU")
    o <- c("CCA")
    oo <- c("CCC")
    ooo <- c("CCG")
    oooo <- c("CCU")
    p <- c("ACA")
    pp <- c("ACC")
    ppp <- c("ACG")
    pppp <- c("ACU")
    r <- c("GUA")
    rr <- c("GUC")
    rrr <- c("GUG")
    rrrr <- c("GUU")
    s <- c("CGA")
    ss <- c("CGC")
    sss <- c("AGA")
    ssss <- c("CGG")
    sssss <- c("CGU")
    ssssss <- c("AGG")
    t <- c("CUA")
    tt <- c("CUC")
    ttt <- c("CUG")
    tttt <- c("CUU")
    ttttt <- c("UUA")
    tttttt <- c("UUG")
    u <- c("UCA")
    uu <- c("UCC")
    uuu <- c("UCG")
    uuuu <- c("UCU")
    uuuuu <- c("ACG")
    uuuuuu <- c("AGU")
    v <- c("UAA")
    vv <- c("UGA")
    vvv <- c("UAG")
    # Se establece un condicional para cada variable
    if(codon == a){
      w <- "Met"
    }
    if(codon == b){
      w <- "Trp"
    }
    if(codon == c){
      w <- "Asp"
    }
    if(codon == cc){
      w <- "Asp"
    }
    if(codon == d){
      w <- "Asn"
    }
    if(codon == dd){
      w <- "Asn"
    }
    if(codon == e){
      w <- "Cys"
    }
    if(codon == ee){
      w <- "Cys"
    }
    if(codon == f){
      w <- "Phe"
    }
    if(codon == ff){
      w <- "Phe"
    }
    if(codon == g){
      w <- "Glu"
    }
    if(codon == gg){
      w <- "Glu"
    }
    if(codon == h){
      w <- "Gln"
    }
    if(codon == hh){
      w <- "Gln"
    }
    if(codon == i){
      w <- "His"
    }
    if(codon == ii){
      w <- "His"
    }
    if(codon == j){
      w <- "Lys"
    }
    if(codon == jj){
      w <- "Lys"
    }
    if(codon == k){
      w <- "Tyr"
    }
    if(codon == kk){
      w <- "Tyr"
    }
    if(codon == l){
      w <- "Ile"
    }
    if(codon == ll){
      w <- "Ile"
    }
    if(codon == lll){
      w <- "Ile"
    }
    if(codon == m){
      w <- "Ala"
    }
    if(codon == m){
      w <- "Ala"
    }
    if(codon == m){
      w <- "Ala"
    }
    if(codon == mm){
      w <- "Ala"
    }
    if(codon == mmm){
      w <- "Ala"
    }
    if(codon == mmmm){
      w <- "Ala"
    }
    if(codon == n){
      w <- "Gly"
    }
    if(codon == nn){
      w <- "Gly"
    }
    if(codon == nnn){
      w <- "Gly"
    }
    if(codon == nnnn){
      w <- "Gly"
    }
    if(codon == o){
      w <- "Pro"
    }
    if(codon == oo){
      w <- "Pro"
    }
    if(codon == ooo){
      w <- "Pro"
    }
    if(codon == oooo){
      w <- "Pro"
    }
    if(codon == p){
      w <- "Thr"
    }
    if(codon == pp){
      w <- "Thr"
    }
    if(codon == ppp){
      w <- "Thr"
    }
    if(codon == pppp){
      w <- "Thr"
    }
    if(codon == r){
      w <- "Val"
    }
    if(codon == rr){
      w <- "Val"
    }
    if(codon == rrr){
      w <- "Val"
    }
    if(codon == rrrr){
      w <- "Val"
    }
    if(codon == s){
      w <- "Arg"
    }
    if(codon == ss){
      w <- "Arg"
    }
    if(codon == sss){
      w <- "Arg"
    }
    if(codon == ssss){
      w <- "Arg"
    }
    if(codon == sssss){
      w <- "Arg"
    }
    if(codon == ssssss){
      w <- "Arg"
    }
    if(codon == t){
      w <- "Leu"
    }
    if(codon == tt){
      w <- "Leu"
    }
    if(codon == ttt){
      w <- "Leu"
    }
    if(codon == tttt){
      w <- "Leu"
    }
    if(codon == ttttt){
      w <- "Leu"
    }
    if(codon == tttttt){
      w <- "Leu"
    }
    if (codon == u){
      w <- "Ser"
    }
    if (codon == uu){
      w <- "Ser"
    }
    if (codon == uuu){
      w <- "Ser"
    }
    if (codon == uuuu){
      w <- "Ser"
    }
    if (codon == uuuuu){
      w <- "Ser"
    }
    if (codon == uuuuuu){
      w <- "Ser"
    }
    if(codon == v){
      w <- "STOP"
    }
    if(codon == vv){
      w <- "STOP"
    }
    if(codon == vvv){
      w <- "STOP"
    }
    w
  }
  # Se crea ciclo que permite reconocer vector de codones y realiza la secuencia de 
  # aminoácidos
  y <- length(codones)
  res <- rep(NA, y)
  for (i in 1:y) {
    res[i] <- aa(codon = codones[i])
  }
  res # se imprime secuencia de aminoácidos
}

# Ejemplo:
# proteinas(codones = c("AUG", "AAU", "ACU", "GCC"))