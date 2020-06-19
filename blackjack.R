rm(list=ls()) # Limpia el espacio de trabajo de R
cartas <- c('A','K','Q','J','10',
            '9','8','7','6','5',
            '4','3','2') # Trece cartas por figura (digamos tr?boles)
baraja <- c(cartas,cartas,cartas,cartas) # Cuatro figuras por bajara (tr?boles, corazones, diamantes, espadas)
super_baraja <- c(baraja,baraja,baraja,baraja) # Cuatro barajas mezcladas
cartas_mayores <- cartas[2:5]

simulate_hand <-  function(baraja){
    # Esta funci?n simula una extracci?n de dos cartas de la s?per baraja (SB)
    
    # Primera carta:
    indice_primera <- sample(1:length(baraja), # Extrae al azar un entero entre 1 y la longitud de la SB 
                             size=1,# (tambi?n se puede extraer m?s de un elemento) 
                             replace=F) # Con/sin reemplazo (irrelevante cuando size=1)
    first_card <- baraja[indice_primera] # Extrae la primera carta...
    super_after_first <- baraja[-indice_primera] # ...s?cala de la s?per baraja...
    # Segunda carta:
    indice_segunda <- sample(1:length(super_after_first), # ...y extrae otra posici?n de la baraja recortada.
                             size=1,
                             replace=F)
    second_card <- super_after_first[indice_segunda] # Extrae segunda carta.
    
    # Mano
    hand <- c(first_card,second_card)
    return(hand)
}

# Simulaciones con objetos similares a los arrojados por la funcion simulate_hand

# Checa en la comparacion si 'A' esta en el objeto hand

hand <- c('Q','A')
hand=='A'
hand <- c('A','A')
hand=='A'
hand <- c('A','7')
hand=='A'
hand <- c('5','10')
hand=='A'

# Cuenta cuantos 'A' hay en cada mano

hand <- c('Q','A')
sum(hand=='A')
hand <- c('A','A')
sum(hand=='A')
hand <- c('A','7')
sum(hand=='A')
hand <- c('5','10')
sum(hand=='A')

# Checa si cada mano tiene un solo 'A'

hand <- c('Q','A')
sum(hand=='A')==1
hand <- c('A','A')
sum(hand=='A')==1
hand <- c('A','7')
sum(hand=='A')==1
hand <- c('5','10')
sum(hand=='A')==1

# Checa si cada elemento del objeto hand esta en el vector dado

hand <- c('Q','A')
hand%in%cartas_mayores
hand <- c('A','A')
hand%in%cartas_mayores
hand <- c('A','7')
hand%in%cartas_mayores
hand <- c('5','10')
hand%in%cartas_mayores
hand <- c('J','K')
hand%in%cartas_mayores

# Cuenta cuantas cartas de hand estan en el vector dado
# Basicamente suma cuantos 1 hay
hand <- c('Q','A')
sum(hand%in%cartas_mayores)
hand <- c('A','A')
sum(hand%in%cartas_mayores)
hand <- c('A','7')
sum(hand%in%cartas_mayores)
hand <- c('5','10')
sum(hand%in%cartas_mayores)
hand <- c('J','K')
sum(hand%in%cartas_mayores)

# Checa si una sola carta de hand esta en el vector a comparar de las figuras
hand <- c('Q','A')
sum(hand%in%cartas_mayores)==1
hand <- c('A','A')
sum(hand%in%cartas_mayores)==1
hand <- c('A','7')
sum(hand%in%cartas_mayores)==1
hand <- c('5','10')
sum(hand%in%cartas_mayores)==1
hand <- c('J','K')
sum(hand%in%cartas_mayores)==1

# Para que ganemos un blackjack las condiciones siguientes tienen que cumplir
# sum(hand=='A')==1 y sum(hand%in%c('K','Q','J','10')==1)==1
hand <- c('Q','A') # Esta es la unica condicion que sera verdadera y por tanto blackjack
sum(hand=='A')==1&sum(hand%in%cartas_mayores)==1
hand <- c('A','A')
sum(hand=='A')==1&sum(hand%in%cartas_mayores)==1
hand <- c('A','7')
sum(hand=='A')==1&sum(hand%in%cartas_mayores)==1
hand <- c('5','10')
sum(hand=='A')==1&sum(hand%in%cartas_mayores)==1
hand <- c('J','K')
sum(hand=='A')==1&sum(hand%in%cartas_mayores)==1

# Ahora a simular "manos" en multiples juegos de blackjack
n_simulations <- 1000
n_blackjack <- 0

for (i in 1:n_simulations) {
    hand <- simulate_hand(super_baraja)
    
    if (sum(hand=='A')==1 & sum(hand%in%cartas_mayores)==1) {
        n_blackjack <- n_blackjack + 1
    }
}

print(n_blackjack/n_simulations)

# Convirtiendo el bloque de codigo en una sola funcion
casino_night <- function(n_hands, baraja){
    # Esta funci?n simula una noche en el casino en la que se observan 'n_hands' manos
    
    # Noten que 'n_hands' ya no se define dentro de la funci?n,
    # y en cambio pasa como un argumento.
    
    n_blckjck <- 0 # Contador de blackjacks en 'n_hands' manos
    for(i in 1:n_hands){ # Repite las siguientes instrucciones para cada elemento 'i' del vector '1:n_hands':
        
        # a. En cada iteraci?n simula una mano (de dos cartas)
        hand <- simulate_hand(baraja)
        # b. y pregunta si es blackjack.  
        if(sum(hand=='A')==1&sum(hand%in%cartas_mayores)==1){ # En caso de que la mano sea blackjack:
            # b1. Agrega uno al conteo total de blckjcks.
            n_blckjck <- n_blckjck+1
        }
    }
    return(n_blckjck/n_hands) # La funci?n devuelve la prpoporci?n de blackjacks.
}

print(casino_night(10000, super_baraja))

# Simulando un año en el casino
nights <- c()

for (i in 1:365) {
    nights[i] <- casino_night(10000, super_baraja)
}

# Graficando la distribucion de las probabilidades de obtner un blackjack por cada noche de un a?o

hist(nights,xlim=c(0,0.1))
abline(v=0.0475,col='#0000eeaa',lwd=4)

##########################################################################

# Ejercicio 1
# 4 cartas que no fueron ni ases ni cartas mayores fueron retiradas de la baraja

extract_simple_cards <- function(baraja, cards_to_remove) {
    excluded_cards <- c("A", "K", "Q", "J", "10")
    safe_extraction <- F
    while (!safe_extraction) {
        extracted_cards <- sample(1:length(baraja), size = cards_to_remove, replace = F)
        if (!any(baraja[extracted_cards] %in% excluded_cards)) {
            safe_extraction <- T
        }
    }
    nueva_baraja <- baraja[-extracted_cards]
    return(nueva_baraja)
}

baraja_ejercicio_1 <- extract_simple_cards(super_baraja, 4)

nights_ejercicio_1 <- c()

for (i in 1:365) {
    nights_ejercicio_1[i] <- casino_night(10000, baraja_ejercicio_1)
}

hist(nights_ejercicio_1, xlim = c(0, 0.1))
abline(v=0.049,col='#0000eeaa',lwd=4)

##########################################################################
# Ejercicio 2
# Cada noche se juegan 1,000 manos en lugar de 10,0000
# No espero un cambio significativo respecto a si se juegan 10,0000 manos, tal vez
# solo cambien un poco los decimales, pero la probabilidad que se mostrará con mayor
# frecuencia será cercano a los 0.0475 de la simulación con 10,000 manos. Sin embargo,
# tendrá una mayor disperción, ya que mientras más simulaciones haya, más cercano será
# el valor de la probabilidad a 0.0475

nights_ejercicio_2 <- c()

for (i in 1:365) {
    nights_ejercicio_2[i] <- casino_night(1000, super_baraja)
}
hist(nights_ejercicio_2, xlim = c(0, 0.1))
abline(v=0.0475,col='#0000eeaa',lwd=4)

# Tal como se puede ver en el histograma de la distribución de probabilidad
# la disperción fue mayor, pero el valor de probabilidad más frecuente se mantuvo cercano
# a 0.0475. Esto por el número de eventos, teniendo infinitas simulaciones la probabilidad
# se acerca cada vez más a 0.0475, mientras menos eventos haya, mayor será la disperción
# de la distribución.