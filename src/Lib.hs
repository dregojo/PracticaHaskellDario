{-positivo :: Ord a => a => a
positivo
        | x < 0 == True = False
        | x > 0 == False = True
-}
positivo :: Number -> Bool
positivo numero = numero > 0

{-costoEntrada :: Number -> Number
costoEntrada edad
        |edad > 18 = 100 + 2 * (edad + 1)
        |edad > 60 = 100 + 2 * (edad / 2 - 20)
        |otherwise = 100 + 2 * (18 - edad)
-}
costoEntrada :: Number -> Number
costoEntrada edad
        |edad <= 18 = costoBase (18 - edad)
        |edad > 60 = costoBase (edad / 2 - 20)
        |otherwise = costoBase (edad + 1)
        
costoBase :: Number -> Number
costoBase factor = 100 + 2 * factor
{-


devuelveAlgo :: (Number -> Bool) -> Number -> Number -> (Number -> Number) -> Number

x :: Number -> Bool
y :: Number
z :: Number
w :: Number -> Number

devuelveOtraCosa a b c = (> a).length.filter b. map c

devuelveOtraCosa :: Number -> (y -> Bool) -> (x -> y) -> Bool
a :: Number
b :: y -> Bool
c :: x -> y
-}
devuelveAlgo x y z w
        |x y && x z = w y + z
        |otherwise = z

devuelveOtraCosa a b c = (> a).length.filter b. map c

mayoresQueNAlAplicar :: Number -> (Number -> Number) -> [Number] -> [Number]
mayoresQueNAlAplicar n funcion = filter ((> n).funcion)
--Otra forma de hacer lo mismo usando expresión lambda
mayoresQueNAlAplicar' :: Number -> (Number -> Number) -> [Number] -> [Number]
mayoresQueNAlAplicar' n funcion = filter (\elemento -> funcion elemento > n)

maximoSegun funcion lista = maximum (filter (\elemento -> funcion elemento == maximo funcion lista) lista)
maximo funcion = maximum.map funcion

type Partido = String

data Mesa = UnaMesa {
        distrito :: String,
        votos :: [Partido],
        cantidadVotantesHabilitados :: Number
}deriving (Show,Ord,Eq)

data Eleccion = UnaEleccion{
        mesas :: [Mesa],
        partidos :: [Partido]
}deriving Show

cantidadVotosMesa partido = length.filter(== partido).votos
cantidadVotosEleccion partido = sum.map(cantidadVotosMesa partido).mesas
partidoGanador eleccion = maximoSegun (`cantidadVotosEleccion` eleccion) (partidos eleccion)
votosPorPartido eleccion = map(`cantidadVotosEleccion` eleccion) (partidos eleccion)
eleccionIrregular = any mesaIrregular.mesas
mesaIrregular mesa = ((> cantidadVotantesHabilitados mesa).length.votos) mesa
mesaConMenorIndiceDeAusentismo eleccion = maximoSegun presentismo (mesas eleccion)
presentismo mesa = min (length (votos mesa)/cantidadVotantesHabilitados mesa) 1

mesa1 = UnaMesa{
        distrito = "paternal",
        votos = ["lla","lla","pro","fp","lla","fit","un","fit"],
        cantidadVotantesHabilitados = 10
}
mesa2 = UnaMesa{
        distrito = "paternal",
        votos = ["un","pro"],
        cantidadVotantesHabilitados = 2
}
mesa3 = UnaMesa{
        distrito = "almagro",
        votos = ["fit","fp","fit"],
        cantidadVotantesHabilitados = 1
}
eleccion1 = UnaEleccion{
        mesas = [mesa1,mesa2,mesa3],
        partidos = ["fp","fit","pro","un","lla"]
}
eleccion2 = UnaEleccion{
        mesas = [mesa1,mesa2],
        partidos = ["fp","fit","pro","un","lla"]
}
-}
