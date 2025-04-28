module Lib () where

data Poder = UnPoder {
    nombre :: String,
    danio :: Int,
    vida :: Int
}

data Personaje = UnPersonaje {
    nombre :: String,
    poder :: Poder,
    superPoder :: Poder,
    estadoDeSuperPoder :: Bool,
    cantidadDeVida :: Int
}

recibirDanio :: Int -> Int -> Int
recibirDanio cantidadDeVida daño
    | cantidadDeVida <= 1000 = 0
    | otherwise = cantidadDeVida - 1000

bolaEspinosa :: Personaje -> Int
bolaEspinosa personaje 
    | personaje.nombre == "contrincante" = recibirDanio personaje.cantidadDeVida 1000
    | otherwise = 0

sanar :: Int -> Int -> Int
sanar cantidadDeVida vida = cantidadDeVida + vida

reducirVida :: Int -> Int -> Int
reducirVida numero cantidadDeVida = cantidadDeVida `div` numero

lluviaDeTuercas :: Personaje -> Int
lluviaDeTuercas personaje
    | personaje.nombre == "colega" = sanar personaje.cantidadDeVida 800
    | personaje.nombre == "contrincante" = reducirVida 2 personaje.cantidadDeVida
    | otherwise = 0

granadaDeEspinas :: Personaje -> Int -> Int
granadaDeEspinas contrincante radio
    | contrincante.cantidadDeVida < 800 && radio > 3 = recibirDanioYDesactivar personaje
    | otherwise = 0

conPocaVida :: Personaje -> String
conPocaVida personaje
    | personaje.vida < 800 = personaje.nombre
    | otherwise = " "

quienesTinenPocaVida :: [Personaje] -> [String]
quienesTinenPocaVida personajes = filter conPocaVida personajes

espinas :: Personaje
espinas = UnPersonaje "Espinas" bolaEspinosa granadaDeEspinas 4800

pamela :: Personaje
pamela = UnPersonaje "Pamela" lluviaDeTuercas torretaCurativa 9600
