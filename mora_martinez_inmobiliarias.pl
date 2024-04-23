% Orientaciones de los pisos
este.
oeste.
norte.
sur.

% Las viviendas pueden ser interiores o exteriores (dar a un patio de luces o a la calle).
exterior.
interior.

% Distintos tipos de vivienda 
piso.
casa.
loft.
atico. 

% Varias caracteristicas de las viviendas pueden estar presentes o ausentes
si.
no.

% Las viviendas se localizan en las afueras o en el centro de la ciudad
afueras.
centro.

% Declaracion de predicados dinamicos (seran modificados por el programa)
:- dynamic vivienda/2, anuncios_viviendas/1, viviendas_visitadas/1, contador_semanas/1, maximo_precio/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Caracteristicas de las viviendas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

precio(X, Y):- vivienda(X,[Y,_, _,_, _,_,_, _,_, _,_,_, _,_, _ , _]).
metros(X, Y):- vivienda(X,[_,Y, _,_, _,_,_, _,_, _,_,_, _,_, _ , _]).
anyo_construccion(X, Y):- vivienda(X,[_,_, Y,_, _,_,_, _,_, _,_,_, _,_, _ , _]).
tipo_vivienda(X, Y):- vivienda(X,[_,_, _,Y, _,_,_, _,_, _,_,_, _,_, _ , _]).
altura(X, Y):- vivienda(X,[_,_, _,_, Y,_,_, _,_, _,_,_, _,_, _ , _]).
orientacion_comedor(X, Y):- vivienda(X,[_,_, _,_, _,Y,_, _,_, _,_,_, _,_, _ , _]).
orientacion_habitaciones(X, Y):- vivienda(X,[_,_, _,_, _,_,Y, _,_, _,_,_, _,_, _ , _]).
comedor_interior_o_exterior(X, Y):- vivienda(X,[_,_, _,_, _,_,_, Y,_, _,_,_, _,_, _ , _]).
habitaciones_interior_o_exterior(X, Y):- vivienda(X,[_,_, _,_, _,_,_, _,Y, _,_,_, _,_, _ , _]).
tiene_terraza(X, Y):- vivienda(X,[_,_, _,_, _,_,_, _,_, Y,_,_, _,_, _ , _]).
num_habitaciones(X, Y):- vivienda(X,[_,_, _,_, _,_,_, _,_, _,Y,_, _,_, _ , _]).
num_banyos(X, Y):- vivienda(X,[_,_, _,_, _,_,_, _,_, _,_,Y, _,_, _ , _]).
tiene_ascensor(X, Y):- vivienda(X,[_,_, _,_, _,_,_, _,_, _,_,_, Y,_, _ , _]).
garaje_incluido(X, Y):- vivienda(X,[_,_, _,_, _,_,_, _,_, _,_,_, _,Y, _ , _]).
trastero_incluido(X, Y):- vivienda(X,[_,_, _,_, _,_,_, _,_, _,_,_, _,_, Y, _]).
zona(X, Y):- vivienda(X,[_,_, _,_, _,_,_, _,_, _,_,_, _,_, _, Y]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Declaramos los requisitos minimos del comprador
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maximo_precio(500).
precio_sospechoso(40).
minimo_metros(20).
antiguedad_maxima(1960).
orientacion_preferida(este).
int_o_ext_preferido(exterior).
habitciones_minimas(2).
altura_minima(0).
minimo_banyos(1).
ascensor_requerido(no).
garaje_requerido(no).
trastero_requerido(no).

n_viviendas_semana(40). % Num anuncios nuevos que aparecen cada semana
descartar_primeras_n(50). % Num viviendas que se visitan y se descartan al principio

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Declaramos unas cuantas viviendas de prueba %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vivienda(v1,[200, 90, 2000, piso, 7, este, sur, exterior, interior, no, 30, 1, si, si, no, afueras]).
vivienda(v2,[200, 70, 1980, piso, 3, norte, este, interior, exterior, no, 3, 0, si, si, si, afueras]).
vivienda(v3,[250, 80, 1955, piso, 1, este, este, exterior, interior, no, 1, 2, si, si, no, centro]).
vivienda(v4,[150, 90, 1980, piso, 7, norte, sur, exterior, exterior, si, 4, 1, si, no, si, centro]).
vivienda(v5,[50, 60, 1955, piso, 1, este, sur, interior, interior, si, 2, 1, si, no, si, centro]).

%test incoh4
vivienda(v6,[50, 60, 2000, casa, 1, este, sur, interior, interior, si, 2, 1, no, no, si, centro]).
vivienda(v7,[50, 60, 2000, loft, 1, este, sur, interior, interior, si, 4, 1, no, no, si, centro]).
vivienda(v8,[50, 60, 1920, loft, 1, este, sur, interior, interior, si, 1, 1, no, no, si, centro]).

%test incoh1
vivienda(v10,[50, 60, 2000, atico, 1, este, sur, interior, interior, si, 2, 1, no, no, si, centro]).
vivienda(v11,[50, 60, 2000, atico, 1, este, sur, interior, interior, no, 4, 1, no, no, si, centro]).

%test incoh2 y 3
vivienda(v12,[50, 60, 1920, loft, 1, este, sur, interior, interior, si, 4, 1, no, no, si, centro]).
vivienda(v13,[400, 60, 2010, loft, 1, este, sur, interior, interior, si, 4, 1, no, no, si, centro]).
vivienda(v14,[400, 60, 1920, loft, 1, este, sur, interior, interior, si, 4, 1, no, no, si, centro]).
vivienda(v15,[200, 60, 2010, loft, 1, este, sur, interior, interior, si, 4, 1, no, no, si, centro]).
vivienda(v16,[200, 60, 1920, loft, 1, este, sur, interior, interior, si, 4, 1, no, no, si, centro]).
vivienda(v17,[200, 60, 2010, loft, 1, este, sur, interior, interior, si, 4, 1, no, no, si, afueras]).
vivienda(v18,[400, 60, 2010, loft, 1, este, sur, interior, interior, si, 4, 1, no, no, si, afueras]).


%test incoh5
vivienda(v20,[50, 60, 1920, loft, 1, este, sur, interior, interior, si, 4, 1, no, no, si, centro]).
vivienda(v21,[400, 100, 2010, loft, 1, este, sur, interior, interior, si, 4, 1, no, no, si, centro]).
vivienda(v22,[400, 200, 1920, loft, 1, este, sur, interior, interior, si, 4, 1, no, no, si, centro]).
vivienda(v23,[50, 60, 1920, loft, 1, este, sur, interior, interior, si, 1, 1, no, no, si, centro]).
vivienda(v24,[400, 100, 2010, loft, 1, este, sur, interior, interior, si, 1, 1, no, no, si, centro]).
vivienda(v25,[400, 200, 1920, loft, 1, este, sur, interior, interior, si, 1, 1, no, no, si, centro]).

vivienda(v26,[50, 60, 1920, piso, 1, este, sur, interior, interior, si, 4, 1, no, no, si, centro]).
vivienda(v27,[400, 100, 2010, piso, 1, este, sur, interior, interior, si, 4, 1, no, no, si, centro]).
vivienda(v28,[400, 200, 1920, piso, 1, este, sur, interior, interior, si, 4, 1, no, no, si, centro]).
vivienda(v29,[50, 60, 1920, piso, 1, este, sur, interior, interior, si, 1, 1, no, no, si, centro]).
vivienda(v30,[400, 100, 2010, piso, 1, este, sur, interior, interior, si, 1, 1, no, no, si, centro]).
vivienda(v31,[400, 200, 1920, piso, 1, este, sur, interior, interior, si, 1, 1, no, no, si, centro]).

vivienda(v32,[180, 84, 1998, piso, 1, este, oeste, exterior, interior, si, 3, 1, si, si, no, afueras]).

% Declaramos una lista con todas las viviendas.
anuncios_viviendas([v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20,
                    v21, v22, v23, v24, v25, v26, v26, v28, v29, v30, v31, v32]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Reglas para comprobar si una vivienda cumple con las caracteristicas necesarias %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Estas reglas calculan caracteristicas 'secundarias' a partir de las caracteristicas del anuncio.

%% El piso esta en rango de precio si no es demasiado barato ni demasiado caro
puedo_pagar(X) :- precio(X, Y), maximo_precio(W), Y =< W.
no_es_sospechoso(X) :- precio(X, Y), precio_sospechoso(W), Y>W.
precio_en_rango(X) :- puedo_pagar(X), no_es_sospechoso(X),
                            write("El precio esta en el rango adecuado"),
                            nl.  

%% El piso esta en rango de metros si tiene mas metros que los minimos requeridos
cumple_minimo_tamanyo(X) :- metros(X, Y), minimo_metros(W), Y >= W,
                            write("Tiene al menos "),
                            write( W),
                            write(" metros cuadrados"),
                            nl.

%% El piso es lo suficientemente nuevo o bien esta en el centro
cumple_antiguedad_minima(X) :- anyo_construccion(X, Y), antiguedad_maxima(W), Y >= W.
cumple_antiguedad_o_zona_centro(X) :- zona(X, Y), (cumple_antiguedad_minima(X); Y = centro),
                            write("Es lo suficientemente nuevo"),
                            nl.

%% Tiene suficientes habitaciones
tiene_habitciones_minimas(X) :- num_habitaciones(X, Y), habitciones_minimas(W), Y >= W,
                            write("Tiene al menos "),
                            write( W),
                            write(" habitaciones"),
                            nl.

%% Tiene suficientes banyos
tiene_banyos_minimos(X) :- num_banyos(X, Y), minimo_banyos(W), Y >= W,
                            write("Tiene al menos "),
                            write(W),
                            write(" banyos"),
                            nl.

%% Cumple requisitos garaje y trastero
cumple_garaje(X) :-  garaje_incluido(X, Y), 
                     garaje_requerido(W), 
                     (W = si, Y = si ; W = no).
cumple_trastero(X) :-  trastero_incluido(X, Y), trastero_requerido(W), 
                       (W = si, Y = si ; W = no).
cumple_garaje_y_trastero(X) :- cumple_garaje(X), cumple_trastero(X),
                            write("Cumple condicion garaje y trastero."),
                            nl.

%% Cumple requisito ascensor
cumple_ascensor(X) :-  tiene_ascensor(X, Y), ascensor_requerido(W), (W = si, Y = si ; W = no).

%% O el comedor o las habitaciones dan al exterior
da_al_exterior(X) :- comedor_interior_o_exterior(X, Z), 
                          habitaciones_interior_o_exterior(X, W), 
                          int_o_ext_preferido(Pref),                         
                          (Z = Pref; W = Pref),
                          write("da al "),
                          write(Pref),
                          nl.

%% Hay ventilacion cruzada si las habitaciones y el comedor tienen orientacion distinta
ventilacion_cruzada(X) :- orientacion_comedor(X, Z), 
                          orientacion_habitaciones(X, W), 
                          Z \= W,
                          write("Tiene ventilacion cruzada"),
                          nl.

%% Alguna de las orientaciones se corresponde con la preferida del comprador
cumple_orientacion(X) :- orientacion_comedor(X, Y), orientacion_habitaciones(X, W), orientacion_preferida(Z), (Y = Z; W = Z).

%% Tiene que tener una altura superior al 3er piso, o bien ser un primero con terraza
cumple_altura_o_terraza(X) :- altura(X, Y), tiene_terraza(X, W), altura_minima(Z), (Y >= Z; Y = 1, W = si).

% Cumple condiciones de ventilacion e iluminacion globales
buena_ventilacion_e_ilum(X) :- da_al_exterior(X), ventilacion_cruzada(X), cumple_orientacion(X), cumple_altura_o_terraza(X).

%% Cumple con todas las caracteristicas globales del piso
cumple_caracteristicas_globales(X) :- precio_en_rango(X), 
                                      cumple_minimo_tamanyo(X), 
                                      tiene_habitciones_minimas(X),
                                      tiene_banyos_minimos(X),
                                      cumple_garaje_y_trastero(X),
                                      cumple_ascensor(X),
                                      cumple_antiguedad_o_zona_centro(X),
                                      buena_ventilacion_e_ilum(X),
                                      write("Cumple con todas las caracteristicas necesarias."),
                                      nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Reglas para detectar incoherencias en el anuncio 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Estas reglas sirven para detectar configuraciones improbables en las viviendas anunciadas,
%% y asi descartarlas puesto que probablemente se trate de un anuncio fraudulento.

%% Un atico tiene que tener terraza
incoh1(X) :- tipo_vivienda(X, atico), tiene_terraza(X, no).

% Un Loft no puede tener mas de tres habitaciones
incoh2(X) :- tipo_vivienda(X, loft), num_habitaciones(X, W), W > 2.

% En el centro, por menos de 300.000, no puede ser nuevo
incoh3(X) :- zona(X, centro), precio(X, W), anyo_construccion(X, Z), W < 300, Z > 2000.

% Una vivienda posterior a 1980, que no es una casa, siempre tiene ascensor
incoh4(X) :- tiene_ascensor(X, no), tipo_vivienda(X, W), anyo_construccion(X, Z), W \= casa, Z > 1980.

% Una vivienda con mas de 100 metros, que no es un loft, no puede tener menos de 3 habitaciones
incoh5(X) :- tipo_vivienda(X, Y), metros(X, W), num_habitaciones(X, Z), Y \= loft, W > 100, Z < 3.

%% Detectar incoherencias sospechosas en el anuncio
anuncio_parece_falso(X) :- (incoh1(X);
                           incoh2(X);
                           incoh3(X);
                           incoh4(X);
                           incoh5(X)),
                           write("Hay inconsistencias en el anuncio."),
                           nl, nl.

visitar_piso(X) :- \+ anuncio_parece_falso(X), cumple_caracteristicas_globales(X),
                          write("ES UN PISO A VISITAR."),
                          nl, nl.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% PROBAMOS LA LOGICA DEL COMPRADOR CON LAS VIVIENDAS DE PRUEBA %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

nuevas_viviendas([]).

% Regla base para la lista vacía
lista_visitar([], []).
% Regla para encontrar los elementos que cumplen con visitar_piso en la cabeza de la lista
lista_visitar([X|Resto], [X|Viviendas]) :-
    visitar_piso(X), % Verificar si el piso cumple con la regla visitar_piso
    lista_visitar(Resto, Viviendas).
% Regla para descartar los elementos que no cumplen con visitar_piso en la cabeza de la lista
lista_visitar([_|Resto], Viviendas) :-
    lista_visitar(Resto, Viviendas).


run_lista_visitar :-
    anuncios_viviendas(X),
    lista_visitar(X, Y),
     write('Lista de viviendas anunciadas: '), nl,
    write(X), nl, nl,
    write('Viviendas que merece la pena visitar: '), nl,
    write(Y), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%%%%%%%%%%%%%%%%% GENERADOR DE ANUNCIOS DE VIVIENDAS ALEATORIOS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

%% Primero esta el codigo para generar las caracteristicas una a una
%% Luego esta el codigo para generar una vivienda
%% Por ultimo, esta el codigo para generar N viviendas 

% Generar la zona del piso al azar. Sera determinante para otras caracteristicas del piso

generar_zona(Zona):- random_member(Zona, [centro, afueras]).

% Los pisos en el centro no tienen ni trastero ni garaje
generar_trastero_y_garaje(P, afueras):-random_member(P, [si, no]).
generar_trastero_y_garaje(no, centro).

% Los pisos en el centro se construyeron entre 1850 y 1960; los pisos en las afueras entre 1950 y 2008 (anyo de la crisis).
generar_anyo(D, afueras):- random_between(1950, 2008,D).
generar_anyo(D, centro):- random_between(1850, 1960,D).

% Generar terraza
generar_terraza(P):-random_member(P, [si, no]).

% Generar ascensor. Si el piso es anterior a 1980 a veces tiene y a veces no; si es posterior, siempre tiene ascensor.
generar_ascensor(Ascensor, Anyo):- Anyo < 1980, random_member(Ascensor, [si, no]).
generar_ascensor(si, Anyo):- Anyo >= 1980.

% Tamanyo de los pisos
generar_metros(D):- random_between(20, 150, D).
 
% Generar Num habitaciones dependiendo de los metros cuadrados del piso
generar_habitaciones(L, Metros):- Metros >= 100, random_between(0, 4,L).
generar_habitaciones(L, Metros):- Metros < 100, Metros > 60, random_between(0, 2,L).
generar_habitaciones(L, Metros):- Metros =< 60, random_between(0, 1,L).

% Generar Num Banyos dependiendo de los metros cuadrados del piso. Si es menor de 70 metros tendra 1 banyo, si es mayor tendra 1 o 2.
generar_banyos(L, Metros):- Metros >= 70, random_between(1, 2,L).
generar_banyos(1, Metros):- Metros < 70.

% Generar orientacion interior o exterior. En los anuncios, estas orientaciones no son fieles a la realidad sino que se ponen al azar,
%   De ahi que sea necesario para el comprador darse cuenta de las contradicciones de los anuncios. 

generar_interior_o_exterior(X):- random_member(X, [interior, exterior]).

% Generar orientacion en comedor y habitaciones. De nuevo, se pone al azar en los anuncios.

generar_orientacion(X):- random_member(X, [este, oeste, norte, sur]).

% Generar tipo. El tipo a menudo esta equivocado en los anuncios, por lo que no se tienen en cuenta otras caracteristicas
%  Por ejemplo, es comun que a un ultimo piso lo llamen atico, aunque no lo sea, etc. 
generar_tipo_de_vivienda(X):- random_member(X, [piso, loft, atico, casa]).

% Generar altura dependiendo del tipo de vivienda. La altura 0 en un piso significa que es un bajo.
generar_altura(Altura, loft):- random_between(0, 12, Altura).
generar_altura(Altura, piso):- random_between(0, 12, Altura).
generar_altura(Altura, casa):- random_between(0, 1, Altura).
generar_altura(Altura, atico):- random_between(2, 12, Altura).


% Generar el precio del piso dependiendo de sus caracteristicas

% Factores modificadores del precio. El precio final, en miles de Euros, es el producto de todos ellos mas cierta cantidad aleatoria.

factor_tipo(atico, 3).
factor_tipo(loft, 2).
factor_tipo(piso, 1).
factor_tipo(casa, 1).

factor_terraza(si, 2).
factor_terraza(no, 1).

factor_zona(centro, 3).
factor_zona(afueras, 1).

factor_garaje(si, 1.5).
factor_garaje(no, 1).

factor_altura(Altura, 2) :- Altura > 4.
factor_altura(Altura, 1) :- Altura =< 4.

generar_precio(Precio, Metros, Zona, Altura, Tipo, Terraza, Garaje) :- 
    factor_tipo(Tipo, FTipo),
    factor_terraza(Terraza, FTer),
    factor_zona(Zona, FZona),
    factor_garaje(Garaje, FGar),
    factor_altura(Altura, FAlt),
    random_between(0, 50,L),
    Precio is Metros*FTipo*FTer*FZona*FGar*FAlt + L.


% Generar nombre. Los identificadores de los pisos son cadenas de caracteres aleatorios. 
generar_nombre_list([], 0):- !.
generar_nombre_list([Nuevo|Nombre], N):-
    N > 0,
    random_between(97, 122, ASCII_Codigo), 
    char_code(Nuevo, ASCII_Codigo), %generar numeros correspondientes a caracteres ASCII
    N2 is N - 1,
    generar_nombre_list(Nombre, N2).

generar_nombre(Atom, N):- 
    generar_nombre_list(Nombre_lista, N), 
    %atom_chars(Atom, Nombre_lista), % Traducir a caracteres
    atom_string(Atom, Nombre_lista).  % Convertir a string


% Generar un anuncio de una vivienda con caracteristicas aleatorias.
generarAnuncioVivienda(vivienda(Nombre, [Precio,Metros,Anyo,Tipo,
                Altura,OriComedor,
                OriHabits,ExtIntComedor,
                ExtIntHabs,Terraza,NHabs,
                NBanyos,Ascensor,Garaje,Trastero, Zona])):-
        generar_zona(Zona),
        generar_trastero_y_garaje(Trastero, Zona),
        generar_trastero_y_garaje(Garaje, Zona),
        generar_anyo(Anyo, Zona),
        generar_metros(Metros),
        generar_terraza(Terraza),
        generar_ascensor(Ascensor, Anyo),
        generar_habitaciones(NHabs, Metros),
        generar_banyos(NBanyos, Metros),
        generar_interior_o_exterior(ExtIntComedor),
        generar_interior_o_exterior(ExtIntHabs),
        generar_orientacion(OriComedor),
        generar_orientacion(OriHabits),
        generar_tipo_de_vivienda(Tipo),
        generar_altura(Altura, Tipo),
        generar_precio(Precio, Metros, Zona, Altura, Tipo, Terraza, Garaje),
        generar_nombre(Nombre, 20).



%% Generar N viviendas aleatorias
generar_n_viviendas(Viviendas, N) :- generar_n_viviendas([], Viviendas, N).

generar_n_viviendas(Viviendas, Viviendas, 0) :- !.
generar_n_viviendas(Acumulador, Viviendas, N) :-
    N > 0,
    generarAnuncioVivienda(Vivienda),
    N1 is N - 1,
    generar_n_viviendas([Vivienda | Acumulador], Viviendas, N1),
    assertz(Vivienda),
    write("Vivienda generada: "),
    write(Vivienda),
    nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%%%%%%%%%%%%%%%%% PROCESO DE GENERAR N ANUNCIOS SEMANALMENTE, SELCCIONARLOS Y VISITAR   %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

%% Se generarn N viviendas, se seleccionan aquellas que cumplen con los requisitos 
%% y se anyaden a la lista de viviendas visitadas

% Regla base para la lista vacía
lista_visitar2([], []).
% Regla para encontrar los elementos que cumplen con visitar_piso en la cabeza de la lista
lista_visitar2([vivienda(A, B)|Resto], [vivienda(A, B)|Viviendas]) :-
    visitar_piso(A), % Verificar si el piso cumple con la regla visitar_piso
    lista_visitar2(Resto, Viviendas).
% Regla para descartar los elementos que no cumplen con visitar_piso en la cabeza de la lista
lista_visitar2([_|Resto], Viviendas) :-
    lista_visitar2(Resto, Viviendas).


% Tanda de visitas semanales. Cada semana, aparecen 20 anuncios nuevos en el portal, y el comprador decide cuales visitar. 
viviendas_visitadas([]).
run_lista_visitar_random_semana :-
    write('GENERANDO LISTADO DE ANUNCIOS SEMANALES: '), nl,
    n_viviendas_semana(ViviendasPorSemana),
    generar_n_viviendas(X, ViviendasPorSemana),
    %assertz(anuncios_viviendas(X)),
    lista_visitar2(X, Y),
    subtract(X, Y, NoVisitar),
    length(X, LengthX),
    length(Y, LengthY),
    length(NoVisitar, LengthNV),
    write('- Lista de viviendas anunciadas esta semana: '), nl,
    write(X), nl, nl,
    write('- Viviendas que merece la pena visitar esta semana: '), nl,
    write(Y), nl,
    write('- Viviendas que no merece la pena visitar esta semana: '), nl,
    write(NoVisitar), nl,
    write('--> Esta semana merece la pena visitar '), write(LengthY), write(" viviendas, y descartamos "),  
    write(LengthNV), write(" de los "),  write(LengthX), write(" anuncios semanales."), nl, nl,
    viviendas_visitadas(W),
    append(W, Y, Z),
    retract(viviendas_visitadas(_)),
    assertz(viviendas_visitadas(Z)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%%%%%%%%%%%%%%%%% ETAPA DE VISITAS 1: DESCARTAR LAS PRIMERAS 50 VIVIENDAS    %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

% El comprador va visitando viviendas semana tras semana hasta haber visitado un minimo de 50.
contador_semanas(0).
run_semanas_hasta_ver_N_pisos :- 
        run_lista_visitar_random_semana,
        descartar_primeras_n(NumDescartes),
        contador_semanas(Semanas_transcurridas),
        Semanas_transcurridas2 is Semanas_transcurridas + 1,
        retract(contador_semanas(Semanas_transcurridas)),
        assertz(contador_semanas(Semanas_transcurridas2)),
        viviendas_visitadas(X),
        length(X, LengthVisitadas),
        write('==> Hasta ahora hemos visitado '), write(LengthVisitadas), 
        write(' viviendas en '), write(Semanas_transcurridas2), write(" semanas, en la ronda de descartes."), nl, nl, nl, 
        ( LengthVisitadas < NumDescartes -> run_semanas_hasta_ver_N_pisos; true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%%%%%%%%%%%%%%%%% ETAPA DE VISITAS 2: QUEDARSE CON LA PRIMERA MAS BARATA QUE LAS ANTERIORES %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5


%Iterar hasta encontrar una vivienda mas barata que el precio mas barato de las primeras 50 viviendas
run_semanas_hasta_ver_mas_barata :-
        run_lista_visitar_random_semana,
        contador_semanas(Semanas_transcurridas),
        Semanas_transcurridas2 is Semanas_transcurridas + 1,
        retract(contador_semanas(Semanas_transcurridas)),
        assertz(contador_semanas(Semanas_transcurridas2)),
        viviendas_visitadas(X),
        length(X, LengthVisitadas),
        maximo_precio(Max),
        write('--> Una vez ajustado el precio maximo a '),  write(Max), write(', hemos visitado '), write(LengthVisitadas), 
        write(' viviendas en '), write(Semanas_transcurridas2), write(" semanas."), nl, nl, nl, 
        ( LengthVisitadas < 1 -> run_semanas_hasta_ver_mas_barata; true).        



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%%%%%%%%%%%%%%%%% PROCESO PRINCIPAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

%%   EL PROCESO PRINCIPA CONSISTE EN:
%%   ETAPA 1, CALCULAR PRECIO MINIMO VISITANDO 50 VIVIENDAS CON LAS CARACTERISTICAS ADECUADAS 
%%   ETAPA 2: SEGUIR VISITANDO VIVIENDAS HASTA ENCONTRAR UNA CON LAS CARACTERISTICAS Y MAS BARATA QUE LAS 50 PRIMERAS
%%   IMPRIMIR LA VIVIENDA ENCONTRADA Y FINALIZAR

% Funcion auxiliar para calcular la vivienda mas barata a partir de una lista
calcular_vivienda_mas_barata([], _):- !.

calcular_vivienda_mas_barata([Vivienda], Vivienda).

calcular_vivienda_mas_barata([vivienda(X, _)|Resto], ViviendaMasBarata):-
    calcular_vivienda_mas_barata(Resto, vivienda(W, _)),
    precio(X, PrecioX),
    precio(W, PrecioW),
    (PrecioX =< PrecioW -> ViviendaMasBarata = vivienda(X, _); ViviendaMasBarata = vivienda(W, _)).

% Funcion auxiliar para devolver el precio a partir de un elemento vivienda
mostrar_precio(vivienda(X, _), Y) :- precio(X, Y). 

% Proceso principal
run_todo :- 
        run_semanas_hasta_ver_N_pisos,
        viviendas_visitadas(X),
        calcular_vivienda_mas_barata(X, MasBarata),
        mostrar_precio(MasBarata, Precio),
        %write(X), nl,
        write("La mas barata de las primeras 50 es:"), nl,
        write(MasBarata), write(" con precio de "), write(Precio), write(" mil Euros."), nl,
        write("Ahora buscaremos hasta dar con una vivienda mas barata que todas las anteriores"), nl, nl, 
        retract(maximo_precio(_)),
        asserta(maximo_precio(Precio)),
        retract(viviendas_visitadas(X)),
        asserta(viviendas_visitadas([])),
        run_semanas_hasta_ver_mas_barata,
        viviendas_visitadas([Y]),
        mostrar_precio(Y, PrecioViviendaComprada),
        contador_semanas(Semanas),
        write("Enhorabuena, hemos encontrado una vivienda para comprar: "), nl,
        write(Y), nl,
        write("Su precio final es: "), write(PrecioViviendaComprada), write(" mil Euros."), nl,
        write("Nos ha costado "), write(Semanas), write(" semanas."), nl.
