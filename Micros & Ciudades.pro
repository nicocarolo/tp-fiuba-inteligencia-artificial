domains
	Ruta = string  /* Ruta 2, 11, 63, 56 */
	Ciudad = string /* Capital Federal, Chascomus, Mar del Plata ... */
	Puertas = string /* Abiertas o Cerradas */
	Accion = string
	NPasajeros = integer
	Movimiento = integer
	Arranca = integer /* 0 = false, 1 = true */
	ListaC = Ciudad*
	ListaP = Npasajeros*
	MViajes = TEstado*
	TEstado = micro (Ruta,Ciudad,NPasajeros,Puertas,Arranca,Movimiento,Accion) 
	
predicates
	/* Indica si una ciudad tiene peaje */
	nondeterm peaje (Ciudad)
	
	/* Indica si dos ciudades estan conectadas por una ruta */
	nondeterm conectados (Ciudad,Ciudad,Ruta)
	
	/* Indica si se puede ir de una ciudad a otra por una ruta */
	nondeterm camino (Ciudad,Ciudad,Ruta)
	
	/*Indica si un estado pertenece a una lista de estados, o si una ciudad pertenece a una lista de ciudades */ 
	nondeterm pertenece (TEstado,MViajes)
	pertenece (Ciudad,ListaC)
	
	/* Frena el micro */
        nondeterm frenar (TEstado,TEstado)
        
        /* Arranca el micro */
        nondeterm arrancar (ListaC,TEstado,TEstado)
        
        /* Abre las puertas del micro */
        nondeterm abrirPuertas (TEstado,TEstado)
        
        /* Cierra las puertas del micro */
        nondeterm cerrarPuertas (TEstado,TEstado)
        
        /* Hace que el micro avance */
        nondeterm avanzar (ListaC,ListaC,TEstado,TEstado)
        
        /* Hace bajar a los pasajeros del micro */
        nondeterm bajan (ListaC,ListaP,TEstado,TEstado)
        
        /* Paga el peaje */
        nondeterm pagar (TEstado,TEstado)
        
        /* Calcula la cantidad de pasajeros que bajan en una parada */
        nondeterm cantBaja (Ciudad,ListaC,ListaP,NPasajeros)
        
        /* Busca la solunción de caminos */
        /* Lista de ciudades visitadas,Estado inicial,Estado final,prof,solución,Estados anteriores,Lista de Paradas,Lista de Pasajeros */
        nondeterm pp (ListaC,TEstado,TEstado,integer,MViajes,MViajes,ListaC,ListaP)
        nondeterm regla (TEstado,TEstado,ListaC,ListaP,ListaC,ListaC)
        
clauses
	
	peaje ("Dolores").
        peaje ("Costa Azul").
        
        conectados ("Capital Federal","Magdalena","Ruta 11").
        conectados ("Capital Federal","Chascomus","Ruta 2").
        conectados ("Magdalena","Pipina","Ruta 11").
        conectados ("Chascomus","Dolores","Ruta 2").
        conectados ("Pipina","Conesa","Ruta 11").
        conectados ("Dolores","Maipu","Ruta 2").
        conectados ("Dolores","Conesa","Ruta 63").
        conectados ("Maipu","Mar del Plata","Ruta 2").
        conectados ("Conesa","Costa Azul","Ruta 11").
        conectados ("Conesa","Pinamar","Ruta 56").
        conectados ("Costa Azul","Pinamar","Ruta 11").
        conectados ("Pinamar","Mar del Plata","Ruta 11").

        camino(X,Y,Z) :- conectados(X,Y,Z) ; conectados(Y,X,Z).
        
        pertenece (X,[X|_]) :- !.
        pertenece (X,[_|CO]) :- pertenece (X,CO).
        
        /* Si tiene motor en marcha, se mueve, tiene puertas cerradas entonces frena
          Frena el micro: Deja en 0 al movimiento y el arranque */
        frenar(micro(R,C,P,Puertas,A,Mov,Ac),micro(R,C,P,Puertas,A2,Mov2,Ac2)):- 
        	A > 0, Mov > 0, Puertas = "Cerradas", Ac = "Avanza", Mov2 = 0, A2 = 0,
        	Ac2 = "Frena".
        
        /* Si Frena y tiene que pagar peaje entonces lo paga */
        pagar(micro(R,C,P,Puertas,A,Mov,Ac),micro(R,C,P,Puertas,A,Mov,Ac2)):- 
        	A = 0, Mov = 0, Puertas = "Cerradas", Ac = "Frena", peaje(C),
        	Ac2 = "Paga Peaje".
        
        /* Si tiene puertas cerradas, arrancó, existe un camino y la siguiente ciudad no esta repetida entonces avanza a la siguiente ciudad */
        avanzar(CV,[C2|CV],micro(_,C,P,Puertas,A,Mov,Ac),micro(R,C2,P,Puertas,A,Mov,Ac2)) :-
        	Puertas = "Cerradas", A > 0, Ac = "Arranca", camino(C,C2,R), not(pertenece(C2,CV)),
        	Ac2 = "Avanza".
        
        /* Si tiene puertas cerradas, arrancó, no paga peaje y existe un camino entoces avanza a la siguiente ciudad */                                                                   
        avanzar(CV,[C2|CV],micro(_,C,P,Puertas,A,Mov,Ac),micro(R,C2,P,Puertas,A,Mov,Ac2)) :-
        	Puertas = "Cerradas", A > 0, Ac = "Avanza", not(peaje(C)), camino(C,C2,R), not(pertenece(C2,CV)),
        	Ac2 = "Avanza".
        
        /* Si esta en reposo y con puertas cerradas entonces arranca */                                                                         
        arrancar(_,micro(R,C,P,Puertas,A,Mov,Ac),micro(R,C,P,Puertas,A2,Mov2,Ac2)) :- 
        	A = 0, Mov = 0, Puertas = "Cerradas",
        	Ac = "Cierra Puertas", A2 = 1, Mov2 = 1, Ac2 = "Arranca".
        
        /* Si esta en reposo, con puertas cerradas y pagó peaje entonces arranca */  
        arrancar(LC,micro(R,C,P,Puertas,A,Mov,Ac),micro(R,C,P,Puertas,A2,Mov2,Ac2)) :-
        	A = 0 ,Mov = 0, Puertas = "Cerradas", Ac = "Paga Peaje", not(pertenece(C,LC)),
        	A2 = 1, Mov2 = 1, Ac2 = "Arranca".
        
        /* Si Frenó y no paga peaje entonces abre las puertas */
        abrirPuertas(micro(R,C,P,Puertas,A,Mov,Ac),micro(R,C,P,Puertas2,A,Mov,Ac2)) :- 
        	A = 0,Mov = 0,
        	Puertas = "Cerradas",
        	Ac = "Frena",
        	not(peaje(C)),
        	Puertas2 = "Abiertas",
        	Ac2 = "Abre Puertas".
        
        /* Si frenó, paga peaje y existe parada entonces abre las puertas */
        abrirPuertas(micro(R,C,P,Puertas,A,Mov,Ac),micro(R,C,P,Puertas2,A,Mov,Ac2)) :- 
        	A = 0,Mov = 0,
        	Puertas = "Cerradas",
        	Ac = "Paga Peaje",
        	Puertas2 = "Abiertas",
        	Ac2 = "Abre Puertas".
        
        /* Si frenó, tiene las puertas abiertas y bajaron los pasajeros entonces cierra las puertas */
        cerrarPuertas(micro(R,C,P,Puertas,A,Mov,Ac),micro(R,C,P,Puertas2,A,Mov,Ac2)) :- 
        	A = 0,Mov = 0,
        	Puertas = "Abiertas",
        	Ac = "Bajan Pasajeros",
        	Puertas2 = "Cerradas",
        	Ac2 = "Cierra Puertas".
        
        /* Si frenó y abre las puertas entonces bajan los pasajeros */
        bajan(LC,LP,micro(R,C1,P1,Puertas,A,Mov,Ac),micro(R,C1,P2,Puertas,A,Mov,Ac2)) :-
        	A = 0, Mov = 0, Puertas = "Abiertas",
        	Ac = "Abre Puertas",
        	P1>0,cantBaja(C1,LC,LP,P),
        	P2 = P1 - P, P2 >= 0,
        	Ac2 = "Bajan Pasajeros".
        
        cantBaja(X,[X|_],[Y|_],Y) :- !. 
        cantBaja(X,[_|T1],[_|T2],Y) :- cantBaja(X,T1,T2,Y).
        
        pp(_,micro(R,C,P,Pu,A,M,Ac),micro(_,C,P,Pu,A,M,_),_,[Ef],_,_,_) :-
        	Ef=micro(R,C,P,Pu,A,M,Ac),
        	!.
        	
        pp(CVis,EI,EF,N,[EI|R],AntEst,LC,LP) :- 
        	N>0,
        	regla(EI,E2,LC,LP,CVis,CV),
        	not(pertenece(E2,AntEst)),
        	N1=N-1,
        	pp(CV,E2,EF,N1,R,[E2|AntEst],LC,LP).
        
        regla(Ei,Ef,_,_,CV,CV) :- frenar(Ei,Ef).
        regla(Ei,Ef,LC,_,CV,CV) :- arrancar(LC,Ei,Ef).
        regla(Ei,Ef,_,_,CV,CV) :- pagar(Ei,Ef).
        regla(Ei,Ef,_,_,CV,CV) :- abrirPuertas(Ei,Ef).
        regla(Ei,Ef,_,_,CV,CV) :- cerrarPuertas(Ei,Ef).
        regla(Ei,Ef,_,_,CVis,CV) :- avanzar(CVis,CV,Ei,Ef).
        regla(Ei,Ef,LC,LP,CV,CV) :- bajan(LC,LP,Ei,Ef).
	
goal

	Pasajeros = 10,
        /*Lista de Ciudades Visitadas, Estado inicial, Estado final, Profundidad de busqueda maxima, Solucion, Lista de Estados anteriores, Lista de Paradas, Lista de Pasajeros que bajan en cada parada */
	pp(
	["Conesa"],
	micro("Inicio","Conesa",Pasajeros,"Cerradas",0,0,"Cierra Puertas"),
	micro("0","Pinamar",10,"Cerradas",0,0,"0"),
	200,
	SOL,
	[micro("Inicio","Conesa",Pasajeros,"Cerradas",0,0,"Cierra Puertas")],
	[],
	[]
	).