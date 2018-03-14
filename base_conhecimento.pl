% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/5.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IDUtente, Nome, Idade, Morada -> {V,F}
utente( 1, marco, 25, braga).
utente( 2, afonso, 30, braga).
utente( 3, daniel, 20, braga).
utente( 4, francisco, 22, felgueiras).
utente( 5, rafael, 23, porto).
utente( 6, bruno, 21, braga).
utente( 7, hugo, 24, porto).
utente( 8, luis, 35, lisboa).

consultaUtente( ID, N, I, M, S ) :- solucoes( ( ID, N, I, M ), utente( ID, N, I, M ), S ).


% Extensão do predicado prestador: IDPrestador, Nome, Especialidade, Instituição -> {V,F}
prestador( 1, wilson, medico, um).
prestador( 3, marciano, medico, hospitalbraga).
prestador( 4, silvio, enfermeiro, hospitalbraga).
prestador( 5, marcio, medico, centrosaudegualtar).
prestador( 6, armando, tecnicoRaioX, hospitalbraga).

consultaInstituicoes( S ) :- solucoes( Is, prestador( _, _, _, Is ), S).


% Extensão do predicado cuidado: Data, IDUtente, IDPrestador, Descricao, Custo -> {V,F}
cuidado( 2017/03/17, 1, 1, curativo, 20 ).
cuidado( 2018/03/01, 1, 2, consulta, 25 ).
cuidado( 2018/03/02, 7, 4, penso, 5).
cuidado( 2018/03/03, 2, 5, consulta, 18).
cuidado( 2018/03/04, 5, 4, penso, 8).
cuidado( 2018/03/05, 6, 3, consulta, 19).
cuidado( 2018/03/06, 8, 4, penso, 6).
cuidado( 2018/03/07, 5, 1, exame, 100).
cuidado( 2018/03/08, 8, 5, consulta, 20).
cuidado( 2018/03/09, 6, 6, raioX, 75).

consultaCuidados( I, M, D, S ) :- prestador(ID,_,_,I),
                                  solucoes( ( D, IDU, IDP, De, C ), cuidado( D, IDU, IDP, De, C ), S ).

consultaUtente( P, E, I, S ) :- prestador( P, _, E, I ),
                                solucoes( ID, cuidado( _, ID, P, _, _ ), S ).

somaL([],0).
somaL([B|C],R) :- somaL(C,T),
                  R is T + B.

totalCuidados( U, E, P, D, R ) :- solucoes( C, cuidado(D, U, P, _, C), S ),
                                  somaL(S,R).

todasIP( IDU,S ) :- 
	prestador(Ps, _, _, Is),
	solucoes( (Ps, Is), cuidado(_, IDU, Ps, _, _), S).

% Calculo das receitas de uma determinada Instituição (extra enunciado)

receitasInst( Inst, R ) :- prestador(ID, _, _, Inst),
                           solucoes( C, cuidado(_, _, ID, _, C), S),
                           somaL(S, R).

% Invariante Estrultural:  nao permitir a insercao de conhecimento
%                         repetido

% não permitir a inserção de duplicados de utente
%+utente( IDU, No, I, M ) :: (solucoes( (IDU, No, I, M ),(utente( IDU, No, I, M )),S ),
%                 comprimento( S,N ), 
%                  N == 1
%
%                  ).


% não permitir a inserção de utente com um ID que já está registado na base de conhecimento
+utente( IDU, No, I, M ) :: (solucoes( IDU,(utente( IDU, _, _, _ )),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).


% não permitir a inserção de um utente em que a sua idade seja negativa
+utente( _, _, I, _ ) :: I > 0.


%não permitir a remoção de utentes com cuidados registados
-utente(ID, _, _, _) :: (solucoes( ID,(cuidado(_, ID, _, _, _)), S),
                          comprimento(S, N),
                          N == 0
                          ).


% não permitir a inserção de duplicados de prestador
%+prestador( ID, No, E, I) :: (solucoes((ID, No, E, I),(prestador(ID, No, E, I)),S),
%                    comprimento( S,N ),
%                    N == 1
%                    ).

% não permitir a inserção de prestador com um ID que já está registado na base de conhecimento
+prestador( IDU, No, E, I ) :: (solucoes( IDU,(prestador( IDU, _, _, _ )),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).

%não permitir a remoção de prestadores com cuidados registados
-prestador(ID, _, _, _) :: (solucoes( ID,(cuidado(_, _, ID, _, _)), S),
                          comprimento(S, N),
                          N == 0
                          ).

%não permitir a inserção de duplicados de cuidado
+cuidado(D, IDU, IDP, Desc, C) :: (solucoes( (D, IDU, IDP, Desc, C), (cuidado(D, IDU, IDP, Desc, C)), S),
                                  comprimento( S,N),
                                  N == 1
                                  ).

%não permitir a inserção de cuidados se os intervenientes não existirem na base de conhecimento
+cuidado(D, IDU, IDP, Desc, C) :: (solucoes( (IDU, IDP), (utente(IDU, _, _, _), prestador(IDP, _, _, _)), S),
                                  comprimento(S, N),
                                  N == 1
                                  ).

% Invariante Referencial: nao admitir mais do que 2 progenitores
%                         para um mesmo individuo
+filho( F,P ) :: (solucoes( Ps ,filho( F, Ps), S),
				  comprimento(S,N),
				  N =< 2
				  ).


comprimento([], 0).
comprimento([_|L], R) :- comprimento(L,T),
	R is 1 + T.

solucoes(X,Y,Z) :- findall(X,Y,Z).

insere(P) :- assert(P).
insere(P) :- retract(P), !, fail.

teste([]).
teste([R|L]) :- R, teste(L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

registar( Termo ) :- solucoes(Inv, +Termo :: Inv, S),
					 insere(Termo),
					 teste(S).

remove(P) :- retract(P).
remove(P) :- assert(P), !, fail.

remover( Termo ) :- solucoes(Inv, -Termo :: Inv, S),
					  remove(Termo),
					  teste(S).

-filho( F,P ) :: (solucoes( F ,idade( F, _), S),
				  comprimento(S,N),
				  N == 0
				  ).
