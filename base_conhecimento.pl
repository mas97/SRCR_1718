
% FEITO NA FICHA 4
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
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
:- dynamic filho/2.
:- dynamic pai/2.

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

consultaInstituicoes( S ) :- solucoes( Is, prestador( _, _, _, Is ), S).


% Extensão do predicado cuidado: Data, IDUtente, IDPrestador, Descricao, Custo -> {V,F}
cuidado( 2017/03/17, 1, 1, curativo, 20 ).

consultaCuidados( I, M, D, S ) :- prestador(ID,_,_,I),
                                  solucoes( ( D, IDU, IDP, De, C ), cuidado( D, IDU, IDP, De, C ), S ).

consultaUtente( P, E, I, S ) :- prestador( P, _, E, I ),
                                solucoes( ID, cuidado( _, ID, P, _, _ ), S ).

% Invariante Estrultural:  nao permitir a insercao de conhecimento
%                         repetido

+utente( IDU, N, I, M ) :: (solucoes( ( IDU, N, I, M ),(utente( IDU, N, I, M )),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).

% não permitir a inserção de utente com um ID que já está registado na base de conhecimento
+utente( IDU, _, _, _ ) :: (solucoes( IDUs,(utente( IDUs, _, _, _ )),S ),
                  comprimento( S,N ), 
                  N =< 1
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
