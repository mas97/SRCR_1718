% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

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
:- dynamic instituicao/3.
:- dynamic excecao/1.
:- dynamic (-)/1.
:- dynamic nulo/1.
:- dynamic (::)/1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Bibliotecas
:- use_module(library(lists)).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IDUtente, Nome, Idade, Morada -> {V,F}
% Conhecimento positivo
utente( 1, marco, 25, braga).
utente( 2, afonso, 30, braga).
utente( 3, daniel, 20, braga).
utente( 4, francisco, 22, felgueiras).
utente( 5, rafael, 23, porto).
utente( 6, bruno, 21, braga).
utente( 7, hugo, 24, porto).
utente( 8, luis, 35, lisboa).

% Conhecimento negativo
-utente( 1, carlos, 12, guimaraes).
-utente( 2, beatriz, 18, porto).
-utente( 4, miguel, 34, viana).
-utente( 7, manuel, 23, braga).
-utente( 9, antonio, 28, braganca).
-utente( 12, leonardo, 14, paredes).
-utente( 15, pedro, 57, braga).

% Receita
-utente( IDU, N, I, M ) :-
        nao( utente( IDU, N, I, M ) ),
        nao( excecao( utente( IDU, N, I, M ) ) ).

% Conhecimento incerto
utente( 9, ana, 22, uncert1).
excecao( utente(IDU, No, I, M)) :- utente(IDU, No, I, uncert1).

utente( 10, beatriz, 43, uncert2).
excecao( utente(IDU, No, I, M)) :- utente(IDU, No, I, uncert2).

% Conhecimento impreciso
excecao( utente(11, bernardo, 15, felgueiras)).
excecao( utente(11, bernardo, 17, felgueiras)).
excecao( utente(12, crispim, I, porto)) :- I >= 32, I =< 36. 
excecao( utente(14, inacio, I, M)) :- I >= 17, I =< 20.

% Conhecimento interdito
utente( 13, beltrano, nullval1, guimaraes).
nulo( nullval1 ).
excecao(utente(IDU, No, I, M)) :- utente(IDU, No, nullval1, M).

%nao permitir a insercao de utentes EXPLICARRRRRRRRRRRRRRRRRRRRRR.
+utente(IDU, No, I, M) :: (solucoes( Is, (utente(13, beltrano, Is, guimaraes), nao( nulo( Is ) ) ), S),
                         comprimento(S, N),
                         N == 0
                         ).

%nao permitir a insercao de utentes EXPLICARRRRRRRRRRRRRRRRRRRRRR.
-utente(IDU, No, I, M) :: (solucoes( Is, (utente(13, beltrano, Is, guimaraes), nao( nulo( Is ) ) ), S),
                         comprimento(S, N),
                         N == 1
                         ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado prestador: IDPrestador, Nome, Especialidade, IDInst -> {V,F}

% Conhecimento positivo
prestador( 1, wilson, medico, 2).
prestador( 2, eduardo, cirurgiao, 1).
prestador( 3, marciano, medico, 1).
prestador( 4, silvio, enfermeiro, 4).
prestador( 5, marcio, medico, 5).
prestador( 6, armando, tecnicoRaioX, 3).
prestador( 7, paulo, tecnicoRaioX, 6).

%Conhecimento negativo
-prestador( 2, marco, medico, 1).
-prestador( 4, filipe, cirurgiao, 1).
-prestador( 5, daniel, medico, 5).
-prestador( 7, eduardo, enfermeiro, 2).
-prestador( 9, afonso, medico, 4).

% Receita
-prestador( ID, No, E, I) :-
	nao( prestador( ID, No, E, I)),
	nao( excecao( prestador( ID, No, E, I))).

% Conhecimento incerto
prestador( 8, pedro, uncert3, 5).
excecao( prestador(IDP, No, E, IDI)) :- prestador(IDP, No, uncert3, IDI).

prestador( 9, carolina, uncert4, 3).
excecao( prestador(IDP, No, E, IDI)) :- prestador(IDP, No, uncert4, IDI).


% Conhecimento impreciso
excecao( prestador(10, No, enfermeiro, 2)) :- No == rafael.
excecao( prestador(10, No, enfermeiro, 2)) :- No == francisco.

excecao( prestador(11, No, medico, 4)) :- No == silvio.
excecao( prestador(11, No, medico, 4)) :- No == dionisio.
excecao( prestador(11, No, medico, 4)) :- No == wilson.

excecao( prestador(12, icaro, E, 3)) :- E == cirurgiao.
excecao( prestador(12, icaro, E, 3)) :- E == medico.

excecao( prestador(13, garcia, E, 1)) :- E == tecnicoRaioX.
excecao( prestador(13, garcia, E, 1)) :- E == enfermeiro.
excecao( prestador(13, garcia, E, 1)) :- E == oftalmologista.

excecao( prestador(14, No, E, 3)) :- No == gabriel, E == medico.
excecao( prestador(14, No, E, 3)) :- No == gabriel, E == cirurgiao.
excecao( prestador(14, No, E, 3)) :- No == maria, E == medico.
excecao( prestador(14, No, E, 3)) :- No == maria, E == cirurgiao.



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidado: Data, IDUtente, IDPrestador, Descricao, Custo -> {V,F}

% Conhecimento positivo
cuidado( 2017/03/17, 1, 1, curativo, 20 ).
cuidado( 2018/03/01, 1, 2, consulta, 25 ).
cuidado( 2018/03/02, 7, 4, penso, 5).
cuidado( 2018/03/03, 2, 5, consulta, 18).
cuidado( 2018/03/04, 5, 4, penso, 8).
cuidado( 2018/03/05, 6, 3, consulta, 19).
cuidado( 2018/03/07, 5, 1, exame, 100).
cuidado( 2018/03/09, 6, 6, raioX, 75).
cuidado( 2018/03/06, 8, 4, penso, 6).
cuidado( 2018/03/08, 8, 5, consulta, 20).
cuidado( 2018/07/25, 8, 2, exame, 40).
cuidado( 2018/10/22, 8, 1, consulta, 20).

% Conhecimento negativo
-cuidado( 2018/03/02, 1, 1, consulta, 90).
-cuidado( 2018/03/06, 3, 2, exame, 85).
-cuidado( 2018/03/10, 5, 2, penso, 80).
-cuidado( 2018/03/12, 6, 3, consulta, 75).
-cuidado( 2018/03/13, 8, 5, raioX, 70).
-cuidado( 2018/03/16, 10, 5, consulta, 75).

% Receita
-cuidado( D, IDU, IDP, De, C ) :-
        nao( cuidado( D, IDU, IDP, De, C ) ),
        nao( excecao( cuidado( D, IDU, IDP, De, C ) ) ).


% Conhecimento incerto
cuidado( 2018/01/20, 7, 4, uncert5, 25).
excecao( cuidado(D, IDU, IDP, De, C)) :- cuidado(D, IDU, IDP, uncert5, C).

cuidado( 2018/04/10, 2, 3, consulta, uncert6).
excecao( cuidado(D, IDU, IDP, De, C)) :- cuidado(D, IDU, IDP, De, uncert6).

cuidado( uncert7, 5, 2, penso, 5).
excecao( cuidado(D, IDU, IDP, De, C)) :- cuidado(uncert7, IDU, IDP, De, C).


% Conhecimento impreciso
excecao( cuidado( 2018/02/02, 2, 6, exame, C)) :- C >= 20, C =< 30.
excecao( cuidado( 2018/04/08, 5, 7, raioX, C)) :- C >= 60, C =< 80.
excecao( cuidado( 2018/03/D, IDU, IDP, De, C)) :- D >= 6, D =< 9.
excecao( cuidado( 2018/D/05, IDU, IDP, De, C)) :- D >= 1, D =< 2.
excecao(cuidado(D, 5, 3, exame, C)) :- D == 2018/04/02, C >= 25, C =< 35.

% Conhecimento interdito
cuidado( 2018/05/05, nullval2, 3, curativo, 30).
nulo( nullval2 ).
excecao(cuidado(D, IDU, IDP, De, C)) :- cuidado(D, nullval2, IDP, De, C).

%nao permitir a insercao de cuidados com utentes restritos.
+cuidado(D, IDU, IDP, De, C) :: (solucoes( IDUs, (cuidado(2018/05/05, IDUs, 3, curativo, 30), nao( nulo( IDUs ) ) ), S),
                        comprimento(S, N),
                        N == 0
                        ).

%nao permitir a insercao de utentes EXPLICARRRRRRRRRRRRRRRRRRRRRR.
-cuidado(D, IDU, IDP, De, C) :: (solucoes( IDUs, (cuidado(2018/05/05, IDUs, 3, curativo, 30), nao( nulo( IDUs ) ) ), S),
                        comprimento(S, N),
                        N == 1
                        ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado instituicao: IDInst, Nome, Cidade -> {V,F}

% Conhecimento positivo
instituicao( 1, hospitalbraga, braga).
instituicao( 2, hospitalsaojoao, porto).
instituicao( 3, hospitalsantamaria, porto).
instituicao( 4, hospitaltrofa, porto).
instituicao( 5, centrosaudegualtar, braga).
instituicao( 6, hospitalaveiro, aveiro).

% Conhecimento negativo
-instituicao(1, hospitalbraga, guimaraes).
-instituicao(1, hospitalbarco, viladoconde).
-instituicao(1, hospitalsaojoao, porto).
-instituicao(2, hospitalmonte, vilaverde).
-instituicao(4, hospitalazurem, guimaraes).
-instituicao(4, hospitalporto, porto).

% Receita
-instituicao(Id, N, C) :-
        nao( instituicao(Id, N, C) ),
        nao( excecao( instituicao(Id, N, C) ) ).


% Conhecimento incerto
instituicao( 7, uncert8, guimaraes).
excecao( instituicao(IDI, No, Ci)) :- instituicao(IDI, uncert8, Ci).

% Conhecimento impreciso
excecao( instituicao(8, centrosaudeguimaraes, Ci)) :- Ci == amares.
excecao( instituicao(8, centrosaudeguimaraes, Ci)) :- Ci == penafiel.
excecao( instituicao(8, centrosaudeguimaraes, Ci)) :- Ci == guimaraes.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Quantos prestadores tem uma instituicao
quantosPrest(I, N) :- solucoes((ID, No, E, I), prestador(ID, No, E, I), S),
                   comprimento(S, N).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identifica utentes por criterios
consultaUtente( ID, N, I, M, S ) :- solucoes( ( ID, N, I, M ), utente( ID, N, I, M ), S ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identifica utentes pela instituicao prestadora de cuidados
consultaUtente( P, E, I, R ) :- solucoes( (ID, N, Id, M), ( cuidado( _, ID, P, _, _ ) , prestador( P, _, E, I ), utente(ID, N, Id, M) ), S ), sort(S, R).
                                
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar as instituições prestadoras de cuidados de saúde por criterios desde que estas tenham cuidados registados
consultaInstituicoes( Id, N, C, R ) :- solucoes( (Id, N, C), ( instituicao(Id, N, C), cuidado(_, _, IdP, _, _), prestador(IdP, _, _, Id) ), S), sort(S, R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Determinar todas as instituições/prestadores a que um utente já recorreu
todasInstPrest( IDU,S ) :- 
  solucoes( (Ps, Id), (prestador(Ps, _, _, Id),cuidado(_, IDU, Ps, _, _)), S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Calculo das receitas de uma determinada Instituicao (extra enunciado)
receitasInst( Inst, R ) :- solucoes( C, (cuidado(_, _, ID, _, C),prestador(ID, _, _, Inst)) , S),
                           somaL(S, R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identifica cuidados por criterios
consultaCuidados( IDI, IDU, IDP, Ci, D, S ) :- 
  solucoes( ( D, IDU, IDP, De, C ), (instituicao(IDI, _, Ci), prestador(IDP,_,_,IDI),cuidado( D, IDU, IDP, De, C )), S ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Calcula o valor total dos custos de um determinado utente
somaL([],0).
somaL([B|C],R) :- somaL(C,T),
                  R is T + B.

totalCuidados( U, E, P, D, R ) :- solucoes( C, (cuidado(D, U, P, _, C), prestador(_,_,E,_)), S ),
                                  somaL(S,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identifica todos os cuidados prestados por uma determinada instituição
cuidadosInst( IDInst , S ) :- solucoes( (IDInst, Desc) , (prestador( IDPrest , _ , _, IDInst ), cuidado(_,_,IDPrest, Desc, _)), S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Cuidado de saúde mais frequente para um determinado utente

conta(X,[],[],0).
conta(X, [X|L], Lr, R) :-
	conta(X,L,Lr,S),
	R is S + 1.
conta(X, [H|L], [H|Lr], S) :-
	conta(X,L,Lr,S),
	X \= H.

pairFreq([],[]).
pairFreq([H|T],[(R3,H)|R2]) :- 
	conta(H, T, Lr, R1),
	R3 is R1 + 1,
	pairFreq(Lr, R2).

cuidadosUtente(IDU, Rff) :- 
	solucoes( D, cuidado(_,IDU,_,D,_), S ),
	pairFreq(S, R),
	sort(R,Rf),
	reverse(Rf,Rff).

%--------------------------------- - - - - - - - - - -  -  -  -  -  - 
% Relatório de contas de uma instituição dado o mes e o ano

calcTotal([], 0).
calcTotal([(_, _, _, _, Cus)|L], T) :-
	calcTotal(L,Tr),
	T is Cus + Tr.

addTotal(T, [], [(total -> T)]).
addTotal(T, [H|L], [H|Lr]) :-
	addTotal(T, L, Lr).


relatContas( A/M,IDI, Rf) :- 
	solucoes( ( A/M/D, IDU, Desc, Esp, Cus), (prestador(IDP,_,Esp,IDI),cuidado( A/M/D, IDU, IDP, Desc, Cus )), S ),
	calcTotal(S, T),
	addTotal(T, S, Rf).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% INVARIANTES -------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% nao permitir a insercao de utente com um ID que ja esta registado na base de conhecimento
+utente( IDU, No, I, M ) :: (solucoes( IDU,(utente( IDU, _, _, _ )),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%nao permitir a remocao de utentes com cuidados registados
-utente(ID, _, _, _) :: (solucoes( ID,(cuidado(_, ID, _, _, _)), S),
                          comprimento(S, N),
                          N == 0
                          ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% nao permitir a insercao de prestador com um ID que ja esta registado na base de conhecimento
+prestador( IDU, _, _, _ ) :: (solucoes( IDU,(prestador( IDU, _, _, _ )),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%nao permitir a remocao de prestadores com cuidados registados
-prestador(ID, _, _, _) :: (solucoes( ID,(cuidado(_, _, ID, _, _)), S),
                          comprimento(S, N),
                          N == 0
                          ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%nao permitir a insercao de duplicados de cuidado
+cuidado(D, IDU, IDP, Desc, C) :: (solucoes( (D, IDU, IDP, Desc, C), (cuidado(D, IDU, IDP, Desc, C)), S),
                                  comprimento( S,N),
                                  N == 1
                                  ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%nao permitir a insercao de cuidados se os intervenientes nao existirem na base de conhecimento
+cuidado(_, IDU, IDP, _, _) :: (solucoes( (IDU, IDP), (utente(IDU, _, _, _), prestador(IDP, _, _, _)), S),
                                  comprimento(S, N),
                                  N == 1
                                  ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%nao permitir a insercao de duplicados de instituicao
+instituicao(IDInst, _, _) :: (solucoes( IDInst ,instituicao(IDInst, _, _), S),
                                  comprimento( S,N),
                                  N == 1
                                  ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%nao permitir a remocao de instituições com prestadores dessa mesma instituição
-instituicao(ID, _, _) :: (solucoes( ID,(prestador(_, _, _, ID)), S),
                          comprimento(S, N),
                          N == 0
                          ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Calcula o comprimento de uma lista.
comprimento([], 0).
comprimento([_|L], R) :- comprimento(L,T),
	R is 1 + T.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
solucoes(X,Y,Z) :- findall(X,Y,Z).


teste([]).
teste([R|L]) :- R, teste(L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F,D}

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Regista um termo na base de conhecimento
insere(P) :- assert(P).
insere(P) :- retract(P), !, fail.


% EVOLUÇÃO

% Conhecimento perfeito
registar( Termo ) :- 
	solucoes(Inv, +Termo :: Inv, S),
	insere(Termo),
	teste(S).

% Conhecimento imperfeito utente - impreciso (Idade)
registar( utente(IDU, No, I, M), Tipo ) :- 
	Tipo == impreciso,
	solucoes(Inv, +excecao(utente(IDU, No, I, M)) :: Inv, S),
	insere(excecao(utente(IDU, No, I, M))),
	teste(S).

% Conhecimento imperfeito utente - impreciso (Idade)
registar( utente(IDU, No, I, M), Menor, Maior ) :-
	solucoes(Inv, +excecao(utente(IDU, No, I, M)) :: Inv, S),
	insere((excecao(utente(IDU, No, Idade, M)) :- Idade >= Menor, Idade =< Maior)),
	teste(S).


% Conhecimento imperfeito utente - interdito (idade)
registar( utente(IDU, No, I, M), Tipo ) :-
	Tipo == interdito,
	solucoes(Inv, +utente(IDU, No, I, M) :: Inv, S),
	insere(utente(IDU, No, I, M)),
	teste(S),
	insere(nulo(I)),
	insere((excecao(utente(IDUtente, Nome, Idade, Morada)) :- utente(IDUtente, Nome, I, Morada))).


% Conhecimento imperfeito utente - incerto (Morada)
registar( utente(IDU, No, I, M), Tipo ) :-
	Tipo == incerto,
	solucoes(Inv, +utente(IDU, No, I, M) :: Inv, S),
	insere(utente(IDU, No, I, M)),
	teste(S),
	assert((excecao(utente(IDUtente, Nome, Idade, Morada)) :- utente(IDUtente, Nome, Idade, M))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -


% Conhecimento imperfeito prestador - impreciso (Nome e Especialidade)
registar( prestador(IDP, No, E, IDI), Tipo ) :- 
	Tipo == impreciso,
	solucoes(Inv, +excecao(prestador(IDP, No, E, IDI)) :: Inv, S),
	insere(excecao(prestador(IDP, No, E, IDI))),
	teste(S).



% Conhecimento imperfeito prestador - incerto (Especialidade)
registar( prestador(IDP, No, E, IDI), Tipo ) :-
	Tipo == incerto,
	solucoes(Inv, +prestador(IDP, No, E, IDI) :: Inv, S),
	insere(prestador(IDP, No, E, IDI)),
	teste(S),
	insere((excecao(prestador(IDPrestador, Nome, Especialidade, IDInstituicao)) :- prestador(IDPrestador, Nome, E, IDInstituicao))).

	
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% CUIDADO E INSTITUICAO NÃO ESTAO TESTADOS!!!!!!!!!!!!!!!!!!!!

% Conhecimento imperfeito cuidado - impreciso (Custo)
registar( cuidado(D, IDU, IDP, De, C), Menor, Maior, Parametro ) :-
	Parametro == custo,
	solucoes(Inv, +excecao(cuidado(D, IDU, IDP, De, C)) :: Inv, S),
	insere((excecao(cuidado(D, IDU, IDP, De, Custo)) :- Custo >= Menor, Custo =< Maior)),
	teste(S).


% Conhecimento imperfeito cuidado - interdito (IDUtente)
registar( cuidado(D, IDU, IDP, De, C), Tipo ) :-
	Tipo == interdito,
	solucoes(Inv, +cuidado(D, IDU, IDP, De, C) :: Inv, S),
	insere(cuidado(D, IDU, IDP, De, C)),
	teste(S),
	insere(nulo(IDU)),
	insere((excecao(prestador(Data, IDUtente, IDPrestador, Descricao, Custo)) :- cuidado(Data, IDU, IDPrestador, Descricao, Custo))).


% Conhecimento imperfeito cuidado - incerto (Descrição)
registar( cuidado(D, IDU, IDP, De, C), Tipo, Parametro ) :-
	Tipo == incerto,
	Parametro == descricao,
	solucoes(Inv, +cuidado(D, IDU, IDP, De, C) :: Inv, S),
	insere(cuidado(D, IDU, IDP, De, C)),
	teste(S),
	assert((excecao(cuidado(Data, IDUtente, IDPrestador, Descricao, Custo)) :- cuidado(Data, IDUtente, IDPrestador, De, Custo))).

% Conhecimento imperfeito cuidado - incerto (Custo)
registar( cuidado(D, IDU, IDP, De, C), Tipo, Parametro ) :-
	Tipo == incerto,
	Parametro == custo,
	solucoes(Inv, +cuidado(D, IDU, IDP, De, C) :: Inv, S),
	insere(cuidado(D, IDU, IDP, De, C)),
	teste(S),
	assert((excecao(cuidado(Data, IDUtente, IDPrestador, Descricao, Custo)) :- cuidado(Data, IDUtente, IDPrestador, Descricao, C))).

% Conhecimento imperfeito cuidado - incerto (Data)
registar( cuidado(D, IDU, IDP, De, C), Tipo, Parametro ) :-
	Tipo == incerto,
	Parametro == data,
	solucoes(Inv, +cuidado(D, IDU, IDP, De, C) :: Inv, S),
	insere(cuidado(D, IDU, IDP, De, C)),
	teste(S),
	assert((excecao(cuidado(Data, IDUtente, IDPrestador, Descricao, Custo)) :- cuidado(D, IDUtente, IDPrestador, Descricao, Custo))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -


% Conhecimento imperfeito instituicao - impreciso (Cidade)
registar( instituicao(ID, N, C), Tipo ) :- 
	Tipo == impreciso,
	solucoes(Inv, +excecao(instituicao(ID, N, C)) :: Inv, S),
	insere(excecao(instituicao(ID, N, C))),
	teste(S).

% Conhecimento imperfeito prestador - incerto (Nome)
registar( instituicao(ID, N, C), Tipo ) :- 
	Tipo == incerto,
	solucoes(Inv, +instituicao(ID, N, C) :: Inv, S),
	insere(instituicao(ID, N, C)),
	teste(S),
	insere((excecao(instituicao(Identificador, Nome, Cidade)) :- instituicao(Identificador, N, Cidade))).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Remove um termo da base de conhecimento
remove(P) :- retract(P).
remove(P) :- assert(P), !, fail.

%INVOLUÇÃO
remover( Termo ) :- solucoes(Inv, -Termo :: Inv, S),
					  remove(Termo),
					  teste(S).

% Conhecimento imperfeito utente - impreciso (Idade)
remover( utente(IDU, No, I, M), Tipo ) :- 
	Tipo == impreciso,
	solucoes(Inv, -excecao(utente(IDU, No, I, M)) :: Inv, S),
	remove(excecao(utente(IDU, No, I, M))),
	teste(S).

% Conhecimento imperfeito utente - impreciso (Idade)
remover( utente(IDU, No, I, M), Menor, Maior ) :-
	solucoes(Inv, -excecao(utente(IDU, No, I, M)) :: Inv, S),
	remove((excecao(utente(IDU, No, Idade, M)) :- Idade >= Menor, Idade =< Maior)),
	teste(S).


% Conhecimento imperfeito utente - interdito (idade)
remover( utente(IDU, No, I, M), Tipo ) :-
	Tipo == interdito,
	solucoes(Inv, -utente(IDU, No, I, M) :: Inv, S),
	remove(utente(IDU, No, I, M)),
	teste(S),
	retract(nulo(I)),
	retract((excecao(utente(IDUtente, Nome, Idade, Morada)) :- utente(IDUtente, Nome, I, Morada))).


% Conhecimento imperfeito utente - incerto (Morada)
remover( utente(IDU, No, I, M), Tipo ) :-
	Tipo == incerto,
	solucoes(Inv, -utente(IDU, No, I, M) :: Inv, S),
	remove(utente(IDU, No, I, M)),
	teste(S),
	retract((excecao(utente(IDUtente, Nome, Idade, Morada)) :- utente(IDUtente, Nome, Idade, M))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -


% Conhecimento imperfeito prestador - impreciso (Nome e Especialidade)
remover( prestador(IDP, No, E, IDI), Tipo ) :- 
	Tipo == impreciso,
	solucoes(Inv, -excecao(prestador(IDP, No, E, IDI)) :: Inv, S),
	remove(excecao(prestador(IDP, No, E, IDI))),
	teste(S).



% Conhecimento imperfeito prestador - incerto (Especialidade)
remover( prestador(IDP, No, E, IDI), Tipo ) :-
	Tipo == incerto,
	solucoes(Inv, -prestador(IDP, No, E, IDI) :: Inv, S),
	remove(prestador(IDP, No, E, IDI)),
	teste(S),
	retract((excecao(prestador(IDPrestador, Nome, Especialidade, IDInstituicao)) :- prestador(IDPrestador, Nome, E, IDInstituicao))).

	
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Conhecimento imperfeito cuidado - impreciso (Custo)
remover( cuidado(D, IDU, IDP, De, C), Menor, Maior, Parametro ) :-
	Parametro == custo,
	solucoes(Inv, -excecao(cuidado(D, IDU, IDP, De, C)) :: Inv, S),
	remove((excecao(cuidado(D, IDU, IDP, De, Custo)) :- Custo >= Menor, Custo =< Maior)),
	teste(S).


% Conhecimento imperfeito cuidado - interdito (IDUtente)
remover( cuidado(D, IDU, IDP, De, C), Tipo ) :-
	Tipo == interdito,
	solucoes(Inv, +cuidado(D, IDU, IDP, De, C) :: Inv, S),
	remove(cuidado(D, IDU, IDP, De, C)),
	teste(S),
	retract(nulo(IDU)),
	retract((excecao(prestador(Data, IDUtente, IDPrestador, Descricao, Custo)) :- cuidado(Data, IDU, IDPrestador, Descricao, Custo))).


% Conhecimento imperfeito cuidado - incerto (Descrição)
remover( cuidado(D, IDU, IDP, De, C), Tipo, Parametro ) :-
	Tipo == incerto,
	Parametro == descricao,
	solucoes(Inv, +cuidado(D, IDU, IDP, De, C) :: Inv, S),
	remove(cuidado(D, IDU, IDP, De, C)),
	teste(S),
	retract((excecao(cuidado(Data, IDUtente, IDPrestador, Descricao, Custo)) :- cuidado(Data, IDUtente, IDPrestador, De, Custo))).

% Conhecimento imperfeito cuidado - incerto (Custo)
remover( cuidado(D, IDU, IDP, De, C), Tipo, Parametro ) :-
	Tipo == incerto,
	Parametro == custo,
	solucoes(Inv, +cuidado(D, IDU, IDP, De, C) :: Inv, S),
	remove(cuidado(D, IDU, IDP, De, C)),
	teste(S),
	retract((excecao(cuidado(Data, IDUtente, IDPrestador, Descricao, Custo)) :- cuidado(Data, IDUtente, IDPrestador, Descricao, C))).

% Conhecimento imperfeito cuidado - incerto (Data)
remover( cuidado(D, IDU, IDP, De, C), Tipo, Parametro ) :-
	Tipo == incerto,
	Parametro == data,
	solucoes(Inv, +cuidado(D, IDU, IDP, De, C) :: Inv, S),
	remove(cuidado(D, IDU, IDP, De, C)),
	teste(S),
	retract((excecao(cuidado(Data, IDUtente, IDPrestador, Descricao, Custo)) :- cuidado(D, IDUtente, IDPrestador, Descricao, Custo))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -


% Conhecimento imperfeito instituicao - impreciso (Cidade)
remover( instituicao(ID, N, C), Tipo ) :- 
	Tipo == impreciso,
	solucoes(Inv, -excecao(instituicao(ID, N, C)) :: Inv, S),
	remove(excecao(instituicao(ID, N, C))),
	teste(S).

% Conhecimento imperfeito prestador - incerto (Nome)
remover( instituicao(ID, N, C), Tipo ) :- 
	Tipo == incerto,
	solucoes(Inv, -instituicao(ID, N, C) :: Inv, S),
	remove(instituicao(ID, N, C)),
	teste(S),
	retract((excecao(instituicao(Identificador, Nome, Cidade)) :- instituicao(Identificador, N, Cidade))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

conjuncao( A, B, verdadeiro ) :-
	A == verdadeiro, B == verdadeiro.
conjuncao( A, B, falso ) :-
	A == falso, B == falso.
conjuncao( A, B, falso ) :-
	A == verdadeiro, B == falso.
conjuncao( A, B, falso ) :-
	A == falso, B == verdadeiro.
conjuncao( A, B, falso ) :-
	A == falso, B == desconhecido.
conjuncao( A, B, falso ) :-
	A == desconhecido, B == falso.
conjuncao( A, B, desconhecido ) :-
	A == desconhecido, B == desconhecido.
conjuncao( A, B, desconhecido ) :-
	A == desconhecido, B == verdadeiro.
conjuncao( A, B, desconhecido ) :-
	A == verdadeiro, B == desconhecido.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

disjuncao( A, B, verdadeiro ) :-
	A == verdadeiro, B == verdadeiro.
disjuncao( A, B, falso ) :-
	A == falso, B == falso.
disjuncao( A, B, verdadeiro ) :-
	A == verdadeiro, B == falso.
disjuncao( A, B, verdadeiro ) :-
	A == falso, B == verdadeiro.
disjuncao( A, B, desconhecido ) :-
	A == falso, B == desconhecido.
disjuncao( A, B, desconhecido ) :-
	A == desconhecido, B == falso.
disjuncao( A, B, desconhecido ) :-
	A == desconhecido, B == desconhecido.
disjuncao( A, B, verdadeiro ) :-
	A == desconhecido, B == verdadeiro.
disjuncao( A, B, verdadeiro ) :-
	A == verdadeiro, B == desconhecido.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% DEMOS

demoComp( [ Q ], R ) :- 
	demo( Q , RQ ),
	conjuncao( RQ, verdadeiro, R ).
demoComp( [ Q ], R ) :- 
	demo( Q, RQ ),
	disjuncao( RQ, falso, R ).
demoComp( [(Q , e) | LQ], R ) :-
	demo( Q, RQ ),
	demoComp( LQ, RL ),
	conjuncao( RQ, RL, R).
demoComp( [(Q , ou) | LQ], R ) :-
	demo( Q, RQ ),
	demoComp( LQ, RL ),
	disjuncao( RQ, RL, R).

demoListaC( [], verdadeiro ).
demoListaC( [ Q ], R ) :- demo( Q, R ).
demoListaC( [ Q | LQ ], R ) :-
	demo( Q, RQ ),
	demoListaC( LQ, RL ),
	conjuncao( RQ, RL, R ).

demoListaD( [], verdadeiro ).
demoListaD( [ Q ], R ) :- demo( Q, R ).
demoListaD( [ Q | LQ ], R ) :-
	demo( Q, RQ ),
	demoListaD( LQ, RL ),
	disjuncao( RQ, RL, R ).


