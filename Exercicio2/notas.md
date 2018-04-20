# Regras

A elaboração do caso prático deverá ser de molde a respeitar as necessidades de demonstração das seguintes funcionalidades: 

- Representar conhecimento positivo e negativo; 

- Representar casos de conhecimento imperfeito, pela utilização de valores nulos de todos os tipos estudados; 

- Manipular invariantes que designem restrições à inserção e à remoção de conhecimento do sistema; 

- Lidar com a problemática da evolução do conhecimento, criando os procedimentos adequados; 

- Desenvolver um sistema de inferência capaz de implementar os mecanismos de raciocínio inerentes a estes sistemas. 
 
# Conhecimento imperfeito:

1. Utente
2. Prestador
3. Cuidado
4. Instituição

- Incerto (não se sabe quem é o pai)
	- Utente: Morada
	- Prestador: Especialidade
	- Cuidado: Descrição, Custo (~), Data (~)
	- Instituição: Nome (~)

- Impreciso (preço do Maserati)
	- Utente: Idade
	- Prestador: Nome, Especialidade
	- Cuidado: Custo
	- Instituição: Cidade

- Interdito (não se pode saber/inserir)
	- Utente: idade
	- Cuidado: IdUt

- Identificar conhecimento impreciso: 
	demo(excecao(Questao),R) -> R == verdadeiro

- Identificar conhecimento incerto: 


	Fora do range
	demo(Questao,R) -> R == falso.
	demo(excecao(Questao),R) -> R == desconhecido.

	Dentro do range
	demo(Questao,R) -> R == desconhecido.
	demo(excecao(Questao),R) -> R == verdadeiro.
	solucoes(Questao, S) -> Comp(S) == 0.

	Incerto
	demo(Questao,R) -> R == desconhecido.
	demo(excecao(Questao),R) -> R == verdadeiro.
	solucoes(Questao, S) -> Comp(S) == 1.


