%%% is_regexp/1
%%% Funzione principale per controllare la validità della
%%% regular expression data in input. Essa controllerà
%%% una epsilon transizione, la presenza di un atomo oppure
%%% gli operatori del linguaggio plus, star, seq e or.
is_regexp(epsilon) :-
  true, !.


is_regexp(RE) :-
    atomic(RE), !.


%% controlla se è una lista
is_regexp(RE) :-
  is_list(RE),
  open_regexp(RE), !.


%% controlla plus/n
is_regexp(plus(RE)) :-
    check_if_not_double(RE, star),
    check_if_not_double(RE, plus),
    is_regexp(RE), !.
  

%% controlla star/n
is_regexp(star(RE)) :-
  check_if_not_double(RE, star),
  check_if_not_double(RE, plus),
  is_regexp(RE), !.


%% controlla seq/n
is_regexp(RE) :-
  =..(RE, [ seq | Rest ]),  % RE = seq(Rest)
  open_regexp(Rest), !.


%% controlla or/n
is_regexp(RE) :-
  =..(RE, [ or | Rest ]),   % RE = or(Rest)
  open_regexp(Rest), !.


%% Controlllo predicati riservati
is_regexp(RE) :-
  compound(RE),
  is_not_reserved(RE).


%%% is_not_reserved/1
%%% Funzione di supporto che controlla l'utilizzo di termini riservati
is_not_reserved(RE) :-
  =..(RE, [Testa | _Coda]),
  dif(Testa, seq),
  dif(Testa, or),
  dif(Testa, star),
  dif(Testa, plus).


%%% check_if_not_double/2
%%% Funzione di supporto che controlla l'utilizzo illegale di sequenze
%%% di termini riservati, come per esempio star(star(a)).
check_if_not_double(RE, Predicato) :-
  atomic(RE),
  dif(RE, Predicato), !.


check_if_not_double(RE, Predicato) :-
  =..(RE, [ Predicato2 | _Coda ]), % RE = Predicato2(_Coda)
  dif(Predicato, Predicato2), !.


check_if_not_double(_RE, _Predicato) :-
  fail, !.


%%% open_regexp/1
%%% Caso base della funzione di supporto open_regexp/2 definita successivamente 
open_regexp([]) :-
  true, !.

%%% open_regexp/2+
%%% Funzione di supporto che legge il primo elemento di una lista data in input
%%% controllando la sua validità tramite il predicato is_regexp/1,
%%% procedendo ricorsivamente con il resto della lista.
open_regexp([ Testa | Coda ]) :-
  is_regexp(Testa),
    open_regexp(Coda), !.


%%% nfa_regexp_comp/2
%%% Funzione principale per la generazione dell'automa a stati finiti,
%%% identificato da una variabile FA_Id.
nfa_regexp_comp(FA_Id, RE) :-
  is_regexp(RE),
  gensym(q, Start),
  assert(nfa_initial(FA_Id, Start)),
  gensym(q, End),
  assert(nfa_final(FA_Id, End)),
  nfa_regexp_comp(FA_Id, RE, Start, End).


%%% nfa_regexp_comp/4
%% caso di un atomo
nfa_regexp_comp(FA_Id, RE, Start, End) :-
  is_regexp(RE),
  atomic(RE),
  assert(nfa_delta(FA_Id, Start, RE, End)),
  !.


%% Caso PLUS
nfa_regexp_comp(FA_Id, plus(RE), Start, End) :-
  is_regexp(RE),
  nfa_regexp_comp(FA_Id, seq(RE, RE, star(RE)), Start, End),
  !.


%% Caso STAR
nfa_regexp_comp(FA_Id, star(RE), Start, End) :-
  is_regexp(RE),
  assert(nfa_delta(FA_Id, Start, epsilon, End)),
  gensym(q, Start_1),
  assert(nfa_delta(FA_Id, Start, epsilon, Start_1)),
  gensym(q, End_1),
  assert(nfa_delta(FA_Id, End_1, epsilon, Start_1)),
  assert(nfa_delta(FA_Id, End_1, epsilon, End)),
  nfa_regexp_comp(FA_Id, RE, Start_1, End_1),
  !.


%% Caso SEQ
nfa_regexp_comp(FA_Id, RE, Start, End) :-
  is_regexp(RE),
  =..(RE, [ seq, Next ]),
  nfa_regexp_comp(FA_Id, Next, Start, End),
  !.


nfa_regexp_comp(FA_Id, RE, Start, End) :-
  is_regexp(RE),
  =..(RE, [ seq, Next, Rest ]),
  gensym(q, End_2),
  nfa_regexp_comp(FA_Id, Next, Start, End_2),
  gensym(q, Start_2),
  assert(nfa_delta(FA_Id, End_2, epsilon, Start_2)),
  nfa_regexp_comp(FA_Id, Rest, Start_2, End),
  !.


nfa_regexp_comp(FA_Id, RE, Start, End) :-
  is_regexp(RE),
  =..(RE, [ seq, Next | Rest ]),
  gensym(q, End_2),
  nfa_regexp_comp(FA_Id, Next, Start, End_2),
  gensym(q, Start_2),
  gensym(q, End_3),
  assert(nfa_delta(FA_Id, End_2, epsilon, Start_2)),
  nfa_regexp_comp(FA_Id, Next, Start_2, End_3),
  gensym(q, Start_3),
  assert(nfa_delta(FA_Id, End_3, epsilon, Start_3)),
  nfa_regexp_comp(FA_Id, Rest, Start_3, End),
  !.


nfa_regexp_comp(FA_Id, RE, Start, End) :-
  is_regexp(RE),
  =..(RE, [ or, Next | Rest]),
  gensym(q, Start_2),
  assert(nfa_delta(FA_Id, Start, epsilon, Start_2)),
  nfa_regexp_comp(FA_Id, Next, Start_2, End),
  nfa_cycle_or(FA_Id, Rest, Start_2, End),
  !.


nfa_regexp_comp(FA_Id, [ RE ], Start, End) :-
  is_regexp(RE),
  nfa_regexp_comp(FA_Id, RE, Start, End),
  !.
 

nfa_regexp_comp(FA_Id, [ Next | Rest ], Start, End) :-
  gensym(q, End_2),
  nfa_regexp_comp(FA_Id, Next, Start, End_2),
  gensym(q, Start_2),
  assert(nfa_delta(FA_Id, End_2, epsilon, Start_2)),
  nfa_regexp_comp(FA_Id, Rest, Start_2, End),
  !.


%%% nfa_cycle_or/4
%%% Funzione di supporto alla generazione di un automa a stati finiti
%%% nel caso di un'operazione OR
nfa_cycle_or(FA_Id, [ RE ], Start, End) :-
  nfa_regexp_comp(FA_Id, RE, Start, End),
  !.

nfa_cycle_or(FA_Id, [ Next | Rest ], Start, End) :-
  nfa_regexp_comp(FA_Id, Next, Start, End),
  nfa_cycle_or(FA_Id, Rest, Start, End),
  !.


%%% nfa_test/2
%%% Funzione principale di controllo di un input relativamente ad un automa
%%% identificato da una variabile FA_Id
nfa_test(FA_Id, []) :-
  nfa_initial(FA_Id, Q),
  nfa_test(FA_Id, [], Q),
  !.


nfa_test(FA_Id, []) :-
  nfa_initial(FA_Id, Q),
  nfa_delta(FA_Id, Q, X, NextQ),
  X = epsilon,
  nfa_test(FA_Id, [], NextQ),
  !.


nfa_test(FA_Id, []) :-
  nfa_initial(FA_Id, Q),
  nfa_delta(FA_Id, Q, X, NextQ),
  X = [],
  nfa_test(FA_Id, [], NextQ),
  !.


nfa_test(FA_Id, [ Char | NextChar ]) :-
  nfa_initial(FA_Id, Q),
  nfa_delta(FA_Id, Q, Char, NextQ),
  nfa_test(FA_Id, NextChar, NextQ),
  !.


nfa_test(FA_Id, [ Char | NextChar]) :-
  nfa_initial(FA_Id, Q),
  nfa_delta(FA_Id, Q, Y, NextQ),
  Y = [],
  nfa_test(FA_Id, [ Char | NextChar], NextQ),
  !.


nfa_test(FA_Id, [ Char | NextChar]) :-
  nfa_initial(FA_Id, Q),
  nfa_delta(FA_Id, Q, Y, NextQ),
  Y = epsilon,
  nfa_test(FA_Id, [ Char | NextChar], NextQ),
  !.


%%% nfa_test/3
nfa_test(FA_Id, [], QState) :-
  nfa_final(FA_Id, QState),
  !.


nfa_test(FA_Id, [], QState) :-
  nfa_delta(FA_Id, QState, X, NextQ),
  X = epsilon,
  nfa_test(FA_Id, [], NextQ),
  !.


nfa_test(FA_Id, [ First | Rest ], QState) :-
  nfa_delta(FA_Id, QState, X, NextQ),
  X = epsilon,
  nfa_test(FA_Id, [ First | Rest ], NextQ), 
  !.


nfa_test(FA_Id, [ First | Rest ], QState) :-
  nfa_delta(FA_Id, QState, First, NextQ),
  nfa_test(FA_Id, Rest, NextQ),
  !.


%%% nfa_clear/1
%%% Funzione per pulire la base di conoscenza instanziata durante
%%% la generazione di un automa con nfa_regexp_comp/2
nfa_clear(FA_Id) :-
  retract(nfa_delta(FA_Id, _, _, _)),
  nfa_clear(FA_Id).


nfa_clear(FA_Id) :-
  retract(nfa_initial(FA_Id, _)),
  retract(nfa_final(FA_Id, _)).


nfa_clear :-
  retractall(nfa_delta(_, _, _, _)),
  retractall(nfa_initial(_, _)),
  retractall(nfa_final(_, _)).
  

%%% nfa_list/1
%%% Funzione per elencare la base di conoscenza, possibilmente filtrando
%%% tramite l'identificatore FA_Id.
nfa_list(FA_Id) :-
  listing(nfa_delta(FA_Id, _, _, _)),
  listing(nfa_initial(FA_Id, _)),
  listing(nfa_final(FA_Id, _)).


nfa_list :-
  listing(nfa_delta(_, _, _, _)),
  listing(nfa_initial(_, _)),
  listing(nfa_final(_, _)).
