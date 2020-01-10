;;; check-regexp(RE)
;;; Funzione di supporto a is-regexp(RE), nella quale viene controllato
;;; il primo elemento della lista in input RE, nel caso esso sia una lista
;;; a sua volta. 
(defun check-regexp (RE)
    (if (atom (first RE))
            (cond
                ((eql (first RE) 'seq) T)
                ((eql (first RE) 'or) T)
                ((eql (first RE) 'star) T)
                ((eql (first RE) 'plus) T)
                (T (is-regexp (rest RE))))))


;;; is-regexp(RE)
;;; Funzione principale per il controllo di validità di una regular expression
;;; data in input. Nel caso il valore di input RE risulta nullo, ritorna NIL;
;;; nel caso risulta un atomo, ritorna T; nel caso di una lista viene chiamata
;;; la funzione di supporto check-regexp(RE)
(defun is-regexp (RE)
    (cond
        ((null RE) NIL)
        ((atom RE) T)
        ((listp RE) (check-regexp RE))))
 

;;; comp-seq(RE I F Q)
;;; Funzione di supporto a comp-start(RE I F Q). Essa genera la sezione
;;; dell'automa riguardante la seq.
;;; Le variabili di input saranno:
;;; RE: la regexp processata dal programma
;;; I:  stato iniziale corrente
;;; F:  stato finale
;;; Q:  lista delle liste di transizioni dell'automa
(defun comp-seq (RE I F Q)
    (cond
        ((eql (first RE) NIL) I F Q)
        ((atom (first RE))
        (setf temp (gensym "Q"))
            (cond  
                ((eql (first (rest RE)) NIL) 
                    (comp-seq (rest RE) F F (cons (list I (first RE) F) Q)))
                
                (T (comp-seq (rest RE) temp F 
                    (cons (list I (first RE) temp) Q)))))
        ((listp (first RE))
            (cond 
                ((eql (first (rest RE)) NIL)
                    (comp-seq (rest RE) (gensym "Q") F 
                        (append (comp-start (first RE) I F Q) Q)))
                    
                (T
                    (setf temp2 (gensym "Q"))
                    (comp-seq (rest RE) temp2 F 
                        (append (comp-start (first RE) I temp2 Q) Q)))))))


;;; comp-or(RE I F Q)
;;; Funzione di supporto a comp-start(RE I F Q). Essa genera la sezione
;;; dell'automa riguardante l'or.
;;; Le variabili di input saranno:
;;; RE: la regexp processata dal programma
;;; I:  stato iniziale corrente
;;; F:  stato finale
;;; Q:  lista delle liste di transizioni dell'automa 
(defun comp-or (RE I F Q)
    (cond
        ((eql (first RE) NIL) I F Q)
        ((atom (first RE))
            (comp-or (rest RE) I F (cons (list I (first RE) F) Q)))
        ((listp (first RE))
            (cond 
                ((eql (first (rest RE)) NIL)
                    (comp-or (rest RE) (gensym "Q") F
                        (append (comp-start (first RE) I F Q) Q)))
                (T 
                    (comp-or (rest RE) I F
                        (append (comp-start (first RE) I F Q) Q)))))))


;;; comp-star(RE I F Q)
;;; Funzione di supporto a comp-start(RE I F Q). Essa genera la sezione
;;; dell'automa riguardante la star.
;;; Le variabili di input saranno:
;;; RE: la regexp processata dal programma
;;; I:  stato iniziale corrente
;;; F:  stato finale
;;; Q:  lista delle liste di transizioni dell'automa
(defun comp-star (RE I F Q)
    (cond
        ((eql (first RE) NIL) I F 
            (append (list (list I 'epsilon F) (list F 'epsilon I)) Q))
        ((atom (first RE))
            (comp-star (rest RE) I F (cons (list I (first RE) F) Q)))
        ((listp (first RE))
            (comp-star (rest RE) I F 
                (append (comp-start (first RE) I F Q) Q)))))


;;; comp-plus(RE I F Q)
;;; Funzione di supporto a comp-start(RE I F Q). Essa genera la sezione
;;; dell'automa riguardante il plus.
;;; Le variabili di input saranno:
;;; RE: la regexp processata dal programma
;;; I:  stato iniziale corrente
;;; F:  stato finale
;;; Q:  lista delle liste di transizioni dell'automa
(defun comp-plus (RE I F Q)
    (cond
        ((eql (first RE) NIL) I F 
            (append (list (list F 'epsilon I)) Q))
        ((atom (first RE))
            (comp-plus (rest RE) I F (cons (list I (first RE) F) Q)))
        ((listp (first RE))
            (comp-plus (rest RE) I F 
                (append (comp-start (first RE) I F Q) Q)))))


;;; comp-start(RE I F Q)
;;; Funzione di supporto principale dove avviene la generazione 
;;; dell'automa a stati finiti.
;;; Essa controllerà il primo elemento dell'input RE. Nel caso di un atomo,
;;; viene restituita immediatamente la lista con l'unica transizione
;;; dell'automa. Nel caso di una lista, viene eguagliato agli operatori
;;; del linguaggio, richiamando la funzione pertinente in base all'operatore.
;;; Le variabili di input saranno:
;;; RE: la regexp processata dal programma
;;; I:  stato iniziale corrente
;;; F:  stato finale
;;; Q:  lista delle liste di transizioni dell'automa
(defun comp-start (RE I F Q)
    (cond 
        ((atom RE) (list F RE I))
        ((eql (first RE) 'seq) (comp-seq (rest RE) I F Q))
        ((eql (first RE) 'or) (comp-or (rest RE) I F Q))
        ((eql (first RE) 'star) (comp-star (rest RE) I F Q))
        ((eql (first RE) 'plus) (comp-plus (rest RE) I F Q))))


;;; nfa-regexp-comp(RE I F Q)
;;; Funzione principale per la generazione dell'automa a stati finiti.
;;; Essa controllerà la validità della regular expression inserita
;;; dall'utente, per poi richiamare la funzione comp-start, con stato iniziale
;;; I, stato finale F e base della lista di transizioni come lista vuota
(defun nfa-regexp-comp (RE) 
    (if (is-regexp RE) T NIL)
    (remove-duplicates 
        (reverse (comp-start RE 'I 'F '()))
        :from-end t))


;; nfa-test
;;; hey-google(S FA Y)
;;; Funzione di supporto alla funzione google(S FA X).
;;; Essa andrà a generare una lista contenente solo le transizioni
;;; relative allo stato iniziale S ricercato.
;;; Le variabili di input saranno:
;;; S:  stato iniziale
;;; FA: lista delle transizioni dell'intero automa
;;; Y:  lista delle transizioni relative a S 
(defun hey-google (S FA Y)
    (google S (rest FA) (remove 'nil (append (list (assoc S FA)) Y))))


;;; google(S FA X)
;;; Funzione di supporto principale per la generazione della lista di
;;; transizioni relative allo stato iniziale S.
;;; Le variabili di input saranno:
;;; S:  stato iniziale
;;; FA: lista delle transizioni dell'intero automa
;;; X:  lista delle transizioni relative a S 
(defun google (S FA X)
    (cond 
        ((eql (first FA) NIL) X)
        ((atom (first FA)) (list FA))
        ((listp (first FA)) (hey-google S FA X))))


;;; do-epsilon(alist)
;;; Funzione di supporto che percorrerà l'epsilon transizione
;;; nell'automa per permettere l'avanzamento del programma, 
;;; se necessario.
(defun do-epsilon (alist)
    (cond
        ((eql (first alist) NIL) NIL)
        ((eql (first (rest (first alist))) 'epsilon) 
            (first (rest (rest (first alist)))))
        (T (do-epsilon (rest alist)))))


;;; check-empty-star(I alist)
;;; Funzione di supporto che percorrerà l'epsilon transizione
;;; nel caso di una star percorsa 0 volte.
(defun check-empty-star (alist)
(cond 
    ((and 
        (eql (first (rest (first alist))) 'epsilon)
        (eql (first (rest (rest (first alist)))) 'F)) 
        T)))


;;; check-input(input FA)
;;; Funzione di supporto che controllerà la correttezza dell'input
;;; relativamente all'automa richiamato.
(defun check-input (input FA)
    (cond
        ((eql (first FA) NIL) NIL)
        ((eql (first input) (first (rest (first FA)))) NIL)
        (T (check-input input (rest FA)))))


;;; looper(I input F alist FA)
;;; Funzione di supporto che controllerà le varie casistiche di
;;; percorrenza delle transizioni dell'automa, in base al primo
;;; elemento presente nell'input.
;;; Le variabili di input saranno:
;;; I:     stato iniziale
;;; input: la lista di caratteri da controllare
;;; F:     stato finale
;;; alist: lista di transizioni relative a I
;;; FA:    lista di transizioni dell'automa
(defun looper (I input F alist FA)
    (cond

        ;; input non vuoto, non seguire le epsilon o va in loop
        ((eql (first alist) NIL) (check-input input FA))
        
        ;; I = F & alist con epsilon, segui la epsilon
        ((and (eql I F) (eql (first (rest (first alist))) 'epsilon)) 
            (tester
                (first (rest (rest (first alist)))) input F FA))

        ;; I = F & alist vuoto, errore
        ((and (eql I F) (eql (first alist) NIL)) NIL)

        ;; input finito, controlla se star vuota
        ((eql (first input) NIL) (check-empty-star alist))

        ;; alist vuoto, controlla epsilon
        ((eql (first alist) NIL)
            (tester 
                (do-epsilon (remove-duplicates (google I FA '()) :from-end t))
                input F FA))

        ;; consuma input e procedi lungo l'FA
        ((and (eql I (first (first alist))) 
            (eql (first input) (first (rest (first alist)))))
            (tester (first (rest (rest (first alist)))) (rest input) F  FA))

        ;; Input vuoto e alist vuoto, errore
        ((and (eql I NIL) (eql (first alist) NIL)) NIL)        

        ;; Ricorsività
        (T (looper I input F (rest alist) FA))))


;;; tester(I input F FA)
;;; Funzione di supporto principale per il test dell'input relativamente ad un
;;; automa generato.
;;; Essa controllerà la validità dello stato iniziale, la risoluzione
;;; dell'automa, ovvero input consumato totalmente e stato iniziale uguale
;;; allo stato finale, oppure nel caso base chiamerà la funzione di
;;; supporto looper(I input F alist FA)
(defun tester (I input F FA)
    (cond
        ((eql I NIL) NIL)
        ((and (eql (first input) NIL) (eql I F)) T)
        (T (looper I input F 
            (remove-duplicates (google I FA '()) :from-end t) FA))))


;;; nfa-test(FA input)
;;; Funzione principale per il controllo dell'input relativamente
;;; ad un automa generato da nfa-regexp-comp(FA)
(defun nfa-test (FA input)
    (tester 'I input 'F FA))
        