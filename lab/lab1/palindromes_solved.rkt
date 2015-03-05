;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname palindromes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Avem de rezolvat urmatoarea problema:
;; Sa se gaseasca toate numerele naturale, mai mici sau egale cu un numar n,
;; care sunt palindroame atat in baza 10 cat si in baza 2

;; Pentru a rezolva problema, vom defini, pe rand, functii pentru:
;; - reprezentarea (ca lista de "cifre") a unui numar natural intr-o baza b
;; - testul ca o lista este palindrom (adica este totuna cu lista inversata)
;; - testul ca un numar este palindrom in toate bazele dintr-o lista data
;; - parcurgerea tuturor numerelor pana la un numar dat n, si selectarea celor
;;   palindroame in toate bazele dintr-o lista data de baze

;; 1. (2p)
;; Fie urmatoarele axiome pentru obtinerea reprezentarii unui numar natural
;; in baza b.
;; num->base(0,b) = [ ]                                   ; pt n=0
;; num->base(n,b) = num->base(n div b, b) ++ [ n mod b ]  ; pt n>0
;; Implementati functia corespunzatoare in Scheme:

(define (consB L e)
  (if (null? L) 
  (cons e '())
  (cons(car L) (consB (cdr L) e))))


(define (num->base n b)
  (if (zero? n) 
      '() 
      (consB (num->base(quotient n b) b) (remainder n b))))
  
  (check-expect (num->base 489 10) '(4 8 9))
  (check-expect (num->base 489 2) '(1 1 1 1 0 1 0 0 1))
  
  ;; 2. (2p)
  ;; Fie urmatoarele axiome pentru inversarea unei liste.
  ;; rev([ ]) = [ ]
  ;; rev(x:l) = rev(l) ++ [x]
  ;; Implementati functia corespunzatoare in Scheme:
  
  (define (rev L)
    (if (null? L)
    '()
    (consB (rev(cdr L)) (car L))))
(check-expect (rev '(5 1 4 8 7)) '(7 8 4 1 5))

;; 3. (1p)
;; Implementati testul ca o lista L este palindrom.

(define (palindrome? L)
  (if (equal? L (rev L))
      #t
      #f))

(check-expect (palindrome? '(1 4 4 1)) #t)
(check-expect (palindrome? '(1 4 2 4 1)) #t)
(check-expect (palindrome? '(1 4 4 1 4 1)) #f)

;; 4. (2,5p)
;; Testati ca n este palindrom in toate bazele din lista Bases

(define (all-palindromes? n Bases)
  ;; daca Bases e null
  (if(null? Bases)
     #t
     ;; verific dc n este palindromul in primul element din lista Bases
     (if(palindrome? (num->base n (car Bases)))
        ;; daca da, apelez fct recursiv pt restul listei
        (all-palindromes? n (cdr Bases))
        #f)))

(check-expect (all-palindromes? 585 '(2 10)) #t)
(check-expect (all-palindromes? 594 '(2 10)) #f)

;; 5. (2,5p)
;; Gasiti toate numerele naturale, mai mici sau egale cu n, care sunt
;; palindroame in toate bazele din lista Bases

(define (palindromes-to-n n Bases)
  ; daca n este zero, returez lista (0)
  (if(zero? n)
     (cons 0 '())
     ;; daca n este palindrom in toate bazele din lista Bases
     (if(all-palindromes? n Bases)
        ;; apelez recursiv functia pentr n-1) si returnez rezultatul +n
        (consB (palindromes-to-n (sub1 n) Bases )n )
        ;;
        (palindromes-to-n (sub1 n) Bases))))
      
      (check-expect (palindromes-to-n 100 '(2 10)) '(0 1 3 5 7 9 33 99))
      
      ;; 6. BONUS (2p)
      ;; Sa se gaseasca primul numar de cel putin 2 cifre care este palindrom in
      ;; minim 4 baze dintre bazele 2, 3, 4, 5, 6, 7, 8, 9, 10.
      
      (define first-4-palindrome
        'your-code-here)
      
      (check-expect first-4-palindrome 121)
      
      ;; 7. BONUS (3p)
      ;; Un numar Lychrel este un numar natural care nu devine palindrom in urma
      ;; procesului iterativ de a aduna numarul cu inversul sau.
      ;; Exemple de numere care nu sunt Lychrel:
      ;; 56 devine palindrom dupa o iteratie: 56 + 65 = 121
      ;; 59 devine palindrom dupa 3 iteratii:
      ;; 59 + 95 = 154, 154 + 451 = 605, 605 + 506 = 1111
      ;; Sa se gaseasca numerele naturale pana la Max care nu sunt si nu devin
      ;; palindroame dupa n iteratii
      
      (define (maybe-lychrel Max n)
        'your-code-here)
      
      (check-expect (maybe-lychrel 200 25) '(196))