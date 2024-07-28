# Nowe pliki przykładowe:

1. good/closures.mlt - domknięcia
2. good/comparisons_of_str_bool.mlt - porównania stringów i booli
3. good/concatenation.mlt - konkatenacja stringów
4. good/conflict.mlt - naprawiony konflikt shift/reduce
5. good/fun_var.mlt - funkcje jako zmienne, przekazanie funkcji przez referencje
6. good/lambdas.mlt - funkcje anonimowe
7. bad/arg_named_as_fun.mlt - argument funkcji nazwany tak samo jak funkcja
8. bad/dupped_arg_names.mlt - powtarzające się nazwy argumentów
9. bad/illegal_fname.mlt - nielegalna nazwa funkcji
10. bad/prosty.mlt - program x=1;

# Poprawki do projektu:

1. Z gramatyki wyrzuciłem inkrementację i dekrementację, ponadto naprawiłem konflikt shift/reduce w przypadku konkatenacji napisów.
   Ponadto dodałem Składnię funkcji anonimowych: lambda ZwracanyTyp <- (Argumenty) {ciało};
   albo lambda ZwracanyTyp <- (Argumenty) {ciało} (wyrażeniaArg); gdy chcemy od razu wywołać.
   Jeśli wywołujemy, to ze wszystkimi argumentami (brak aplikacji częściowych).

2. TC: wyrzucić IO - zrobione

3. TC: Maybe Env, wtedy będzie używany local tylko gdy wartość
   jest postaci Just - niezrobione

4. TC: usunąć Maybe z typeCheckExpr zrobione

5. TC nie powinien dopuścić redefinicji print zrobione

6. Interpreter: Maybe Env podobnie jak w TC niezrobione

7. Powinny być porównania dla String i Bool, konkatenacja dla
   String zrobione

8. Brzydko wypisywane typy w komunikatach o błędach. zrobione

9. Powinny być osobne błędy dla TC i Runtime zrobione

10. Pozwala Pan by nazwy parametrów funkcji się powtarzały:
    już nie pozwalam, zrobione

11. Czy można przedefiniować print ? nie można, zrobione

12. Program
    x = 1;
    powoduje wypisanie komunikatu: Bad type: expected Just (Int
    (Just (1,5))), got Nothing at line 1, column 1. Naprawiłem, zrobione

13. Zrealizować pakiet funkcyjny zrobione

# JPP-interpreter

kompilacja: make

interpretowanie pliku: ./Main <plik>

uruchomienie wszystkich przykladow z good i bad:
./runall.sh
(niektóre przyklady proszą o wprowadzenie danych, liczb całkowitych)

Cechy języka:

1. W przypadku deklaracji bez inicjalizacji, zmiennej nadawana jest wartość domyślna:

   - dla int: 0
   - dla string: ""
   - dla bool: false

2. Funkcja print przyjmuje jeden argument typu int, string lub boolean.
   Argument jest przekazywany przez wartość. Funkcja wypisuje argument
   na standardowe wyjście, oraz zwraca wartość swojego argumentu.

3. Wczytanie danych wejściowych odbywa się za pomocą funkcji:

   - int readInt()
   - string readString()

4. Można porównywać zmienne typu int, string i boolean.
