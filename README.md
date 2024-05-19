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

2. można porównywać jedynie liczby całkowite.

3. Funkcja print przyjmuje jeden argument typu int, string lub boolean.
   Argument jest przekazywany przez wartość. Funkcja wypisuje argument
   na standardowe wyjście, oraz zwraca wartość swojego argumentu.

4. Wczytanie danych wejściowych odbywa się za pomocą funkcji:
   - int readInt()
   - string readString()
