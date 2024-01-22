# Kompilator Latte

Kompilator języka Latte, napisany w Haskellu z wykorzystaniem BNFC do generacji parsera.

## Budowanie

By zbudować kompilator, należy wykonać polecenie `make` w katalogu głównym projektu. Po zbudowaniu w katalogu głównym pojawi się plik `latc_llvm`.

## Opcje kompilatora

Kompilator `latc_llvm` przyjmuje następujące opcje:

- `--help` - wypisuje informacje o opcjach kompilatora
- `--verbose` - wypisuje informacje o przebiegu kompilacji
- `--comments` - wypisuje komentarze w kodzie LLVM
- `--remove-trivial-phis=0|1` - usuwa trywialne phi (domyślnie 1)
- `--CSE=0|LCSE|GCSE` - usuwa wspólne podwyrażenia arytmetyczne (domyślnie GCSE) (LCSE: w blokach podstawowych; GCSE: w obrębie funkcji, przy czym nie uwzględnia się wspólnych podwyrażeń pochodzących z bloków instrukcji warunkowej lub bloku pętli ani wyrażeń z samego ich warunku) 
- `--inline=0|1` - wstawia funkcje w miejsce wywołań (domyślnie 1) (przy czym nie wstawia się funkcji już zainlinowanych). Dostępne są dodatkowe opcje:
    - `--inline-max-depth=n` - maksymalna głębokość zagnieżdżenia wywołań funkcji, które mają zostać zainlinowane (domyślnie 3)`
    - `--inline-max-lines=n` - maksymalna liczba instrukcji w funkcji, która ma zostać zainlinowana (domyślnie 5) (przy czym liczba ta niekoniecznie jest dokładna, gdyż uwzględnia się instrukcje powstałe po transformacji drzewa AST)
- `--remove-trivial-blocks=0|1` - usuwa zbędne bloki (domyślnie 1) (tj. bloki, które zawierają tylko skok bezwarunkowy do następnego bloku i nie są wykorzystywane w żadnym phi)
- `--merge-blocks=0|1` - scala bloki, jeśli to możliwe (domyślnie 1) (tj. jeśli blok ma tylko jednego poprzednika i poprzednik ten ma tylko jednego następnika, to bloki te są scalane). Przykładem zastosowania jest następujący kod (przy wyłączonej opcji `--skip-trivial-conditions`):
```
    int foo() {
        int x = 0;
        if (x == 0) {
            return x;
        } else {
            x++;
        }
        return x;
    }
```
- `--skip-trivial-conditions=0|1` - pomija trywialne warunki (domyślnie 1) (np. `if (x == x) {...}` lub `x = 0; if (x == 0) {...}`)


## Struktura projektu

- `src` - katalog zawierający źródła kompilatora
- `src/Frontend.hs` - plik zawierający moduł odpowiedzialny za frontend kompilatora
- `src/AST.hs` - plik zawierający moduł odpowiedzialny za reprezentację AST
- `src/Compiler.hs` - plik zawierający moduł odpowiedzialny za uruchomienie kompilatora z odpowiednimi opcjami
- `src/Generator.hs` - plik zawierający moduł odpowiedzialny za generowanie kodu LLVM
- `src/TreeTransform.hs` - plik zawierający moduł odpowiedzialny za transformację drzewa AST, w tym przenazywanie zmiennych
- `src/Utils.hs` - plik zawierający moduł zawierający pomocnicze funkcje oraz typy danych używane w backendzie kompilatora
- `src/Latte` - katalog zawierający pliki wygenerowane przez BNFC
- `src/Latte.cf` - plik zawierający gramatykę języka Latte rozszerzoną o dodatkowe konstrukcje, patrz sekcja Rozszerzenia
- `src/Makefile` - plik zawierający reguły budowania kompilatora
- `README.md` - plik zawierający niniejszą dokumentację


## Rozszerzenia

- Użycie phi
- Tablice jednowymiarowe i pętle for
- Struktury
- Obiekty
- Metody wirtualne
- LCSE i GCSE

## Kluczowe (potencjalnie niestandardowe) informacje o języku:

- Wszystkie wyrażenia złożone ze stałych, operatorów arytmetycznych i relacyjnych są ewaluowane, a ich wynik jest używany do ustalenia wartości logicznej wyrażeń warunkowych, co pozwala na ustalenie, czy funkcje zwracają wartości w każdym możliwym przypadku (z tych, które wynikają z ewaluacji wyrażeń warunkowych).
- Funkcje, zmienne i klasy są od siebie niezależne, przez co np. może istnieć funkcja o tej samej nazwie jak zmienna czy klasa. To samo dotyczy pól i metod w klasach.
- Wewnątrz klas można używać `self` do odwoływania się do atrybutów i metod klasy, choć nie jest to wymagane. Nie można używać `self` jako nazw zmiennych, funkcji czy klas, w tym nazw pól i metod.
- W bezpośrednim bloku funkcji nie można zadeklarować zmiennej o nazwie argumentu funkcji, choć można zrobić to w blokach w nim zagnieżdżonych.
- W bezpośrednim bloku `for` nie można zadeklarować zmiennej o nazwie zmiennej, która iteruje po kolekcji, choć można zrobić to w blokach w nim zagnieżdżonych.
- Poprawne operacje na napisach: `+`, `==`, `!=`.
- Poprawne operacje na liczbach: `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `<=`, `>`, `>=`.
- Poprawne operacje na wartościach logicznych: `==`, `!=`, `&&`, `||`.

## Dodatkowe informacje

- Algorytm generowania phi bazuje na algorytmie opisanym w pracy [Simple and Efficient Construction of Static Single Assignment Form](https://link.springer.com/content/pdf/10.1007/978-3-642-37051-9_6.pdf).