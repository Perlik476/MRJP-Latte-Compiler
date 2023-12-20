# Kompilator Latte

Kompilator języka Latte, napisany w Haskellu z wykorzystaniem BNFC do generacji parsera.

## Budowanie

By zbudować kompilator, należy wykonać polecenie `make` w katalogu głównym projektu. Po zbudowaniu w katalogu głównym pojawi się plik `latc_llvm`.

## Struktura projektu

- `src` - katalog zawierający źródła kompilatora

- `src/Frontend` - plik zawierający moduł odpowiedzialny za frontend kompilatora

- `src/Latte` - katalog zawierający pliki wygenerowane przez BNFC

- `src/Latte.cf` - plik zawierający gramatykę języka Latte rozszerzoną o dodatkowe konstrukcje, patrz sekcja Rozszerzenia

- `src/Makefile` - plik zawierający reguły budowania kompilatora

- `README.md` - plik zawierający niniejszą dokumentację


## Rozszerzenia

- Tablice jednowymiarowe i pętle for

- Struktury

- Obiekty

- Metody wirtualne

## Kluczowe (potencjalnie niestandardowe) informacje o języku:


- Wszystkie wyrażenia złożone ze stałych, operatorów arytmetycznych i relacyjnych są ewaluowane, a ich wynik jest używany do ustalenia wartości logicznej wyrażeń warunkowych, co pozwala na ustalenie, czy funkcje zwracają wartości w każdym możliwym przypadku (z tych, które wynikają z ewaluacji wyrażeń warunkowych).

- Funkcje, zmienne i klasy są od siebie niezależne, przez co np. może istnieć funkcja o tej samej nazwie jak zmienna czy klasa. To samo dotyczy pól i metod w klasach.

- Wewnątrz klas można używać `self` do odwoływania się do atrybutów i metod klasy, choć nie jest to wymagane. Nie można używać `self` jako nazw zmiennych, funkcji czy klas, w tym nazw pól i metod.

- W bezpośrednim bloku funkcji nie można zadeklarować zmiennej o nazwie argumentu funkcji, choć można zrobić to w blokach w nim zagnieżdżonych.

- W bezpośrednim bloku `for` nie można zadeklarować zmiennej o nazwie zmiennej, która iteruje po kolekcji, choć można zrobić to w blokach w nim zagnieżdżonych.

- Poprawne operacje na napisach: `+`, `==`, `!=`.

- Poprawne operacje na liczbach: `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `<=`, `>`, `>=`.

- Poprawne operacje na wartościach logicznych: `==`, `!=`, `&&`, `||`.
