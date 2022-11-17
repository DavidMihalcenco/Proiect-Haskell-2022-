## Task 1
    - In primul rand imi trebuia doua functii care transforma din String in Float si invers pentru a putea calcula mai departe
        suma tuturor pasilor a undei persoane si de a scrie inapoi ca string acest rezultat. Am functia concateStr care primeste
        un string adun toti pasii din fiecare zi si ii inpart la 8 deoarece am 8 zile. Dupa in functia principala fac media 
        pentru toate persoanele.

## Task 2
    - La acest ex m-am gandit astfel : cu ajutorul functiei sumStep ma uit la cate o persoana cati pasi a facut in total, daca 
        are mai multi de 100 atunci ii scriu numele intr-o lista, daca nu pun o lista goala. Dupa am functia countLists care 
        se uita cate liste cu nume am si imi face un acumulator. Dupa am cateva functii care imi calculeaza suma pasilor unei
        persoane, si suma pasilor la totate persoanele.

## Task 3
    - Am functia mTranspose care imi transpune table-ul, datoria acestei functii voi avea mai multe liste cu pasii in fiecare zi.
        Dupa extrag cate o lista, adica i-au pasii tuturor persoanelor care s-au facut intr-o zi si aflu media.

## Task 4
    - La acest ex tot am folosit transpusa luad VeryActiveMinutes,FairlyActiveMinutes,LightlyActiveMinutes ca 3 liste aparte, dupa
        am in functia forEach extrag cate un element din aceste liste le compar, si in dependenta de faloare am 3 acumulatoare, care 
        insumeaza cate elemente mai mici ca 50 sunt in intervalul [50 100) si < 100.

## Task 5
    - Mi-am facut o functie cmp care dupa cum s-a spus in conditie se uita la total de pasi si sorteaza crescator, daca pasii sunt 
        egali compara dupa nume.

## Task 6
    - Mi-am facut o functie care primeste un string si calculeaza media a tuturor elementelor din string. Dupa am folosit aceasta
        functie pentru a afla media la primele 4 zile si la ultimele 4 zile, le-am concatenat cum s-a spus in conditie, si dupa 
        cu ajutorul functiei cmp1 care sorteaza dupa diferenta, daca diferenta e egala, dupa nume, am primit raspunsul.

## Task 7
    - Am aplicat functia f pe fiecare element din Table adica map (map)

## Task 8
    - Am aplicat functia f pe fiecare linie din table. Si la get_sleep_total am aflat suma minutelor la stringul primit.

## Task 2.1
    - Am functia findInd care primeste numele coloanei dupa care trebuie sortat tabelul
    un string care este header-ul tabelului si un aux care este pozitia acelei coloane 
    in primul array de stringuri. In tsort am un if care cu ajutorul functiei is Nr 
    verific daca stringul este un double, daca da atunci folosesc un comparator pentru
    numere daca nu folosesc unul pentru stringuri.

## Task 2.2
    - Daca headarurile tabelelor sunt egale atunci concatenez tablele.

## Task 2.3
    - Am creat functia sizeTable pentru a verifica diferenta dintre lungimi pentru ca 
    dupa sa mai pun stringuri goale in acel tabel in care nu sunt deajuns. In hunion 
    folosesc repeat care mai intai imi face un string de lungimea care am nevoie cu
    "" si dupa inca un repeat pentru a face mai multe stringuri goale.

## Task 2.4
    - Am functia mergeStrings care imi i-a un string si table se uita daca exista in table coloana specificata ca nume. Daca da atunci i-au din string-ul gasit 
    totul inafara de coloana care deja exista. In functia tjoinaux fac aceeasi doar
    ca pentru fiecare string deja din tabelul 1. In replace ma uit daca exista in tabelul 1 coloana din tabelul 2. Daca da atunci o inlocuiesc si primesc un tabel
    cu noua coloana. In replaceall voi apela functia replace pentru fiecare string din
    t2, asa voi primi un [table] de aceea i-au doar ultimul element care este acel cu
    toate coloanele inlocuite.
    
## Task 2.5
    - Am functia cartFunc care imi aplica functia data in exercitiu pe fiecare rand 
    din tabela primita, fac un map pentru a extrage fiecare row. Dupa apelez recursiv
    aceasta functie pe fiecare linie din tabelul 1, pentru a aplica aceasta functie pe 
    fiecare row din t1.

## Task 2.6
    - Transpun tabela pentru a putea mai usor sa filtrez fiecare coloana, coloana
    devenind rand ma uit daca in array-ul de columnName exista denumirea unei coloane
    de-ale mele, daca exista atunci las daca nu, nu las aceasta coloana. Dupa transpun 
    inapoi tabelul.

# Task 2.7
    - Cu ajutorul functiei findInd aflu ce coloana trebuie filtrata, si pe fiecare coloana aplic functia care a fost data ca paramentru.
    Dupa atasez la ce am primithead t.

# Task 3.1 
    - Apelez toate functiile create anterior

# Task 3.2
    - Fac compararile cum a fost specificat in enuntul temei.

# Task 3.4
    - In graphSolver primesc un row din table si ma uit pentru fiecare element din
        table daca functia mea intoarce ceva, daca e nothing atunci nu fac nimic
        daca returneaza ceva am un acumulator care salveaza numele, si ceea ce 
        intoarce functia.
        Dupa in forAllRows fac acceasi lucru incepand mereu de la elementele pentru
        care deja am facut, de ex ca sa nu fie 2 cu 1 si 1 cu 2.

# Task 3.5
    - Am creat eu functia eq_columns care in caz ca un string e la fel ca celalalt 
        imi acumuleaza numarul de string-uri la fel. Dupa in edge_new controlez 
        daca acest acumulator e mai mare de 5.
        Apelez functia Graph, filtrez string-urile care pe from si to nu au nimic,
        sortez dupa valuarea.
