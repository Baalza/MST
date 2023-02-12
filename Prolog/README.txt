Componenti del gruppo:
1) Balzarotti Niccolò 852003
2) Covelli Matteo 861277
3) Ghanimi Alaaeddine 856573

Implementazione in prolog dell'algoritmo di Prim per la ricerca del minimum
spanning tree in un grafo non diretto con archi etichettati indicanti il 
peso

Il file principale è "mst.pl"
Per effettuare l'esecuzione dell'algoritmo bisogna lanciare in ordine 
mst-prim(G, Source)
e successivamente, sempre per lo stesso grafo
mst-get(G, Source)

**** GRAFO ****
Il file contiene le API per l'implementazione in prolog di un grafo. Ogni 
grafo è un insieme di fatti salvati nella base di dati prolog:

API:
graph(G) -> rappresenta un grafo di nome G.
vertex(G, V) -> rappresenta un vertice del grafo G.
arc(G, A, B, W) -> rappresenta un arco tra il vertice A e il vertice B 
                   del grafo G.

I predicati che utilizzano le API sono i seguenti:
new_graph(G) -> Creazione del grafo G nella base di dati prolog.
delete_graph(G) -> Rimuove sia il grafo, i vertici e gli archi
                   dalla base di dati prolog.
new_vertex(G,V) -> Crea il vertice V associato al grafo G.
graph_vertices(G, Vs) -> Ritorna il valore true se la lista Vs contiene tutti
                         i vertici associati a G, si possono ottenere tutti i
                         vertici associati a G utilizzando Vs come variabile.
list_vertices(G) -> Output dei vertici associati a G sul terminale.
new_arc(G, A, B, W) -> Creazione di un arco tra i verti A e B
                       associati al grafo G con peso W non negativo.
		       Se l'arco esiste, ma il peso W è diverso, viene
		       sovrascritto.
graph_arcs(G, Es) -> Ritorna il valore true se la lista Es contiene
                     tutti gli archi associati al grafo G,
                     si possono ottenere tutti gli archi utilizzando Es
                     come variabile.
vertex_neighbors( G, V, Ns) -> Ritorna il valore true se la lista Ns contiene
                               tutti gli archi che connettono il vertice V
                               associato al grafo G agli altri vertici se
                               l'arco (diretto o indiretto) è presente
                               nella base di dati prolog.
adjs(G, V, Vs) -> Ritorna il valore true se la lista Vs contiene i vertici
                  adiacenti a V se esiste l'arco che li collega nella base
                  di dati prolog.
list_arcs(G) -> Output degli archi associati a G sul terminale.
list_graph(G) -> Output di tutti i vertici e archi associati a G sul terminale.
La libreria utilizzata per la lettura e scrittura del file è csv.
read_graph(G, FileName) -> Lettura delle row del FileName e creazione 
                           del grafo G, dei vertici e degli archi.
                           (Il FileName deve essere un file .csv, ogni riga
                           deve contenere 3 elementi (vertice A vertice B peso W) 
                           separati da un carattere di tabulazione 
                           (tab separated value).
Questo predicato utilizza i seguenti predicati ausiliari:
create_graph(G, X) -> Crea all'interno della base di dati il grafo G
                      e tutti i suoi componenti (vertici e archi) contenuti
                      nella lista X.
X è una lista in cui ogni elemento è una lista formata in questo modo:
[A, B, W]
row_to_list(Row, Xs) -> Questo predicato trasforma il predicato row(A, B, W)
                        in una lista Xs come segue:
                        [A, B, W]


write_graph(G, FileName, Type) -> Questo predicato è vero quando il grafo G
                                  viene scritto sul FileName (.csv)
La variabile Type può essere di due tipi:
Graph -> G è un termine che identifica un grafo nella base di dati prolog.
         Ogni riga del FileName conterrà 3 elementi:
         Il vertice A il vertice B e il peso W presi dalla base di dati prolog.
Edge -> G è una lista formata dagli archi associati al grafo che stiamo scrivendo.
        Ogni riga del FileName conterrà 3 elementi:
        Il vertice A il vertice B e il peso W presi dalla base di dati prolog.
Questo predicato utilizza i seguenti predicati ausiliari:
arc_to_row(arc(G, A, B, W), row(A, B, W)) -> Questo predicato trasforma l'arco
                                             in un predicato row(A, B, W).
write_graph(G, FileName) -> Questo predicato richiama il predicato
                            write_graph(G, FileName, graph).

**** HEAP ****
Il file contiene le API per l'implementazione in prolog di un heap. Ogni 
heap è un insieme di fatti salvati nella base di dati prolog:

API:
heap(H, S) -> Rappresenta un heap di nome H e con heap size uguale a S.
heap_entry(H, P, K, V) -> Rappresenta un elemento dell'heap H con chiave K valore V e posizione P
                          (le posizioni partono da 1).
I predicati che utilizzano le API sono i seguenti:
new_heap(H) -> Creazione di un heap H nella base di dati prolog.
delete_heap(H) -> Rimuove l'heap H, heap entry incluse, dalla base di dati prolog.
heap_has_size(H, S) -> Il predicato è vero quando S è la dimensione corrente
                       dell'heap H.
heap_empty(H) -> Il predicato è vero quando l'heap H non contiene elementi.
heap_not_empty(H) -> Il predicato p vero quando l'heap H contiene almeno un elemento.
heap_head(H, K, V) -> Il predicato è vero quando l'elemento dell'heap H con chiave
                      min K è V.
list_heap(H) -> Output di tutte le heap_entry dell' heap H sul terminale.
heap_insert(H, K, V) -> Inserisce l'elemento con chiave K e valore V nell heap H
                        mantenendo la propietà di un min heap.
Questo predicato utilizza i seguenti predicati ausiliari:
heap_increase(H, N) -> Questo predicato risale il ramo in cui è presente il nodo N
                       fino a quando la proprietà min_heap è rispettata.
        Questo predicato utilizza i seguenti predicati ausiliari:
	swap_position(H, P1, K1, V1, P2, K2, V2) -> Il predicato scambia l'elemento
                                                    heap_entry(H, P1, K1, V1) con l'elemento
                                                    heap_entry(H, P2, K2, V2) invertendo le
                                                    posizioni P1 e P2.
	minimum(H, N, F, S, Min) -> Questo predicato è vero se Min è pari alla chiave più
                                    piccola tra il nodo padre F e il figlio S.
heapify(H, T) -> Il predicato sistema l'heap partendo dalla posizione T fino alle foglie
                 fino a quando la proprietà di min_heap è rispettata.
                 Il predicato può essere utilizzato solo se il figlio destro e il figlio sinistro
                 di T sono min_heap.
        
        Questo predicato utilizza i seguenti predicati ausiliari:
	minimum(H, P1, P2, P3, Min) -> Questo predicato è vero se Min è pari alla chiave più piccola
                                       associata ai 3 elementi nelle posizioni P1, P2 e P3.



heap_extract(H, K, V) -> Il predicato estrae la coppia K, V con K minima rimuovendola dall heap H e 
                         mantenendo la proprietà di min_heap.
         Questo predicato utilizza i seguenti predicati ausiliari:
         heapify(H, T).

**** MST ****
Per l'algoritmo di Prim sono stati utilizzati i seguenti due fatti:
vertex_key(G, V, K) -> Il predicato è vero quando V è un vertice di G e durante e dopo
                       l'esecuzione dell'algoritmo di Prim, contiene il peso minimo di un arco
                       che connette V nell'albero minimo. Se l'arco non esiste la chaive K è
                       'inf'.
vertex_previous(G, V, U) -> Il predicato è vero quando V e U sono vertici di G e durante e dopo
                            l'esecuzione dell'algoritmo di Prim, il vertice U è il vertice genitore di V
                            nel minimum spanning tree.
mst_prim(G, Source) -> Il predicato, dopo la sua esecuzione, inserisce nella base di dati i predicati
                       vertex_key(G, V, K) per ogni V appartenente a G e inserisce i predicati
                       vertex_previous(G, V, U) per ogni V ottenuti.
Questo predicato utilizza i seguenti predicati ausiliari:
list_lenght(Lista, Lenght) -> E' true se lenght è la lunghezza della lista.
inizialization(G, H, Source) -> Questo predicato inizializza i vertex key di ogni nodo
                                mettendo come chiave K 'inf' e come vertex_previous
                                setta tutti i genitori a nil.
                                Inoltre crea l'heap H e lo inizializza con il predicato inizialization_heap(G, H, Vs).
	Questo predicato utilizza i seguenti predicati ausiliari:
	inizialization_vertices(G, Vs) -> Questo predicato inserisce nella base di dati vertex_key(G, V, inf) per ogni
                                          V appartenente al grafo G e vertex_previous(G, V, nil) per ogni V
                                          appartenene al grafo G (Vs è la lista che contiene tutti i vertici).
	
	inizialization_heap(G, H, V) -> Questo predicato inserisce nella base di dati prolog i vertici contenuti nella lista
                                V all'interno dell'heap con chiave pari al peso dell'arco entrante nel nodo e come
                                valore il nome del nodo.
update_neighbors(G, H, V, neighbors) -> Questo predicato attraverso la lista neighbors contenente i neighbors del vertice V,
					aggiorna la chiave presente nell' heap se il peso dell'arco è minore o uguale
				        alla chiave presente nell'heap.
mst_get(G, Source, PreorderTree) -> Questo predicato è vero quando PreorderTree è una lista degli archi dell' Mst
                                    ordinata secondo un attraversamento preorder rispetto al peso dell'arco.
                                    A parità di peso utilizza un ordinamento lessicografico dei vertici destinatari.
Questo predicato utilizza i seguenti predicati ausiliari:
call_mst_get(G, T, Source, Vs, PreorderTree) -> Questo predicato chiama per ogni elemento della lista Vs [W, F]
                                                l'mst_get sul vertice F dopo aver creato l'arco arc(T, Source, F, W).
get_tail(X, Xs) -> Questo predicato è vero se Xs è la coda della lista X.
get_first_element(X, Y) -> Questo predicato è vero se Y è il primo elemento ella lista X.
update_arcs(G, Ys, Z) -> Questo predicato prende la lista Ys dove gli elementi sono [A, B]
                         e per ogni vertice B trova il relativo peso che è uguale ad A.
addElement(X, Y, Z) -> Questo predicato è vero se Z è la lista risultante dopo l'aggiunta dell'elemento X alla lista Y.



                       


                       



        




