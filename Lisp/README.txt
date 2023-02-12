Componenti del gruppo:
1) Balzarotti Niccolò 852003
2) Covelli Matteo 861277
3) Ghanimi Alaaeddine 856573

Implementazione in lisp dell'algoritmo di Prim per la ricerca del minimum
spanning tree in un grafo non diretto con archi etichettati indicanti il 
peso

Il file principale è "mst.lisp"
Per effettuare l'esecuzione dell'algoritmo bisogna lanciare in ordine 
(mst-prim graph-id source)
e successivamente, sempre per lo stesso grafo
(mst-get graph-id source)

Per l'algoritmo di Prim sono state implementate le seguenti hashtables:
*vertices* -> hashtable che contiene i nodi del grafo
*arcs* -> hashtable che contiene gli archi del grafo
*graphs* -> hashtable che contiene i grafi
*vertex-keys* -> hashtable che contiene l'associazione (graph-id V) -> K , dove 
		 K è la chiave del nodo
*vertex-previous* -> hashtable che contiene l'associazione (graph-id V) -> U ,
		     dove U è il genitore di V
*heaps* -> hashtable che contiene gli heap
*positions* -> hashtable che contiene le posizioni degli elementi dell'heap 
	       contenuto in *heaps*

****GRAFO****

is-graph graph-id -> graph-id or nil
		     questa funzione ritorna il graph-id stesso se il grafo è 
		     già stato creato, nil altrimenti.

new-graph graph-id -> graph-id
	  Creazione del grafo G nella base di dati.

delete-graph graph-id -> nil
	     Rimuove sia il grafo, i vertici e gli archi
             dalla base di dati.

new-vertex graph-id vertex-id -> vertex-rep
	   Crea il vertice vertex-id associato al grafo graph-id nella 
	   hashtable *vertices*.

graph-vertices graph-id -> vertex-rep-list
	       Ritorna la lista dei vertici del grafo.

new-arc graph-id vertex-id-1 vertex-id-2 &optional weight -> arc-rep
	Ritorna una lista composta nel seguente modo
	(arc graph-id vertex-id-1 vertex-id-2 weight)
	Se l'arc-rep esiste, ma il peso è diverso allora viene sovrascritto.

update-arc graph-id vertex-id-1 vertex-id-2 weight -> nil
	   Sovrascrive il peso se il vertice 1 e il vertice 2 sono uguali, ma 
	   il peso è diverso.

graph-arcs graph-id -> arc-rep-list
	   Ritorna una lista di tutti gli archi associati a graph-id.

graph-vertex-neighbors graph-id vertex-id -> arc-rep-list
		       Ritorna una lista contenente gli archi
		       (arc graph-id vertex-id N W)
		       che portano ai vertici N, immediatamente raggiungibili da vertex-id.

graph-vertex-adiacent graph-id vertex-id -> vertex-rep-list
		      Ritorna una lista contenente i vertici
		      (arc graph-id vertex-id V)
		      adiacenti a vertex-id.

graph-print graph-id
Output di una lista dei vertici e degli archi del grafo graph-id sul listener.


**** HEAP ****


new-heap heap-id &optional capacity -> heap-rep
	 Inserisce un nuovo heap nella hash-table *heaps*.

heap-delete heap-id -> T
	    Rimuove tutto lo heap indicizzato da heap-id.

heap-size heap-id -> capacity
	  Restituisce la dimensione dello heap.

heap-actual-heap heap-id
		 Restituisce l'array corrente.

heap-id heap-id -> boolean
	Restituisce true se lo heap esiste, nil altrimenti.

heap-empty heap-id -> boolean
	   Questo predicato è vero quando lo heap heap-id non contiene elementi.

heap-not-empty heap-id -> boolean
	       Questo predicato è vero quando lo heap heap-id contiene almeno
	       un elemento.

heap-print heap-id -> boolean
	   Stampa sul listener lo stato interno dello heap heap-id.


heap-head heap-id -> (K V)
	  Restituisce una lista di 2 elementi, dove K è la chiave minima
	  e V il valore associato.

heapify heap-id i -> boolean
	La funzione sistema lo heap partendo dalla posizione i, fino alle foglie
	fino a quando la proprietà di min_heap è rispettata.
	La funzione può essere utilizzata solo se il figlio destro 
	e il figlio sinistro di i, sono min_heap.

	Questa funzione utilizza le seguenti funzioni ausiliari:

	parent I
	Restituisce la posizione del padre di i.

	left I
	Restituisce la posizione del figlio sinistro di i.

	right I
	Restituisce la posizione del figlio destro di i.

	get-val heap-id POS
	Restituisce il valore del nodo in posizione pos dello heap heap-id.



heap-insert heap-id K V -> boolean
	    Inserisce l'elemento V nello heap con chiave K.

	    Questa funzione utilizza le seguenti funzioni ausiliari:

	    heap-modify-key heap-id new-key old-key V -> boolean
	    		    Sostituisce la chiave old-key associata al valore V con new-key.

			    Questa funzione utilizza le seguenti funzioni ausiliari:

			    check-parent heap-id I -> boolean
			    Sistema nello heap heap-id l'elemento in posizione I,
			    fino a quando non è rispettata la proprietà di min-heap

			    Questa funzione utilizza le seguenti funzioni ausiliari:
			  
			    swap heap-id I J -> boolean
			    Scambia gli elementi nelle posizioni I e J nello heap heap-id

	    heap-increase-key heap-id I K V -> boolean
	    		      Sistema la proprietà min-heap se il nodo padre è più grande del 
	    		      nodo passato come parametro.

			      Questa funzione utilizza le seguenti funzioni ausiliari:

			      check-parent




heap-extract heap-id -> (K V)
	     Ritorna la lista con K V e con K minima.
	     La coppia è rimossa dallo heap heap-id.

	     Questa funzione utilizza le seguenti funzioni ausiliari:

	     heapify



**** MST ****

initialization-vertices graph-id heap-id Source V -> boolean
			Inserisce nella base di dati *vertex-keys* per ogni
			V appartenente al grafo G e *vertex-previous* per ogni V
			appartenene al grafo G.
			Tutti i padri vengono settati a nil e 
			tutte le chiavi a MOST-POSITIVE-DOUBLE-FLOAT


initialization graph-id Source -> boolean
	       Inizializza i vertex key di ogni nodo
	       mettendo come chiave K 'inf' e come vertex_previous
	       setta tutti i genitori a nil.

	       Questa funzione utilizza le seguenti funzioni ausiliari:

	       initialization-vertices


update graph-id heap-id V neighbors -> boolean
       Attraverso la lista neighbors contenente i neighbors del vertice V,
       aggiorna la chiave presente nell' heap se il peso dell'arco è minore
       o uguale alla chiave presente nell'heap.


mst-vertex-key graph-id vertex-id -> K
	       Dato un vertex-id di un grafo graph-id ritorna durante e dopo
	       l'esecuzione dell'algoritmo di Prim il peso minimo di un arco
	       che connette vertex-id nell'albero minimo. Se questo arco non
	       esiste, allora K è MOST-POSITIVE-DOUBLE-FLOAT.

mst-vertex-previous graph-id V -> K
		    Ritorna il vertice K che è il vertice genitore (precedente o parent) di
		    V, nel minimum spanning tree V.


mst-prim graph-id Source -> nil
	 Questa funzione dopo la sua esecuzione, la hash-table *vertex-keys*
	 contiene al suo interno le associazioni
	 (graph-id V) con la chiave dell'arco entrante dell'albero,
	 per ogni V appartenente a graph-id.
	 La hash-table *vertex-previous* contiene le associazioni
	 (graph-id V) con il padre, calcolate durante l'esecuzione
	 dell'algoritmo di Prim.

	 Questa funzione utilizza le seguenti funzioni ausiliari:

	 initialization



mst-get graph-id Source -> preorder-mst
	Questa funzione ritorna un preorder-mst che è una lista degli
	archi del mst ordinata secondo un attraversamento preorder dello
	stesso, fatta rispetto al peso dell'arco.


	Questa funzione utilizza le seguenti funzioni ausiliari:

	call-mst-get graph-id Source list -> list
		     Questa funzione chiama per ogni elemento della lista
		     l'mst-get sul vertice e esegue l'unione delle liste.

	sort-arcs original-predicate next-predicate
		  Esegue l'ordinamento degli archi in base al peso e a parità
		  di peso ordina in modo lessicografico i nodi di arrivo.


	weight arc-1 arc-2 -> boolean
	       Ritorna true se il peso di arc-1 è minore di arc-2.


	adj arc-1 arc-2 -> boolean
	    Controlla secondo l'ordine lessicografico dei vertici di arrivo
	    presenti in arc-1 e arc-2.
	    Ritorna true se arc-1 viene prima di arc-2,
	    nil altrimenti.






























