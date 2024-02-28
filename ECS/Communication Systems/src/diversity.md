---
title: Diversity
author: Federico Casu
date: Novembre 28, 2023
header-includes: | 
    \usepackage{tikz}
    \usetikzlibrary{automata, positioning}
    \usepackage{pgfplots}
---


# Diversity

Come studiato in precedenza, anche se esistono delle tecniche di trasmissione (vedi OFDM) che ci permettono di superare i fenomeni di disturbo, in generale le prestazioni del canale multipath in termini di \texttt{BER} sono più scarse, a parità di SNR, rispetto al modello di canale gaussiano.

Come possiamo aumentare le prestazioni del sistema se il canale di trasmissione è di tipo multipath[^1]?

[^1]: Diamo per scontato che il canale debba essere *flat fading* altrimenti avremo interferenza intersimbolica.

![Esempio di *diversity* nel dominio della frequenza.](figures/diversity/frequency_diversity.png)

Esistono 3 diversi principali tipi di diversity:

1. $\mathsf{Time \text{ } diversity}$. Esempio: un protocollo di trasmissione in cui si trasmette in slot temporali. Si noti che la distanza tra due slot di trasmissione assegnati alla sorgente debbono essere distanti più del *coherence time* del canale: se quest'ultima condizione non fosse verificata allora non si sfrutterebbe il concetto di diversity. 

2. $\mathsf{Frequency \text{ } diversity}$. Esempio: la trasmissione avviene su più canali definiti su intervalli di frequenza non sovrapposti. Qui si deve prestare attenzione alla *coherence bandwidth*.

3. $\mathsf{Spatial \text{ } diversity}$. Esempio: più antenne sullo stesso dispositivo. Le antenne devono essere disposte in modo tale che distano l'una dall'altra più della *coherence distance* del canale.



# Time Diversity: Coding and Interleaving

Nelle trasmissioni wireless che sfruttano il concetto di *time diversity*, lo stesso segnale viene trasmesso in istanti temporali differenti. In alternativa, viene aggiunto un codice di *forward error correction* e il messaggio è diffuso nel tempo mediante la codifica dei bit prima della trasmissione. In questo modo, si evitano i *burst* di errori, semplificando la correzione di quest'ultimi.

## Block Codes

I block codes sono molto semplici: il processo di encoding consiste nell'espandere i $\mathsf{k}$ bits informativi in modo tale che il decoder, in ricezione, possa sfruttare l'espansione per:

1. Trovare gli errori commessi in trasmissione.

2. Correggere gli errori commessi in trasmissione.

Dal punto di vista matematico, i block codes possono essere rappresentati come segue:
$$
    \mathsf{d = u G \text{, } d \in \{0,1\}^{n} \text{, } u \in \{0,1\}^{1 \times k} \text{, } G \in \{0,1\}^{k \times n}}
$$

La matrice $\mathsf{G}$ è detta *generatrice*.

*Esempio*: **Parity Check Code**. L'encoder aggiunge un bit di parità a ogni parola di 7 bit: il bit di parità è `1` se la parola contiene un numero dispari di `1`, `0` se la parola contiene un numero pari di `1`. L'output dell'encoder è una parola di 8 bit che contiene sempre un numero pari di `1`.

La matrice generatrice è la seguente:
$$
    \mathsf{\left[I_{7}, 1_{7} \right]}
$$
Siamo sicuri che la matrice generatrice sia corretta? Dimostriamolo:

1. Supponiamo di avere un parola di 7 bit $\mathsf{u = [u_0, u_1, u_2, u_3, u_4, u_5, u_6]}$.

2. La matrice generatrice è la seguente:
$$
    \mathsf{G} = 
    \begin{bmatrix}
        1 & 0 & 0 & 0 & 0 & 0 & 0 & 1 \\
        0 & 1 & 0 & 0 & 0 & 0 & 0 & 1 \\
        0 & 0 & 1 & 0 & 0 & 0 & 0 & 1 \\
        0 & 0 & 0 & 1 & 0 & 0 & 0 & 1 \\
        0 & 0 & 0 & 0 & 1 & 0 & 0 & 1 \\
        0 & 0 & 0 & 0 & 0 & 1 & 0 & 1 \\
        0 & 0 & 0 & 0 & 0 & 0 & 1 & 1 \\
    \end{bmatrix}
$$

3. Il prodotto $\mathsf{uG}$ è il seguente:
\begin{align*}
    \mathsf{u \cdot G} &= \mathsf{[u_0, u_1, u_2, u_3, u_4, u_5, u_6]}
    \begin{bmatrix}
        1 & 0 & 0 & 0 & 0 & 0 & 0 & 1 \\
        0 & 1 & 0 & 0 & 0 & 0 & 0 & 1 \\
        0 & 0 & 1 & 0 & 0 & 0 & 0 & 1 \\
        0 & 0 & 0 & 1 & 0 & 0 & 0 & 1 \\
        0 & 0 & 0 & 0 & 1 & 0 & 0 & 1 \\
        0 & 0 & 0 & 0 & 0 & 1 & 0 & 1 \\
        0 & 0 & 0 & 0 & 0 & 0 & 1 & 1 \\
    \end{bmatrix} \\
    &= \mathsf{[u_0, u_1, u_2, u_3, u_4, u_5, u_6, \sum_{i=0}^{6}u_i]} 
\end{align*}

4. Se $\mathsf{\sum_{i=0}^{6}u_i}$ è pari allora sappiamo che in $\mathsf{GF(2)}$ la somma sarà nulla ($\mathsf{\sum_{i=0}^{6}u_i = 0}$). Nel caso opposto, la somma sarà non nulla e, conseguentemente, il numero di bit della parola trasmessa diventa pari.

Non tutti gli errori vengono rilevati. Le trasmissioni che introducono un numero pari di errori passano senza destare alcun sospetto perchè il controllo sulla parità è corretto!

Fino ad ora non abbiamo fatto altro che definire un sistema che introduce delle informazioni ridondanti: dove si sfrutta la diversity che tanto abbiamo acclamato? **Data Retransmission**:

1. Il ricevitore invia un `ACK` se la parolaricevuta è corretta, un `NACK` in caso contrario.

2. Dopo aver ricevuto un `NACK`, il trasmettitore ritrasmette la parola associata al `NACK` ricevuto.

Il protocollo appena descritto è detto ARQ (**A**utomated **R**epeated re*Q*uest). Tale protocollo sfrutta la diversità temporale del canale ritrasmettendo i dati dopo un intervallo di tempo $\mathsf{T_{ARQ}}$, un periodo più lungo rispetto al tempo di coerenza del canale $\mathsf{T_C}$. La nuova trasmissione sperimenterà un canale diverso e, si spera, migliore.

Dato un canale di comunicazione con larghezza di banda $\mathsf{B}$, Shannon ha dimostrato che la *capacità del canale* $\mathsf{C}$ può essere calcolata come
$$
    \mathsf{C = B \cdot \log_2(1 + SNR) \quad \text{b/s}}
$$

* Per qualsiasi trasmissione con rate $\mathsf{R < C}$, è possibile trovare un codice di correzione degli errori in modo che la probabilità di errore sia $\mathsf{P_e < \epsilon>}$, con $\mathsf{\epsilon}$ arbitrariamente piccolo.

* Al contrario, se $\mathsf{R > C}$ non è possibile trovare un codice che possa rendere la probabilità di errore arbitrariamente piccola.

Il ragionamento è il seguente: una volta che ho stimato la capacità del canale, adopero un rate di trasmissione che sia minore della capacità del canale e cerco un codice che mi permetta di diminuire la probabilità d'errore fino alla soglia desiderata.

## Decoder: Optimal Strategy

In $\mathsf{GF(2)}$, la distanza tra le parole è calcolata come il numero di bit diversi nelle due stringhe di bit (distanza di Hamming).

* Dopo aver ricevuto la stringa $\mathsf{\hat{x}}$ (lunga $\mathsf{n}$ bits), il decodificatore seleziona la parola $\mathsf{\hat{d}}$ che ha la distanza minima da $\mathsf{\hat{x}}$:
$$
    \mathsf{\hat{d} = argmin(d, \hat{x})}
$$

* L'errore si verifica quando, a causa del rumore, la parola $\mathsf{\hat{x}}$ è più vicina a una parola di codice diversa da quella trasmessa.

* Maggiore è la distanza di Hamming tra le parole di codice, maggiore è la capacità di correzione degli errori di un codice a blocchi.

Le proprietà di correzione e rilevamento di errori di un codice a blocchi dipendono dalla distanza minima di Hamming $\mathsf{d_{min}}$ tra le parole di codice, calcolata come la distanza minima tra tutte le parole di codice di un dato codice.

* Un codice a blocchi con distanza minima $\mathsf{d_{min}}$ può rilevare fino a $\mathsf{d_{min} - 1}$ errori e correggere fino a $\mathsf{\lfloor \frac{d_{min}-1}{2} \rfloor}$ errori.

* I codici in cui la distanza tra le parole è grande sono più robusti contro il rumore e il fading rispetto a codici in cui la distanza è piccola.

Vediamo un intuizione di quest'ultimo concetto:

* Supponiamo di ricevera una parola $\mathsf{\hat{x}}$. Visto che ogni parola del codice che stiamo usando dista almeno $\mathsf{d_{min}}$ da una qualsiasi altra parola del codice, se $\mathsf{\hat{x}}$ contiene $\mathsf{ \leq d_{min} - 1}$ errori allora $\mathsf{\hat{x}}$ non è una parola valida e siamo certi che la trasmissione ha introdotto degli errori. 

* Supponiamo di inviare la parola $\mathsf{d^{(i)}}$. Al ricevitore arriva la parola $\mathsf{\hat{x}}$. In particolare, $\mathsf{d_{H}(d^{(i)}, \hat{x}) > d_{H}(d^{(i+1)}, \hat{x})}$ e di conseguenza il decoder sceglierebbe la parola $\mathsf{d^{(i+1)}}$, commettendo un errore. Ciò è accaduto perchè la parola ricevuta $\mathsf{\hat{x}}$ è affetta da più di $\mathsf{\lfloor \frac{d_{min}-1}{2} \rfloor}$ errori quindi, durante il processo di error correction, il decoder sceglie la parola del codice che risulta essere più vicina alla parola inviata.

\begin{figure}
    \centering
    \begin{tikzpicture}
        % Definizione della retta perpendicolare all'asse x
        \draw[dashed] (0,0) -- (0,4);

        % d^{(i)}
        \filldraw (-4,0) circle (2pt) node[anchor=east] {};
        \draw[dashed] (-4,{-0.25}) node[below] {$\mathsf{d^{(i)}}$};

         % d^{(i+1)}
        \filldraw (4,0) circle (2pt) node[anchor=east] {};
        \draw[dashed] (4,{-0.25}) node[below] {$\mathsf{d^{(i+1)}}$};

        %\hat{x}
        \filldraw (2,0) circle (2pt) node[anchor=east] {};
        \draw[dashed] (2,{-0.25}) node[below] {$\mathsf{\hat{x}}$};

        % Asse x
        \draw[->] (-5,0) -- (5,0) node[right] {$x$};

        % Etichetta x = floor((d_min - 1)/2)
        \draw[dashed] (0,{-0.25}) node[below] {$\mathsf{\lfloor \frac{d_{\text{min}} - 1}{2} \rfloor}$};
    \end{tikzpicture}
    \caption{$\mathsf{\hat{x}}$ non può essere corretto perchè dista più di $\mathsf{\lfloor \frac{d_{\text{min}} - 1}{2} \rfloor}$ dalla parola inviata dal trasmettitore, in questo caso specifico $\mathsf{d^{(i)}}$.}
\end{figure}

Avendo fissato il rapporto $\mathsf{R = \frac{k}{n}}$, $\mathsf{d_{min}}$ è più grande quando $\mathsf{k}$ e $\mathsf{n}$ sono grandi; sfortunatamente, anche la complessità del ricevitore cresce con $\mathsf{k}$.


## Convolutional Codes

Contrariamente ai codici a blocchi in forma sistemica, i codici convoluzionali in generale non sono *sistematici*[^2]. In un codice convoluzionale, \underline{il mittente invia solo i bit di parità}. L'encoder utilizza una finestra scorrevole per calcolare $\mathsf{n > 1}$ bit di parità combinando vari sottoinsiemi di bit nella finestra, in modo che il processo di codifica possa essere visto come una convoluzione in $\mathsf{GF(2)}$. 

[^2]: Un codice è detto *sistematico* se la stringa di bit trasmessa è composta dai bit del messaggio seguiti dai bit di parità.

La differenza più importante rispetto ai codici a blocchi è che ora abbiamo un sistema con memoria. La memoria $\mathsf{L}$ dell'encoder è il numero ingressi di $\mathsf{k}$ bit che influenzano i $\mathsf{n}$ bit in uscita. Di conseguenza, l'uscita dell'encoder dipende sia dall'input corrente $\mathsf{u^{(i)}}$ sia dai $\mathsf{L-1}$ ingressi precedenti $\mathsf{u^{(i-1)}, \dots, u^{(i-L+1)}}$. La risposta impulsiva di ciascuna delle $\mathsf{n}$ convoluzioni è data da un diverso vettore generatore di lunghezza $\mathsf{k \cdot L}$. Essendo il risultato di una convoluzione, i bit codificati non sono organizzati in blocchi, ma costituiscono un flusso continuo di dati.

Un encoder convoluzionale può essere descritto mediante la tripla $\mathsf{(n, k, L)}$. Facciamo un esempio: $\mathsf{(n=2, k=1, L=3)}$.

- $\mathsf{k=1}$: ogni $\mathsf{T}$, l'encoder convoluzionale riceve un bit in input.

- $\mathsf{n=2}$: ogni $\mathsf{T}$, l'encoder convoluzionale produce **due** bit in output.

- $\mathsf{L=3}$: ogni bit prodotto dipende dal bit in ingresso all'istante $\mathsf{i \cdot T}$ e dai due bit immediatamente precedenti $\mathsf{u^{(i-1)} \text{ e } u^{(i-2)}}$.

L'output dell'encoder può essere descritto come il prodotto scalare tra due vettori. Prendiamo in considerazione l'esempio precedente $\mathsf{(n=2, k=1, L=3)}$:
$$
    \mathsf{d^{(i)}_{n} = \sum_{j=0}^{3-1} u^{(i-j)} \cdot g(j)_{n} \text{, } n \in \{0,1\} }
$$
Per capire a fondo il funzionamento dei codici convoluzionali, analizziamo nel dettaglio il seguente esempio:
$$
    \mathsf{(n=2, k=1, L=3) \text{, } g_{1}=[1, 1, 1] \text{, } g_{2}=[1, 0, 1]}
$$

\begin{figure}
    \centering
    \begin{tikzpicture}[->,>=stealth,shorten >=1pt,auto,node distance=3cm,semithick]
    \tikzstyle{every state}=[fill=white,draw=black,text=black,minimum size=25pt]

    \node[state] (10) {10};
    \node[state] (00) [below of=10, xshift=-2.5cm] {00};
    \node[state] (11) [below of=10, xshift=2.5cm] {11};
    \node[state] (01) [below of=00, xshift=2.5cm] {01};

    \path (00) edge [] node {\textcolor{red}{1}/11} (10)
          (10) edge [] node {\textcolor{red}{1}/01} (11)
          (01) edge [] node {\textcolor{red}{0}/11} (00)
          (01) edge [bend left] node {\textcolor{red}{1}/00} (10)
          (10) edge [bend left] node {\textcolor{red}{0}/10} (01)
          (00) edge [loop left] node {\textcolor{red}{0}/00} (00)
          (11) edge [loop right] node {\textcolor{red}{1}/10} (11)
          (11) edge [] node {\textcolor{red}{0}/01} (01);

    \end{tikzpicture}
    \caption{Macchina a stati finiti dell'encoder $\mathsf{(n=2, k=1, L=3)}$.}
\end{figure}

Ad ogni $\mathsf{T}$, l'output dell'encoder dipende dal bit in ingresso e dai due bit precedenti. I due bit appena menzionati rappresentano la memoria (o stato) dell'encoder. Di seguito elenchiamo ogni possibile output in funzione del *1)* bit in ingresso e *2)* lo stato dell'encoder:
\begin{align*}
    \mathsf{u^{(i)} = 0 \text{, } (0,0)} &\rightarrow \mathsf{d_{1}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)} + u^{(i-1)} + u^{(i-2)} = 0+0+0 = 0} \\
    \mathsf{u^{(i)} = 0 \text{, } (0,0)} &\rightarrow \mathsf{d_{2}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)}  + u^{(i-2)} = 0+0 = 0} \\
    \mathsf{u^{(i)} = 1 \text{, } (0,0)} &\rightarrow \mathsf{d_{1}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)} + u^{(i-1)} + u^{(i-2)} = 1+0+0 = 1} \\
    \mathsf{u^{(i)} = 1 \text{, } (0,0)} &\rightarrow \mathsf{d_{2}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)}  + u^{(i-2)} = 1+0 = 1} \\ \\ 
    \mathsf{u^{(i)} = 0 \text{, } (1,0)} &\rightarrow \mathsf{d_{1}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)} + u^{(i-1)} + u^{(i-2)} = 0+1+0 = 1} \\
    \mathsf{u^{(i)} = 0 \text{, } (1,0)} &\rightarrow \mathsf{d_{2}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)}  + u^{(i-2)} = 0+0 = 0} \\
    \mathsf{u^{(i)} = 1 \text{, } (1,0)} &\rightarrow \mathsf{d_{1}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)} + u^{(i-1)} + u^{(i-2)} = 1+1+0 = 0} \\
    \mathsf{u^{(i)} = 1 \text{, } (1,0)} &\rightarrow \mathsf{d_{2}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)}  + u^{(i-2)} = 1+0 = 1} \\ \\
    \mathsf{u^{(i)} = 0 \text{, } (0,1)} &\rightarrow \mathsf{d_{1}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)} + u^{(i-1)} + u^{(i-2)} = 0+0+1 = 1} \\
    \mathsf{u^{(i)} = 0 \text{, } (0,1)} &\rightarrow \mathsf{d_{2}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)}  + u^{(i-2)} = 0+1 = 1} \\
    \mathsf{u^{(i)} = 1 \text{, } (0,1)} &\rightarrow \mathsf{d_{1}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)} + u^{(i-1)} + u^{(i-2)} = 1+0+1 = 0} \\
    \mathsf{u^{(i)} = 1 \text{, } (0,1)} &\rightarrow \mathsf{d_{2}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)}  + u^{(i-2)} = 1+1 = 0} 
\end{align*}
\begin{align*}
    \mathsf{u^{(i)} = 0 \text{, } (1,1)} &\rightarrow \mathsf{d_{1}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)} + u^{(i-1)} + u^{(i-2)} = 0+1+1 = 0} \\
    \mathsf{u^{(i)} = 0 \text{, } (1,1)} &\rightarrow \mathsf{d_{2}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)}  + u^{(i-2)} = 0+1 = 1} \\
    \mathsf{u^{(i)} = 1 \text{, } (1,1)} &\rightarrow \mathsf{d_{1}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)} + u^{(i-1)} + u^{(i-2)} = 1+1+1 = 1} \\
    \mathsf{u^{(i)} = 1 \text{, } (1,1)} &\rightarrow \mathsf{d_{2}^{(i)} = \sum_{j=0}^{2} u^{(i-j)} \cdot g_{1}(j) = u^{(i)}  + u^{(i-2)} = 1+1 = 0}
\end{align*}

Adesso arriva il bello: come avviene la decodifica?

> A differenza dei codici a blocchi, cui ogni blocco poteva essere codificato senza la necessità di conoscere i blocchi precedenti/successivi, i codici convoluzionali posseggono il concetto di memoria: la parola in input all'istante $\mathsf{i}$ partecipa alla codifica delle successive $\mathsf{L-1}$ parole!

Invece, per quanto riguarda la strategia di decisione ottima, sia i codici a blocchi sia i codici convoluzionali adottano lo stesso approccio: decisore a minima distanza (di Hamming).

Supponiamo di ricevere una sequenza di $\mathsf{N}$ parole. Visto che ogni parola è composta da $\mathsf{n}$ bits, ciascuno di questi ottenuto tramite convoluzione, la sequenza di bit ricevuta è lunga $\mathsf{n \cdot N}$. In realtà, i $\mathsf{n \cdot N}$ bits ricevuti sono bit di parità: i bit che effettivamente sono stati trasmessi sono $\mathsf{k \cdot N}$. Da questo deduciamo che possono esistere $\mathsf{2^{k \cdot N}}$ sequenze di bits diverse che possono essere ricevute.

Un decisore a distanza minima sceglie la sequenza di bit $\mathsf{\bar{d}}$, quest'ultima lunga $\mathsf{n \cdot N}$, tale che
$$
    \mathsf{\bar{d} = argmin \left\{ d_{H}(d_i, \hat{x}) \right\} \text{, } i = 1, \dots, 2^{k \cdot N}}
$$
dove $\mathsf{\hat{x}}$ rappresenta la sequenza di bits ricevuta.

Trovare la sequenza di bits $\mathsf{\bar{d}}$ tale che la distanza di Hamming da $\mathsf{\hat{x}}$ sia minima richiede calcolare la distanza di Hamming tra $\mathsf{\hat{x}}$ e tutte le possibili $\mathsf{2^{k \cdot N}}$ sequenze valide! La complessità è esponenziale nel numero di bits inviati/ricevuti!

## Algoritmo di Viterbi

Prima di introdurre l'algoritmo di Viterbi è comodo imparare ad utilizzare i *diagrammi di trellis*. L'evoluzione temporale dell'encoder può essere rappresentata mediante diagramma di trellis. Tale diagramma illustra l'evoluzione degli stati dell'encoder in funzione del tempo. Qualsiasi sequenza di input e la parola codificata corrispondente possono essere rappresentate come un percorso sul diagramma. Ogni nodo del diagramma rappresenta uno stato dell'encoder.

\begin{figure}
    \centering
    \begin{tikzpicture}[>=stealth,auto,node distance=2.25cm,semithick]

    \tikzstyle{every state}=[fill=black,draw=black,text=black,minimum size=0.25pt]

    \node[state,label=left:{00}] (s00) {};
    \node[state,below=of s00,label=left:{10}] (s01) {};
    \node[state,below=of s01,label=left:{01}] (s02) {};
    \node[state,below=of s02,label=left:{11}] (s03) {};

    \node[state,right=of s00] (s10) {};
    \node[state,below=of s10] (s11) {};
    \node[state,below=of s11] (s12) {};
    \node[state,below=of s12] (s13) {};

    \node[state,right=of s10] (s20) {};
    \node[state,below=of s20] (s21) {};
    \node[state,below=of s21] (s22) {};
    \node[state,below=of s22] (s23) {};

    \path[->] 
    (s00) edge [] node {0/00} (s10)
    (s00) edge [] node {1/11} (s11)

    (s10) edge [] node {0/00} (s20)
    (s10) edge [] node {1/11} (s21)

    (s11) edge [] node {0/10} (s22)
    (s11) edge [] node {1/01} (s23);

    \end{tikzpicture}
    \caption{Esempio di diagramma di trellis.}
\end{figure}

Ora possiamo studiare l'idea alla base dell'algoritmo di Viterbi. Il decoder implementa una strategia di decisione a minima distanza. Se la sequenza di bits ricevuta è composta da $\mathsf{N}$ parole di codice, la distanza di Hamming tra una sequenza $\mathsf{d}$ e la sequenza ricevuta $\mathsf{\hat{x}}$ è calcolata come segue:
$$
    \mathsf{d_{H}(d, \hat{x}) = \sum_{i=1}^{N} d_{H}(d^{(i)}, \hat{x}_i)}
$$
ovvero la distanza di Hamming è calcolata come la somma delle distanze di hamming tra la parola ricevuta al signaling time $\mathsf{i}$ e la parola $\mathsf{d_i}$ ottenuta valutando l'evoluzione temporale della macchina a stati dell'encoder.

Analizziamo la distanza di Hamming tra una sequenza $\mathsf{d}$ e la sequenza $\mathsf{\hat{x}}$ ad un certo punto della trasmissione $\mathsf{k < N}$.
$$
    \mathsf{d_{H}(d, \hat{x})|_{k} = \sum_{i=1}^{k} d_{H}(d^{(i)}, \hat{x}_i)}
$$
Supponiamo di avere due sequenze valide, $\mathsf{d_{i}}$ e $\mathsf{d_{j}}$, e quest'ultimi al signaling time $\mathsf{k}$ convergono nello stato $\mathsf{10}$. La distanza di Hamming accumulata fino ad ora dalle due sequenze può essere calcolata come:
$$
    \mathsf{\Lambda_{k}(d_i) = d_{H}(d_i, \hat{x})|_{k} = \sum_{m=1}^{k} d_{H}(d_i^{(m)}, \hat{x}_m)}
$$
$$
    \mathsf{\Lambda_{k}(d_j) = d_{H}(d_j, \hat{x})|_{k} = \sum_{m=1}^{k} d_{H}(d_j^{(m)}, \hat{x}_m)}
$$

Dallo stato $\mathsf{10}$ si prosegue verso lo stato $\mathsf{01}$. La distanza di Hamming tra una sequenza valida e $\mathsf{\hat{x}}$ al signaling time $\mathsf{k+1}$ può assumere due valori differenti (ricorda che ci sono due percosi che convergono nello stato $\mathsf{10}$ al signaling time $\mathsf{k}$):
$$
    \mathsf{\Lambda_{k+1}(d) = \Lambda_{k}(d_i) + d_{H}(d^{(k+1)}, \hat{x}_{k+1})}
$$
$$
    \mathsf{\Lambda_{k+1}(d) = \Lambda_{k}(d_j) + d_{H}(d^{(k+1)}, \hat{x}_{k+1})}
$$

Il decoder sceglierà il percorso che, appunto, minimizza la distanza di Hamming. Visto che la distanza totale associata ai due percorsi ha una componente uguale per entrambi (ovvero la distanza di Hamming a partire da $\mathsf{k+1}$), possiamo \underline{scartare il percorso con distanza maggiore} visto che comunque verrà scartato dal decoder!

\begin{figure}
    \centering
    \begin{tikzpicture}[>=stealth,auto,node distance=2.5cm,semithick]

    \tikzstyle{every state}=[fill=black,draw=black,text=black,minimum size=0.25pt]

    \node[state,label=left:{10}] (s01) {};
    \node[state,below=of s01,label=left:{01}] (s02) {};

    \node[state,right=of s01,label=above right:{$\mathsf{\Lambda_{k}(d_i),\Lambda_{k}(d_j)}$}] (s11) {};
    \node[state,below=of s11] (s12) {};

    \node[state,right=of s10] (s20) {};
    \node[state,below=of s20,label=above right:{$\mathsf{\Lambda_{k+1}(d)}$}] (s21) {};

    \path[->] 
    (s01) edge [] node {$\mathsf{d_{H}(d_i^{(k)}, \hat{x}_{k})}$} (s11)
    (s02) edge [] node {$\mathsf{d_{H}(d_j^{(k)}, \hat{x}_{k})}$} (s11)

    (s11) edge [] node {$\mathsf{d_{H}(d^{(k+1)}, \hat{x}_{k+1})}$} (s21);

    \end{tikzpicture}
    \caption{Algoritmo di Viterbi. Esempio pratico.}
\end{figure}

In generale, assumiamo che ogni parola di codice inizi dallo stato $\mathsf{0}$ e termini nello stato $\mathsf{0}$, in modo che tutti i percorsi abbiano origine dallo stato $\mathsf{0}$ e alla fine convergano nello stato $\mathsf{0}$. Di conseguenza, il percorso soppravissuto che porta allo stato $\mathsf{0}$ rappresenta l'output dell'algoritmo e la metrica cumulata finale $\mathsf{\Lambda}$ è il numero di errori corretti. La complessità dell'algoritmo di Viterbi è lineare rispetto a $\mathsf{N}$, non esponenziale!

## Interleaving

I codici convoluzionali sono principalmente adatti per canali *memoryless* con eventi di errore distribuiti in modo uniforme e non correlati. I canali con fading tendono a causare burst di errori. L'interleaving fa sì che il canale sembri un canale memoryless al decoder e tende a decorrelare gli eventi di errore.

![Diagramma a blocchi di un sistema $\mathsf{Tx\text{-}Rx}$ che adopera il meccanismo di interleaving.](figures/diversity/interleaving_chain.png)

L'interleaving viene realizzato diffondendo i simboli codificati nel tempo o nella frequenza prima della trasmissione. In ricezione, si esegue l'operazione inversa (deinterleaving) della sequenza ricevuta. L'interleaving fa sì che gli errori (quest'ultimi molto ravvicinati tra loro a causa del canale fortemente fading in quel punto) sembrino casuali, consentendo così ai codici convoluzionali di ottenere prestazioni migliori. Il costo dell'interleaving è la *latenza*: sia al trasmettitore che al ricevitore è necessario avere l'intero blocco di dati per avviare il processo di codifica/decodifica. C'è un compromesso: maggiore è la profondità dell'interleaver $\mathsf{K}$, maggiormente gli errori sono decorrelati, ma al contempo crescono la latenza e il ritardo.
