---
title: Multi-carrier signals
author: Federico Casu
date: Novembre 21, 2023
header-includes: | 
	\usepackage{subcaption}
	\usepackage{tikz}
	\usepackage{tcolorbox}

    \usetikzlibrary{calc}
    \usetikzlibrary{shapes, arrows, positioning}
---



# OFDM

Andiamo a studiare come far fronte ai problemi che abbiamo introdotto quando abbiamo parlato del modello di canale *multipath*. Prendiamo in considerazione il seguente modello di canale:
$$
    \mathsf{h(t) = A_{LS} \sum_{i=0}^{N_{c} - 1} a_{i} e^{j \phi_{i}} \delta(t - \tau_{i})}
$$
Come abbiamo studiato in precedenza, le varie repliche del segnale generate da fenomeni come *riflessione*, *riffrazione* e *scattering* introducono **interferenza intersimbolica**. 

In particolare, se la banda del segnale $\mathsf{B_{s}}$ è comparabile o maggiore della *coherence bandwidth* $\mathsf{B_{c}}$, il segnale ricevuto sarà affetto da ISI. Allo stesso modo, se il *delay spread* $\mathsf{\sigma_{s}}$ è comparabile o maggiore del tempo di simbolo $\mathsf{T_{s}}$, il segnale ricevuto sarà affetto da ISI poiché varie repliche del segnale, che trasportano il simbolo inviato all'istante $\mathsf{kT_{s}}$, arriveranno in ritardo sovrapponendosi con gli altri simboli inviati.

L'idea alla base dei sistemi di comunicazione che andremo a studiare è di *dividere* la banda (quindi il canale) a disposizione del servizio in sottobande più piccole e trasmettere non più un solo segnale ma utilizzare i sottocanali per trasmettere diversi segnali in parallelo. In particolare, nei multi-carrier modulation system la banda allocata al servizio è suddivisa nel seguente modo:
$$
    \mathsf{B_{s}' = \frac{B_{s}}{N}} \text{, } \mathsf{N \text{ tale che } B_{s}' < B_{c}}
$$
Seppur la banda originariamente allocata al servizio era maggiore della *coherence bandwidth* del canale, ora ogni sottocanale può essere considerato *flat fading*.

Vediamo come funzione la trasmissione:

1. Consideriamo un modello equivalente di canale. In particolare, se il sistema di comunicazione in esame trasmette un segnale (continuo, generato mediante interpolazione) cui tempo di simbolo è $\mathsf{T_{s}}$, possiamo rappresentare il canale mediante il seguente modello equivalente:
$$
    \mathsf{h_{eq}(t) = \sum_{k=0}^{L-1}h(t)\delta(t - kT_{s})}
$$
Si noti la seguente uguaglianza:
$$
    \mathsf{h_{eq}(t) = \sum_{k=0}^{L-1}h(t)\delta(t - kT_{s}) = h(t) = A_{LS} \sum_{i=0}^{N_{c} - 1} a_{i} e^{j \phi_{i}} \delta(t - \tau_{i})}
$$

2. Supponiamo di voler trasmettere un blocco di $\mathsf{N}$ simboli $\mathsf{[s(0), ..., s(N-1)]}$. Studiamo il segnale $\mathsf{y(t)}$ all'entrata del ricevitore (per semplicità omettiamo il rumore):
$$
    \mathsf{y(t) = \sum_{k=0}^{L-1}h(t)\delta(t - kT_{s}) * s(t) = \sum_{k=0}^{L-1}h(t)s(t - kT_{s})}
$$ 
I campioni così ottenuti in seguito all'azione del campionatore sono i seguenti:
$$
    \mathsf{y(k) = y(t)|_{t = kT_s} = \sum_{h=0}^{L-1}h(kT_s)s(kT_s - hT_{s}) = \sum_{h=0}^{L-1}h(k)s(k - h)}
$$

3. Possiamo rappresentare il vettore composto dai campioni $\mathsf{y(k)}$ mediante un prodotto tra matrice-vettore:
$$
    \mathsf{y = \mathcal{H}s}
$$
dove 
\begin{align*}
    \mathsf{y^{T}} &= \mathsf{[y(0), y(1), ..., y(N-1)]} \\ \\
    \mathsf{s^{T}} &= \mathsf{[s(0), s(1), ..., s(N-1)]} \\ \\
    \mathcal{H} &= 
    \begin{vmatrix}
        \mathsf{h(0)} & \mathsf{0}    & ...           & \mathsf{0}   & \mathsf{0} \\
        \mathsf{h(1)} & \mathsf{h(0)} & ...           & \mathsf{0}   & \mathsf{0} \\
        \vdots        & \vdots        & \vdots        & \vdots       & \vdots \\
        \mathsf{h(L-1)} & \mathsf{h(L-2)} & ...       & ...          & \mathsf{0} \\
        \vdots        & \vdots        & \vdots        & \vdots       & \vdots \\
        0             & ...           & \mathsf{h(L-1)} & ...        & \mathsf{h(0)} \\
    \end{vmatrix}
\end{align*}

4. Seppur la matrice $\mathcal{H}$ abbia una proprietà interessante (è una matrice di *Toepliz*), non abbiamo ancora risolto il problema dell'ISI. Supponiamo di voler espandere il blocco $\mathsf{[s(0), s(1), ..., s(N-1)]}$ con un prefisso composto da $\mathsf{N_{CP} > L}$ simboli:
\begin{align*}
    \mathsf{\bar{s}^{T}} &= \mathsf{[s_{CP}, s]} \\
    &= \mathsf{[s(N-N_{CP}-1), s(N-N_{CP}-2), ... , s(N-1), s(0), s(1), ..., s(N-1)]}
\end{align*}
Come visto sopra, i simboli sono gli ultimi $\mathsf{N_{CP}}$ elementi del vettore $\mathsf{s}$. Per questo motivo, l'espansione applicata al vettore dei simboli è detta *estensione ciclica*.

5. Ora, in seguito all'introduzione del prefisso, la matrice $\bar{\mathcal{H}}$ che rappresenta il canale è la seguente:
\begin{align*}
    \mathsf{y^{T}} &= \mathsf{[y(0), y(1), ..., y(N-1)]} \\ \\
    \mathsf{s^{T}} &= \mathsf{[s(0), s(1), ..., s(N-1)]} \\ \\
    \bar{\mathcal{H}} &= 
    \begin{vmatrix}
        \mathsf{h(0)} & \mathsf{0}    & ...           & \mathsf{h(2)}   & \mathsf{h(1)} \\
        \mathsf{h(1)} & \mathsf{h(0)} & ...           & \mathsf{h(3)}   & \mathsf{h(2)} \\
        \vdots        & \vdots        & \vdots        & \vdots       & \vdots \\
        \mathsf{h(L-1)} & \mathsf{h(L-2)} & ...       & ...          & \mathsf{0} \\
        \vdots        & \vdots        & \vdots        & \vdots       & \vdots \\
        0             & ...           & \mathsf{h(L-1)} & ...        & \mathsf{h(0)} \\
    \end{vmatrix}
\end{align*}
Ora $\bar{\mathcal{H}}$ è una matrice *circolare*! Le matrici circolari hanno una proprietà molto interessante: sono *diagonalizzabili*.
$$
    \bar{\mathcal{H}} = \mathsf{F^{H} H F}
$$
dove $\mathsf{F^{H}F = FF^{H} = I}$ e  $\mathsf{H = diag(h_{0}, ..., h_{N-1})}$. Siano $\mathsf{Y = Fy}$ e $\mathsf{S = Fs}$ le traformate discrete di Fourier dei vettori $\mathsf{y}$ e $\mathsf{s}$. Premoltiplichiamo ambo i membri di $\mathsf{y = \bar{\mathcal{H}}s}$ per la matrice $\mathsf{F}$:
\begin{align*}
    \mathsf{Fy} &= \mathsf{F} \bar{\mathcal{H}} \mathsf{s} \\
    \mathsf{Y} &= \mathsf{F F^{H} H F s} \\
    \mathsf{Y} &= \mathsf{I H S} \\
    \mathsf{Y} &= \mathsf{H S}
\end{align*}
Visto che $\mathsf{H}$ è diagonale, abbiamo eliminato l'interferenza intersimbolica in frequenza!

Ora ragioniamo su come è implementato un sistema di comunicazione OFDM. In particolare vogliamo studiare quali sono i blocchi logici che compongono la catena $\mathsf{Tx-Rx}$:

\begin{tikzpicture}[auto, node distance=1cm,>=latex']

% Blocks
\node [draw, rectangle] (source) {Source};
\node [draw, rectangle, right=of source] (map) {\texttt{Map}};
\node [draw, rectangle, right=of map] (serial) {Serial/Parallel};
\node [draw, rectangle, right=of serial] (idft) {IDFT};
\node [draw, rectangle, right=of idft] (cpins) {Cyclic Prefix Ins.};
\node [draw, rectangle, below=of cpins] (channel) {Multipath Channel};
\node [draw, rectangle, below=of channel] (cprem) {Cyclic Prefix Rem.};
\node [draw, rectangle, left=of cprem] (parallel) {Parallel/Serial};
\node [draw, rectangle, left=of parallel] (dft) {DFT};
\node [draw, rectangle, left=of dft] (dec) {Dec};
\node [draw, rectangle, left=of dec] (rec) {Receiver};

\node [above=0.125cm] at ($(source)!0.5!(map)$) {$\mathsf{T_{b}}$};
\node [above=0.125cm] at ($(map)!0.35!(serial)$) {$\mathsf{T_{s}}$};
\node [above=0.3cm] at ($(serial)!0.65!(idft)$) {$\mathsf{T'=NT_{s}}$};

% Arrows
\draw [->] (source) -- (map);
\draw [->] (map) -- (serial);
\draw [->] (serial) -- (idft);
\draw [->] (idft) -- (cpins);
\draw [->] (cpins) -- (channel);
\draw [->] (channel) -- (cprem);
\draw [->] (cprem) -- (parallel);
\draw [->] (parallel) -- (dft);
\draw [->] (dft) -- (dec);
\draw [->] (dec) -- (rec);

\end{tikzpicture}


## OFDM bandwidth

Per studiare la banda occupata dal segnale OFDM è interessante studiare l'aspetto dei segnali (discreti o analogici) all'uscita di ciascun blocco che compone il trasmettitore.

\begin{tikzpicture}[auto, node distance=1cm,>=latex']

% Blocks
\node [draw, rectangle] (source) {\texttt{Source}};
\node [draw, rectangle, right=of source] (map) {\texttt{Map}};
\node [draw, rectangle, right=of map] (serial) {\texttt{Serial/Parallel}};
\node [draw, rectangle, right=of serial] (idft) {\texttt{IDFT}};
\node [draw, rectangle, right=of idft] (cpins) {\texttt{C.P.} \texttt{Ins.}};

\node [above=0.125cm] at ($(source)!0.5!(map)$) {$\mathsf{b_{i}}$};
\node [above=0.125cm] at ($(map)!0.35!(serial)$) {$\mathsf{S(n)}$};
\node [above=0.125cm] at ($(serial)!0.65!(idft)$) {$\mathsf{S}$};
\node [above=0.125cm] at ($(idft)!0.35!(cpins)$) {$\mathsf{s}$};

% Arrows
\draw [->] (source) -- (map);
\draw [->] (map) -- (serial);
\draw [->] (serial) -- (idft);
\draw [->] (idft) -- (cpins);

\end{tikzpicture}

Si noti che i simboli $\mathsf{S(n)}$ sono rappresentati nel dominio della frequenza. Ricorda: grazie al prefisso circolare, possiamo garantire che, in frequenza, il campione $\mathsf{Y(n)}$ dipende solo dal simbolo $\mathsf{S(n)}$!

Studiamo il blocco di simboli $\mathsf{s}$:
$$
    \mathsf{s^{T} = [s(0), s(1), ..., s(N-1)] = F^{H} S}
$$
$$
    \mathsf{s(n) = \frac{1}{\sqrt{N}} \sum_{k=0}^{N-1} S(k) \cdot exp\left(\frac{j 2 \pi kn}{N} \right) \text{, } n = 0, ..., N-1}
$$

Vogliamo studiare un singolo addendo della somma:
$$
    \mathsf{S(k) \cdot exp \left(\frac{j 2 \pi kn}{N} \right)}
$$
Proviamo a moltiplicare l'esponente di Fourier per $\mathsf{B_{s}T_{s} = 1}$:
$$
    \mathsf{S(k) \cdot exp \left(\frac{j 2 \pi kn B_{s} T_{s}}{N} \right) = S(k) \cdot exp \left(j 2 \pi \frac{B_{s}}{N} k n T_{s} \right) = S(k) \cdot exp(j 2 \pi \Delta f k t)|_{t = n T_{s}}}
$$
Ogni simbolo $\mathsf{S(k)}$ è moltiplicato per un esponenziale complesso che oscilla alla frequenza $\mathsf{\Delta f k}$. Qual'è la durata del simbolo? In un sistema OFDM il tempo di simbolo è dato dalla somma $\mathsf{T_{s}(N + N_{CP})}$: $\mathsf{N}$ campioni da trasmettere più il prefisso ciclico ($\mathsf{N_{CP}}$).

In altre parole, ogni simbolo $\mathsf{S(k)}$ viene trasmesso ad una frequenza $\mathsf{\Delta f k}$ (effetto della moltiplicazione per l'esponenziale complesso) e il segnale trasmesso alla frequenza $\mathsf{\Delta f k}$ rimane costante per $\mathsf{NT_{s}}$: questo è equivalente a moltiplicare l'esponenziale complesso per una $\mathsf{rect}$ di durata $\mathsf{NT_{s}}$ secondi.

Il segnale trasmesso sulla $\mathsf{k}$-esima *sub-carrier* è il seguente:
$$
    \mathsf{s_{k}(t) = \frac{S(k)}{\sqrt{N}} \cdot exp(j 2 \pi \Delta f k t) \cdot rect \left( \frac{t}{NT_{s}}\right)}
$$

> The power spectral density of the OFDM signal is the sum of N $\mathsf{sinc}$ functions, one for each subcarrier. All the $\mathsf{sinc}$ functions are orthogonal by construction and they do not interfere with each other. The overall signal bandwidth can be approximated as:
$$
    \mathsf{B_{OFDM} \approx N \cdot \frac{1}{NT_{S}} = \frac{1}{T_{S}}}
$$