---
title: Electronics Systems
author: Federico Casu
date: Novembre 23, 2023
header-includes: | 
        \usepackage{subcaption}
        \usepackage{pgfplots}
        \usepackage{array}
        \usepackage{tikz-timing}
        \usepackage{circuitikz}
        \usetikzlibrary{positioning}
---


# Digital Circuits Implementation Approaches

Un circuito integrato *special purpose*, noto come $\mathsf{ASIC}$ o **A**pplication **S**pecific **I**ntegrated **C**ircuit, è un circuito integrato creato appositamente per risolvere un'elaborazione ben precisa (special purpose, appunto): la specificità della progettazione, focalizzata sulla risoluzione di un unico problema, consente di raggiungere delle prestazioni in termini di velocità di elaborazione e consumo difficilmente ottenibili con l'uso di soluzioni più generiche (general purpose). 

Non è tutto oro quello che luccica. Quanto costa produrre un chip $\mathsf{ASIC}$? Prima di tutto dobbiamo introdurre una distinzione. Esistono due tipologie di approcci: *custom* e *semi-custom*.

1. L'approccio *custom* permette di avere il totale controllo sulla progettazione del chip: dalla dimensione dei transistor al posizionamento di quest'ultimi all'interno del chip. Tale controllo (o libertà di progettazione) consente di ottenere i risultati migliori in termini di prestazioni, area e consumo di energia. Ovviamente dobbiamo considerare il rovescio della medaglia: progettare e produrre un chip da zero ha i suoi costi.

2. L'approccio *semi-custom* si basa su design già esistenti, utilizzando una piattaforma predefinita con una struttura logica fissa. Durante la fase di progettazione, si definiscono le connessioni tra le strutture logiche messe a dispozione dalla piattaforma in uso. Il costo di sviluppo, così come il tempo necessario, per un chip semi-custom è **inferiore** rispetto a quello di un chip full custom. Questo perché il processo di ingegnerizzazione di un chip semi-custom è più rapido e meno complesso rispetto al caso full-custom.

\begin{figure}
    \centering
    \begin{tikzpicture}
        \begin{axis}[
            xlabel={$\mathsf{n}$},
            ylabel={$\mathsf{Cost = \frac{NRE}{n} + RE}$},
            xmin=0, xmax=50, 
            ymin=0, ymax=10,
            axis lines=left,
            grid=major,
            legend pos=north east,
        ]
        
        % Full custom
        \addplot[blue, domain=1:50, samples=5000] {40/x + 1};
        \addlegendentry{$\mathsf{Full \text{ } Custom}$}

        % Semi-Custom
        \addplot[red, domain=1:50, samples=5000] {20/x + 2};
        \addlegendentry{$\mathsf{Semi \text{-} Custom}$}
        
        \end{axis}
    \end{tikzpicture}
    \caption{Grafico della curva di costo al variare del numero di chip prodotti.}
    \label{fig:cost-curve}
\end{figure}

\begin{figure}
    \centering
    \begin{tikzpicture}
        \begin{axis}[
            xlabel={$\mathsf{n}$},
            ylabel={$\mathsf{Cost = \frac{NRE}{n} + RE}$},
            xmin=0, xmax=50, 
            ymin=0, ymax=10,
            axis lines=left,
            grid=major,
            legend pos=north east,
        ]
        
        % Full custom
        \addplot[blue, domain=1:50, samples=5000] {40/x + 1};
        \addlegendentry{$\mathsf{Full \text{ } Custom}$}

        % Standard Cell
        \addplot[red, domain=1:50, samples=5000] {20/x + 2};
        \addlegendentry{$\mathsf{Semi \text{-} Custom}$}

        % FPGA
        \addplot[gray, domain=1:50, samples=5000] {10/x + 3};
        \addlegendentry{$\mathsf{FPGA}$}
        
        \end{axis}
    \end{tikzpicture}
    \caption{Grafico della curva di costo al variare del numero di chip prodotti: full-custom, semi-custom e FPGA.}
    \label{fig:cost-curve-zoom}
\end{figure}



\newpage{}



# Static and Dynamic Power in CMOS Inverters




# Timing issues

\begin{circuitikz}
  % Flip-flop DF84
  \node [flipflop D] (DF84) {\texttt{DF84}};

  % Flip-flop DF8 below DF84
  \node [flipflop D, below=of DF84, yshift=-0.25cm] (DF8_) {\texttt{DF8}};

  % Exclusive NOR gate EN1
  \node [xnor port, right=of DF84.pin 6] (EN1) {\texttt{EN1}};

  % NOR gate NO2
  \node [nor port, right=of EN1, yshift=-0.65cm] (NO2) {\texttt{NO2}};

  % OR gate
  \node [or port, right=of DF8_.pin 6] (OR) {\texttt{OR}};

  % NAND gate
  \node [nand port, right=of OR, yshift=-1cm] (NAND) {\texttt{NAND}};

  % Flip-flop DF8
  \node [flipflop D, right=of NO2] (DF8) {\texttt{DF8}};

  % Flip-flop DFA below DF84
  \node [flipflop D, below=of DF8, yshift=-0.25cm] (DFA) {\texttt{DFA}};

  % Input labels
  \node [above=0.00125cm of EN1.in 1] {\texttt{A}};
  \node [above=0.00125cm of EN1.in 2] {\texttt{B}};
  \node [above=0.00125cm of NO2.in 1] {\texttt{A}};
  \node [above=0.00125cm of NO2.in 2] {\texttt{B}};
  \node [above=0.00125cm of OR.in 1] {\texttt{A}};
  \node [above=0.00125cm of OR.in 2] {\texttt{B}};
  \node [above=0.00125cm of NAND.in 2] {\texttt{C}};

  % Connections
  \draw (DF84.pin 6) -- ++(0.5,0) |- (EN1.in 1);
  \draw (DF84.pin 6) -- ++(0.5,0) |- (OR.in 1);
  \draw (EN1.out) -- ++(0.5,0) |- (NO2.in 1);
  \draw (NO2.out) -- ++(0.5,0) |- (DF8.pin 1);
  \draw (DF8_.pin 6) -- ++(0.5,0) |- (OR.in 2);
  \draw (DF8_.pin 4) -- ++(0.5,0) |- (NAND.in 2);
  \draw (OR.out) -- ++(0.5,0) |- (NAND.in 1);
  \draw (NAND.out) -- ++(0.5,0) |- (DFA.pin 1);
  \draw (DFA.pin 6) -- ++(0.25,0) |- ++(0,1) -| (NO2.in 2);
  \draw (DFA.pin 6) -- ++(0.25,0) |- ++(0,1) -| (EN1.in 2);
\end{circuitikz}


## Clock Skew

Fino ad ora abbiamo considerato che il fronte di salita del clock arrivi a tutti i registri del circuito allo stesso istante $\mathsf{t}$. In realtà questa è solamente un ipotesi:

* Supponiamo di prendere come riferimento il segnale (onda quadra) in uscita dal generatore di clock.

* Il fronte di salita del clock visto da un registro $\mathsf{i}$ può essere in anticipo/ritardo rispetto al fronte di salita visto dal registro $\mathsf{j}$. Tale scostamento dipende dalla distanza tra il generatore di clock e il registro. Esempio: se il registro $\mathsf{i}$ si trova più "vicino" al generatore rispetto al registro $\mathsf{j}$ allora il fronte di salita visto dal registro $\mathsf{j}$ è in *ritardo* rispetto ciò che ha visto il registro $\mathsf{i}$.

Il fenomeno che abbiamo descritto sopra è detto *clock skew*. Il clock, come qualsiasi altro segnale, impiega del tempo a "percorrere" il mezzo fisico che collega il generatore al registro. Maggiore è la distanza da percorrere, maggiore è il tempo di propagazione. 

Quali sono gli effetti del clock skew sui vincoli temporali sul periodo di clock? Esistono due casi:

1. $\mathsf{Positive \text{ } clock \text{ } skew}$. Si verifica quando il clock si propaga nella stessa direzione del flusso dei dati.

2. $\mathsf{Negative \text{ } clock \text{ } skew}$. Si verifica quando il clock si propaga nella direzione opposta alla direzione del flusso dei dati.

Analizziamo cosa succede nel caso di clock skew *positivo*. Dunque, il circuito in esame è il seguente:

\begin{center}
    \begin{circuitikz}
        \node [flipflop D] (DFF1) {\texttt{DFF1}};

        \node[fourport, right=of DFF1] (LOG) {\texttt{Comb.} \texttt{Log.}};

        \node [flipflop D, right=of LOG] (DFF2) {\texttt{DFF2}};

        % Connections
        \draw (DFF1.pin 6) -- ++(0.5,0) |- (LOG.port4);
        \draw (LOG.port3) -- ++(0.5,0) |- (DFF2.pin 1);

        % Draw clock direction
    \draw[->] ([yshift=-17.5mm]DFF1.west) -- node[above] {clock} ([yshift=-17.5mm]DFF2.east);
    \end{circuitikz}
\end{center}

* Il fronte di salita del clock visto dal registro a valle della logica combinatoria è in ritardo rispetto al fronte di salita del clock visto dal registro a monte.

* Ciò significa che la durata del periodo di clock "percepita" dai segnali che si propagano tra i due registri è *maggiore* visto che al periodo di clock $\mathsf{T}$ devo sommare il ritardo sperimentato dal fronte di salita del clock visto dal registro a valle.
\begin{align*}
    &\mathsf{T + |\delta| \geq t_{cq1} + t_{plog} + t_{su2}} \\
    &\mathsf{T \geq t_{cq1} + t_{plog} + t_{su2} - |\delta|}
\end{align*}
* Un clock skew positivo consente di aumentare la frequenza di clock (diminuire il periodo).

* Per quanto riguarda l'hold time violation, avere clock skew positivo è deleterio. Dato che il fronte di salita visto dal registro a monte è in anticipo rispetto al fronte di salita visto dal registro a valle, la somma dei contamination delay del registro e della logica combinatoria non devono essere minori del hold time del registro a valle a cui si somma il clock skew.
\begin{align*}
    &\mathsf{t_{hold2} + |\delta| \leq t_{cd1} + t_{cdlog}} \\
    &\mathsf{t_{hold2} \leq t_{cd1} + t_{cdlog} - |\delta|}
\end{align*}

Nel caso di clock skew negativo, ciò che cambia nei vincoli temporali è il segno di $\mathsf{\delta}$.

\begin{center}
    \begin{circuitikz}
        \node [flipflop D] (DFF1) {\texttt{DFF1}};

        \node[fourport, right=of DFF1] (LOG) {\texttt{Comb.} \texttt{Log.}};

        \node [flipflop D, right=of LOG] (DFF2) {\texttt{DFF2}};

        % Connections
        \draw (DFF1.pin 6) -- ++(0.5,0) |- (LOG.port4);
        \draw (LOG.port3) -- ++(0.5,0) |- (DFF2.pin 1);

        % Draw clock direction
        \draw[<-] ([yshift=-17.5mm]DFF1.west) -- node[above] {clock} ([yshift=-17.5mm]DFF2.east);
    \end{circuitikz}
\end{center}
\begin{align*}
    &\mathsf{T \geq t_{cq1} + t_{plog} + t_{su2} + |\delta|} \\
    &\mathsf{t_{hold2} \leq t_{cd1} + t_{cdlog} + |\delta|}
\end{align*}

Sicuramente, il vincolo più difficile da rispettare è l'hold time violation: è più semplice (meno costoso) aumentare il periodo di clock piuttosto che diminuire $\mathsf{t_{hold}}$.


\newpage{}


# Full Adder

Il nostro viaggio alla scoperta dei sommatori inizia dal più semplice sommatore che si possa implementare: sommatore a 1 bit.

\begin{figure}
    \centering
    \begin{tabular}{ccc|cc}
        $\mathsf{A}$ & $\mathsf{B}$ & $\mathsf{C_{in}}$ & $\mathsf{S}$ & $\mathsf{C_{out}}$ \\
        \hline
        0 & 0 & 0 & 0 & 0 \\
        0 & 0 & 1 & 1 & 0 \\
        0 & 1 & 0 & 1 & 0 \\
        0 & 1 & 1 & 0 & 1 \\
        1 & 0 & 0 & 1 & 0 \\
        1 & 0 & 1 & 0 & 1 \\
        1 & 1 & 0 & 0 & 1 \\
        1 & 1 & 1 & 1 & 1 \\
    \end{tabular}
    \caption{$\mathsf{1\text{-}bit\text{ }full\text{ }adder}$ truth table.}
    \label{fig:truth_table_full_adder_1_bit}
\end{figure}

Possiamo calcolare le due uscite con le seguenti espressioni logiche:
\begin{align*}
    &\mathsf{S = A \oplus B \oplus C_{in}} \\
    &\mathsf{C_{out} = A \cdot B + A \cdot C_{in} + B \cdot C_{in}}
\end{align*}

Analizziamo più da vicino la tabella di verità del sommatore:

1. Prendiamo in esame le uscite $\mathsf{S}$ e $\mathsf{C_{out}}$. Prendiamo in considerazione solo le uscite associate agli stati diversi da $\mathsf{\{A,B,C_{in}\} = \{0,0,0\}}$ e $\mathsf{\{A,B,C_{in}\} = \{1,1,1\}}$. Possiamo notare una proprietà molto utile:
$$
    \mathsf{S(A, B, C_{in}) = \bar{C}_{out}(A, B, C_{in})}
$$
ovvero l'uscita $\mathsf{S}$ può essere ottenuta come la negazione dell'uscita $\mathsf{C_{out}}$ quando almeno un ingresso è diverso da $\mathsf{0}$ e tutti non sono uguali a $\mathsf{1}$.

\begin{center}
    \begin{tabular}{ccc|cc}
        $\mathsf{A}$ & $\mathsf{B}$ & $\mathsf{C_{in}}$ & $\mathsf{S}$ & $\mathsf{C_{out}}$ \\
        \hline
        0 & 0 & 0 & 0 & 0 \\
        0 & 0 & 1 & \textcolor{red}{1} & \textcolor{blue}{0} \\
        0 & 1 & 0 & \textcolor{red}{1} & \textcolor{blue}{0} \\
        0 & 1 & 1 & \textcolor{blue}{0} & \textcolor{red}{1} \\
        1 & 0 & 0 & \textcolor{red}{1} & \textcolor{blue}{0} \\
        1 & 0 & 1 & \textcolor{blue}{0} & \textcolor{red}{1} \\
        1 & 1 & 0 & \textcolor{blue}{0} & \textcolor{red}{1} \\
        1 & 1 & 1 & 1 & 1 \\
    \end{tabular}
\end{center}

2. Tale proprietà può essere usata per ridurre la complessità (in termini di transistor) del circuito: di fatto possiamo utilizzare la rete combinatoria che produce l'uscita $\mathsf{C_{out}}$ anche per produrre l'uscita $\mathsf{S}$.
$$
    \mathsf{S(A, B, C_{in}) = \bar{C}_{out}(A, B, C_{in})} + A \cdot B \cdot C_{in}
$$

Cosa possiamo trovare nel datasheet di un $\mathsf{1\text{-}bit}$ full adder?

1. La tabella di verità (Figura \ref{fig:truth_table_full_adder_1_bit}).

2. Le capacità associate a ciascun ingresso. Esempio:
\begin{center}
    \begin{tabular}{c|c}
        $\mathsf{Pin}$ & $\mathsf{Cap \text{ } [pF]}$ \\
        \hline
        $\mathsf{A}$ & 0.038      \\
        $\mathsf{B}$ & 0.037      \\
        $\mathsf{C_{in}}$ & 0.032 \\
    \end{tabular}
\end{center}

3. L'occupazione del chip e la *power consumption*.

4. Tutti i ritardi che caratterizzo l'attività della rete combinatoria. In particolare, avremo delle informazioni a riguardo il ritardo di propagazione di ogni percorso, *cioè*:
    * Il ritardo del percorso $\mathsf{A \rightarrow S}$. 
    * Il ritardo del percorso $\mathsf{A \rightarrow C_{out}}$. 
    * Il ritardo del percorso $\mathsf{B \rightarrow S}$. 
    * Il ritardo del percorso $\mathsf{B \rightarrow C_{out}}$. 
    * Il ritardo del percorso $\mathsf{C_{in} \rightarrow S}$. 
    * Il ritardo del percorso $\mathsf{C_{in} \rightarrow C_{out}}$. 

Inoltre, potremo avere delle informazioni a riguardo del contamination delay della rete combinatoria.


## Serial Adder 

Come possiamo effettuare la somma di numeri rappresentati su $\mathsf{N>1}$ bits? Un possibile approccio è descritto in Figura \ref{fig:serial_fa}.

![Arch. of $\mathsf{N}$-bits serial full adder.\label{fig:serial_fa}](figures/arith/serial_adder.png)

Quanto tempo si impiega a calcolare la somma di due interi su $\mathsf{N}$ bits? Facile: $\mathsf{N \cdot T_{clk}}$. Per calcolare il periodo di clock minimo dobbiamo tenere conto dei vincoli imposti dal *setup time*. Anche in questo caso dobbiamo analizzare tutti i percorsi $\mathsf{reg\text{-}log\text{-}reg}$ e calcolare il periodo di clock minimo in funzione del percorso più "lento".

Facciamo un esempio: supponiamo che il tempo di propagazione da un qualsiasi ingresso all'uscita $\mathsf{S}$ sia minore del tempo di propagazione da un qualsiasi ingresso all'uscita $\mathsf{C_{out}}$. In altre parole,

* $\mathsf{T_{A \rightarrow S} < T_{A \rightarrow C_{out}}}$
* $\mathsf{T_{B \rightarrow S} < T_{B \rightarrow C_{out}}}$
* $\mathsf{T_{C_{in} \rightarrow S} < T_{C_{in} \rightarrow C_{out}}}$ 

![Serial adder: lista dei percorsi critici. Ipotesi: $\mathsf{T_{S} < T_{C_{out}}}$.\label{fig:serial_full_adder_critical_path}](figures/arith/serial_adder_critical_path.png)

Come possiamo vedere dalla Figura \ref{fig:serial_full_adder_critical_path}, abbiamo i seguenti vincoli temporali:

1. $\mathsf{T_{clk} \geq T_{cq,A} + T_{C_{out}} + T_{su,C_{in}}}$ 

2. $\mathsf{T_{clk} \geq T_{cq,B} + T_{C_{out}} + T_{su,C_{in}}}$

3. $\mathsf{T_{clk} \geq T_{cq,C_{in}} + T_{C_{out}} + T_{su,C_{in}}}$

Possiamo pensare che tutti i registri del circuito sono dello stesso tipo (quindi hanno gli stessi delay e setup time). Da ciò si ricava che il tempo necessario a compiere la somma di due interi su $\mathsf{N}$-bits è $\mathsf{N T_{clk} \geq N (T_{cq} + T_{C} + T_{su})}$.


## Parallel Adder: Ripple Carry Adder

![Arch. of $\mathsf{4}$-bits ripple carry full adder.](figures/arith/ripple_carry_adder.png){width="75%"}

Con un architettura parallela, la somma avviene in un unico ciclo di clock! 

Supponiamo che $\mathsf{T_{C_{out}} > T_{S}}$. Il percorso "più" critico della rete combinatoria è rappresentanto in verde in Figura \ref{fig:ripple_carry_adder_critical_path}.

![Ripple Carry adder: percorso critico. Ipotesi: $\mathsf{T_{S} < T_{C_{out}}}$.\label{fig:ripple_carry_adder_critical_path}](figures/arith/ripple_carry_adder_critical_path.png){width="75%"}

Sotto queste ipotesi il tempo necessario per eseguire la somma di due interi rappresentati su $\mathsf{N}$ bits è $\mathsf{N \cdot T_{C_{out}}}$.


## Parallel Adder: Pipeline solution

Come possiamo risolvere il problema del ritardo della generazione del carry? Introduciamo un stadio di pipeline (Figura \ref{fig:pipeline_adder}).

![Idea di pipeline applicata ad un ripple carry adder.\label{fig:pipeline_adder}](figures/arith/pipeline_adder.png){width="75%"}

L'archittetura presentata in Figura \ref{fig:pipeline_adder} ha un problema:

1. Supponiamo di voler eseguire una somma ad ogni ciclo di clock.

2. Il riporto in input al sommatore $\mathsf{2}$ è in ritardo di un ciclo di clock rispetto agli input presenti in $\mathsf{A_{2}}$, $\mathsf{B_{2}}$, $\mathsf{A_{3}}$ e $\mathsf{B_{3}}$. Questo è dovuto al fatto che il registro di pipeline campiona il carry $\mathsf{C_{1}}$ al clock $\mathsf{t_{i}}$ e l'output associato è valido nel ciclo di clock successivo. *Soluzione*: ritardiamo $\mathsf{A_{2}}$, $\mathsf{B_{2}}$, $\mathsf{A_{3}}$ e $\mathsf{B_{3}}$ di un ciclo di clock inserendo un registro che campiona gli ingressi in $\mathsf{t_{i}}$ e gli presenta ai sommatori $\mathsf{2}$ e $\mathsf{3}$ al ciclo di clock $\mathsf{t_{i+1}}$. 

3. Ora abbiamo un problema di sincronizzazione sull'output: $\mathsf{S_{2}}$ e $\mathsf{S_{3}}$ sono validi al ciclo di clock $\mathsf{t_{i+1}}$ ma $\mathsf{S_{0}}$ e $\mathsf{S_{1}}$ sono già validi al ciclo di clock $\mathsf{t_{i}}$. *Soluzione*: inseriamo un registro che ritarda $\mathsf{S_{0}}$ e $\mathsf{S_{1}}$ di un ciclo di clock.

![Architettura completa della pipeline applicata ad un ripple carry adder.\label{fig:pipeline_adder_full_arch}](figures/arith/pipeline_adder_full_arch.png){width="75%"}
