---
title: Domande d'esame
author: Federico Casu
date: Dicembre 1, 2023
header-includes: | 
	\usepackage{subcaption}
	\usepackage{tikz}
    \usetikzlibrary{positioning,shapes,shapes.geometric,arrows}
    \usepackage{pgfplots}
---


# Amplitude Modulation (\texttt{AM})

Prima di presentare il segnale modulato \texttt{AM}, studiamo da quali blocchi è composta la catena $\mathsf{Tx-Rx}$:

\begin{figure}[ht]
    \centering
    \begin{tikzpicture}
        \draw (0,0) rectangle (2,2);
        \node at (1,1) {$\mathsf{Source}$};
        \draw[->] (2,1) -- (4,1) node[midway, above] {$\mathsf{m(t)}$};

        % Modulatore
        \draw (4.3,1) circle (0.3);
        \node at (4.3,1) {\texttt{X}};
        \draw[->] (4.6,1) -- (6.5,1) node[midway, above] {$\mathsf{s_{AM}(t)}$};
        \draw[<-] (4.3,1.3) -- (4.3,1.75) node[midway, above] {$\mathsf{cos(2 \pi f_c t)}$};
    \end{tikzpicture}
    \caption{\texttt{AM} - Trasmettitore}
    \label{fig:AM_tr}
\end{figure}

\begin{figure}[ht]
    \centering
    \begin{tikzpicture}
        \draw (0,0) rectangle (2,2);
        \node at (1,1) {$\mathsf{g_{PB}(t)}$};
        \draw[->] (-2,1) -- (0,1) node[midway, above] {$\mathsf{s_{AM}(t)}$};
        \draw[->] (2,1) -- (4,1) node[midway, above] {$\mathsf{\hat{s}_{AM}(t)}$};

        % Demodulatore
        \draw (4.3,1) circle (0.3);
        \node at (4.3,1) {\texttt{X}};
        \draw[->] (4.6,1) -- (6.5,1) node[midway, above] {$\mathsf{y(t)}$};
        \draw[<-] (4.3,1.3) -- (4.3,1.75) node[midway, above] {$\mathsf{2cos(2 \pi f_c t)}$};

        \draw (6.5,0) rectangle (8.5,2);
        \node at (7.5,1) {$\mathsf{g_{LP}(t)}$};
        \draw[->] (8.5,1) -- (10,1) node[midway, above] {$\mathsf{\hat{m}(t)}$};
    \end{tikzpicture}
    \caption{\texttt{AM} - Ricevitore}
    \label{fig:AM_rx}
\end{figure}

Descriviamo il trasmettitore:

1. Il segnale $\mathsf{m(t)}$ viene modulato dal segnale periodico $\mathsf{cos(2 \pi f_c t)}$. Il risultato della modulazione è il segnale $\mathsf{s_{AM}(t) = m(t) \cdot cos(2 \pi f_c t)}$. 

2. Lato ricevitore, il segnale in entrata $\mathsf{s_{AM}(t)}$ viene filtrato mediante un filtro passabanda in modo tale da attenuare le componenti di rumore al di fuori della banda del segnale.

3. L'operazione di demodulazione viene compiuta dal blocco moltiplicatore adoperando il segnale $\mathsf{2 cos(2 \pi f_c t)}$.

4. Il segnale $\mathsf{y(t)}$, ottenuto in seguito alla demodulazione, viene filtrato da un filtro LTI passa basso. L'obiettivo di quest'ultimo filtraggio è eliminare le copie del segnale $\mathsf{m(t)}$ che si sono create in seguito all'operazione di demodulazione.

Per prima cosa, analizziamo il segnale $\mathsf{s_{AM}(t)}$ in frequenza:
\begin{align*}
    \mathsf{\mathcal{F}\{s_{AM}(t)\}} &= \mathsf{\mathcal{F}\{m(t) cos(2 \pi f_c t)\}} = \\
    &= \mathsf{\mathcal{F}\{m(t)\} * \mathcal{F}\{cos(2 \pi f_c t)\}} = \\
    &= \mathsf{M(f) * \left [ \frac{1}{2} \delta(f - f_c) + \frac{1}{2} \delta(f + f_c) \right ] } = \\
    &= \mathsf{\frac{1}{2} M(f - f_c)  + \frac{1}{2} M(f + f_c) }
\end{align*}

\begin{figure}
    \centering
    \begin{tikzpicture}
    \begin{axis}[
        xlabel=$f$,
        ylabel=$y$,
        title={Plot of $\mathcal{F}\{s_{AM}(t)\} = \mathsf{\frac{1}{2} \left(M(f - f_c) + M(f + f_c)\right)}$},
        domain=-100:100,
        axis lines=middle,
        legend pos=north east,
        ]
        \addplot[const plot, red, thick, mark=none, domain=-75:-25, samples=2500] {1};
        \addplot[const plot, red, thick, mark=none, domain=75:25, samples=2500] {1};

        % Plot vertical lines
        \addplot[red, thick, samples=2] coordinates {(-25, 0) (-25, 1)};
        \addplot[red, thick, samples=2] coordinates {(25, 0) (25, 1)};

        \addplot[red, thick, samples=2] coordinates {(-75, 0) (-75, 1)};
        \addplot[red, thick, samples=2] coordinates {(75, 0) (75, 1)};

        \addplot[white, dashed, samples=100] coordinates {(-100, 1.1) (100, 1.1)};
    \end{axis}
    \end{tikzpicture}
\end{figure}

Ora andiamo a vedere quali sono gli effetti della modulazine (in frequenza):
\begin{align*}
    \mathsf{s_{AM}(t) 2 cos(2 \pi f_c t)} &= \mathsf{m(t) cos(2 \pi f_c t) \cdot 2 cos(2 \pi f_c t)} = \\
    &= \mathsf{m(t) \cdot 2 \left( \frac{1}{2} cos(2 \pi (f_c - f_c) t) + \frac{1}{2} cos(2 \pi (f_c + f_c) t) \right)} = \\
    &= \mathsf{m(t)cos(0) + m(t)cos(2 \pi 2f_c t)} = \\
    &= \mathsf{m(t) + m(t)cos(2 \pi 2f_c t)} \\ \\
    \mathcal{F}\mathsf{\{y(t)\}} &= \mathcal{F}\mathsf{\{m(t) + m(t)cos(2 \pi 2f_c t)\}} = \\
    &= \mathsf{M(f) + \frac{1}{2}M(f - 2f_c) + \frac{1}{2}M(f + 2f_c)}
\end{align*}

Ecco spiegato il motivo del filtro passa-basso a valle del demodulatore: demodulare un segnale, oltre a produrre il segnale di partenza, produce due copie del segnale di partenza ma traslate a frequenza $\mathsf{2f_c}$ e $\mathsf{-2f_c}$.

Facciamo qualche considerazione in più sui segnali caratteristici del sistema:

1. Il segnale $\mathsf{m(t)}$ è un segnale *reale*. Il processo di modulazione non cambia la natura del segnale: $\mathsf{s_{AM}(t) = m(t) \cdot cos(2 \pi f_c t)}$ è un segnale reale.

2. Se la banda del segnale $\mathsf{M(f)}$ è $\mathsf{B}$ allora la banda del segnale $\mathsf{S_{AM}(f)}$ è $\mathsf{B + f_c}$. La modulazione aumenta l'occupazione di banda del segnale proprio perchè il segnale modulante viene traslato alla frequenza portante $\mathsf{f_c}$. In realtà, il segnale modulato ha modulo diverso da zero solo all'interno dell'intervallo di frequenze $\mathsf{[f_c - B, f_c + B]}$. Di fatto la banda effetivamente occupata dal segnale è larga $\mathsf{2B}$.

3. $\mathsf{S_{AM}(f)}$ è un segnale reale. Vista tale proprietà, possiamo dire che, vista la *simmetria hermitiana*, il segnale $\mathsf{S_{AM}(f)}$ è simmetrico rispetto l'origine e simmetrico rispetto $\mathsf{f_c}$ e $\mathsf{-f_c}$.



\newpage{}



# Quadrature Amplitude Modulation (\texttt{QAM}) 

La modulazione \texttt{AM} presenta un difetto: per trasmettere un segnale il cui contenuto informativo originariamente occupava una banda $\mathsf{B}$, dobbiamo trasmettere un segnale di banda $\mathsf{2B}$. Per massimizzare l'utilizzo della banda sfruttiamo l'ortogonalità tra seno e coseno.

Supponiamo di voler trasmettere due segnali: $\mathsf{m_{I}(t)}$ e $\mathsf{m_{Q}(t)}$. Il trasmettitore \texttt{QAM} è composto dai seguenti blocchi:

* Un modulatore. A differenza del modulatore \texttt{AM}, il modulatore \texttt{QAM} è composto da due rami:
    - Un ramo si occupa della modulazione del segnale $\mathsf{m_{I}(t)}$. La modulazione viene eseguita per mezzo di $\mathsf{cos(2 \pi f_c t)}$.
    - L'altro ramo si occupa della modulazione del segnale $\mathsf{m_{Q}(t)}$. La modulazione viene eseguita per mezzo di $\mathsf{-sin(2 \pi f_c t)}$.

* Una volta che i segnali $\mathsf{m_{I}(t)}$ e $\mathsf{m_{Q}(t)}$ sono stati modulati, i due segnali vengono sommati e inviati attraverso il canale.

\begin{figure}[ht]
    \centering
    \begin{tikzpicture}
        \draw (0,0) rectangle (2,2);
        \node at (1,1) {$\mathsf{Source}$};
        \draw[->] (2,1) -- (4,1) node[midway, above] {$\mathsf{m_{I}(t)}$};

        % Modulatore
        \draw (4.3,1) circle (0.3);
        \node at (4.3,1) {\texttt{X}};
        \draw[->] (4.6,1) -- (6.5,1) node[midway, above] {$\mathsf{s_{I}(t)}$};
        \draw[<-] (4.3,1.3) -- (4.3,1.75) node[midway, above] {$\mathsf{cos(2 \pi f_c t)}$};
    \end{tikzpicture}
    \caption{\texttt{QAM} - Ramo in fase del $\mathsf{Tx}$}
    \label{fig:QAM_tr_I}
\end{figure}

\begin{figure}[ht]
    \centering
    \begin{tikzpicture}
        \draw (0,0) rectangle (2,2);
        \node at (1,1) {$\mathsf{Source}$};
        \draw[->] (2,1) -- (4,1) node[midway, above] {$\mathsf{m_{Q}(t)}$};

        % Modulatore
        \draw (4.3,1) circle (0.3);
        \node at (4.3,1) {\texttt{X}};
        \draw[->] (4.6,1) -- (6.5,1) node[midway, above] {$\mathsf{s_{Q}(t)}$};
        \draw[<-] (4.3,1.3) -- (4.3,1.75) node[midway, above] {$\mathsf{-sin(2 \pi f_c t)}$};
    \end{tikzpicture}
    \caption{\texttt{QAM} - Ramo in quadratura del $\mathsf{Tx}$}
    \label{fig:QAM_tr_Q}
\end{figure}

\begin{figure}[ht]
    \centering
    \begin{tikzpicture}
        % In-Phase Branch
        \node [draw, minimum height=2cm, minimum width=2cm] (block1) at (0,0) {In-Phase Branch};

        % Quadrature Branch
        \node [draw, minimum height=2cm, minimum width=2cm, below=of block1] (block2) {Quadrature Branch};

        % Summation node
        \node [draw, circle, right=of block2, inner sep=2pt] (sum) {$\Sigma$};
        
        % Connect In-Phase Branch to Summation node with a break
        \draw[->] (block1.east) -| (sum.north) node[midway, above] {$\mathsf{m_{I}(t)}$};

        % Connect Quadrature Branch to Summation node
        \draw[->] (block2.east) -- (sum.west) node[midway, above] {$\mathsf{m_{Q}(t)}$};

        % Output
        \draw[->] (sum.east) -- ++(1.75,0) node[midway, above] {$\mathsf{s_{QAM}(t)}$};
    \end{tikzpicture}
    \caption{\texttt{QAM} - Trasmettitore}
    \label{fig:QAM_tr}
\end{figure}

Il segnale trasmesso dal trasmettitore \texttt{QAM} è il seguente:
$$
    \mathsf{s_{QAM}(t) = m_{I}(t)cos(2 \pi f_c t) - m_{Q}(t)sin(2 \pi f_c t)}
$$

Al contrario di quanto accadeva nella modulazione $\mathsf{AM}$, la modulazione \texttt{QAM} rende lo spettro del modulo di $\mathsf{S_{QAM}(f)}$ simmetrico rispetto l'origine ma si perde la simmetria rispetto la frequenza portante $\mathsf{f_c}$.

La trasformata di Fourier del segnale $\mathsf{s_{QAM}(t)}$ è la seguente:
\begin{align*}
    \mathsf{\mathcal{F}\{s_{QAM}(t)\}} &= \mathsf{\mathcal{F}\{m_{I}(t)cos(2 \pi f_c t) - m_{Q}(t)sin(2 \pi f_c t)\}} = \\
    &= \mathsf{\mathcal{F}\{m_{I}(t)cos(2 \pi f_c t)\} - \mathcal{F}\{m_{Q}(t)sin(2 \pi f_c t)\}} = \\
    &= \mathsf{\frac{1}{2}M_{I}(f - f_c) + \frac{1}{2}M_{I}(f + f_c) - \frac{j}{2}M_{Q}(f - f_c) + \frac{j}{2}M_{Q}(f + f_c)}
\end{align*}

\begin{figure}
    \centering
    \begin{tikzpicture}
    \begin{axis}[
        xlabel=$f$,
        ylabel=$y$,
        title={Plot of $\mathcal{F}\mathsf{\{s_{QAM}(t)\}}$},
        domain=-100:100,
        axis lines=middle,
        legend style={at={(0.5,-0.15)}, anchor=north},
        ]

        \addplot[red, const plot, samples at={-75, -65}] {1.00};
        \addplot[blue, const plot, samples at={75, 65}] {1.00};

        \addplot[red, const plot, samples at={-65, -55}] {0.93};
        \addplot[blue, const plot, samples at={65, 55}] {0.93};

        \addplot[red, const plot, samples at={-55, -45}] {0.90};
        \addplot[blue, const plot, samples at={55, 45}] {0.90};

        \addplot[red, const plot, samples at={-45, -35}] {0.95};
        \addplot[blue, const plot, samples at={45, 35}] {0.95};
        
        \addplot[red, const plot, samples at={-35, -25}] {0.98};        
        \addplot[blue, const plot, samples at={35, 25}] {0.98};

        
        % Plot vertical lines
        \addplot[blue, thick, samples=2] coordinates {(75, 0) (75, 1)};
        \addplot[blue, thick, samples=2] coordinates {(65, 0.93) (65, 1)};
        \addplot[blue, thick, samples=2] coordinates {(55, 0.93) (55, 0.90)};
        \addplot[blue, thick, samples=2] coordinates {(45, 0.90) (45, 0.95)};
        \addplot[blue, thick, samples=2] coordinates {(35, 0.95) (35, 0.98)};
        \addplot[blue, thick, samples=2] coordinates {(25, 0) (25, 0.98)};

        \addplot[red, thick, samples=2] coordinates {(-75, 0) (-75, 1)};
        \addplot[red, thick, samples=2] coordinates {(-65, 0.93) (-65, 1)};
        \addplot[red, thick, samples=2] coordinates {(-55, 0.93) (-55, 0.90)};
        \addplot[red, thick, samples=2] coordinates {(-45, 0.90) (-45, 0.95)};
        \addplot[red, thick, samples=2] coordinates {(-35, 0.95) (-35, 0.98)};
        \addplot[red, thick, samples=2] coordinates {(-25, 0) (-25, 0.98)};

        % Dashed line
        \addplot[white, dashed, samples=100] coordinates {(-100, 1.1) (100, 1.1)};

        \legend{$\mathsf{M_{I}(f - f_c) -jM_{Q}(f - f_c)}$, $\mathsf{M_{I}(f + f_c) + jM_{Q}(f + f_c)}$}
    \end{axis}
    \end{tikzpicture}
\end{figure}

Lato ricevitore il segnale $\mathsf{s(t)}$ viene demodulato sia per mezzo di $\mathsf{2cos(2 \pi f_{c} t)}$ sia $\mathsf{-2sin(2 \pi f_{c} t)}$. Il risultato dell'operazione di demodulazione è il seguente:
\begin{align*}
    \mathsf{m_{I}(t)} &= \mathsf{s_{QAM}(t)} \cdot \mathsf{2cos(2 \pi f_{c} t)} = \\
    &= [\mathsf{m_{I}(t) \cdot cos(2 \pi f_{c} t)} - \mathsf{m_{Q}(t) \cdot sin(2 \pi f_{c} t)}] \cdot \mathsf{2cos(2 \pi f_{c} t)} = \\
    &= \mathsf{m_{I}(t) \cdot cos(2 \pi f_{c} t) \cdot 2cos(2 \pi f_{c} t)} - \mathsf{m_{Q}(t) \cdot sin(2 \pi f_{c} t) \cdot 2cos(2 \pi f_{c} t)} 
\end{align*}
Applicando le formule di Werner[^1] all'equazione di sopra otteniamo il seguente segnale:
\begin{align*}
    &= \mathsf{m_{I}(t) \cdot cos(0)} + \mathsf{m_{I}(t) \cdot cos(2 \pi (2f_{c}) t)} - \mathsf{m_{Q}(t) \cdot sin(0)} - \mathsf{m_{Q}(t) \cdot sin(2 \pi (2f_{c}) t) } = \\
    &= \mathsf{m_{I}(t)} + \mathsf{m_{I}(t) \cdot cos(2 \pi (2f_{c}) t)} - \mathsf{m_{Q}(t) \cdot sin(2 \pi (2f_{c}) t)}
\end{align*}

[^1]: Abbiamo utilizzato $cos(\alpha)cos(\beta) = \frac{1}{2}cos(\alpha + \beta) + \frac{1}{2}cos(\alpha - \beta)$ e $sin(\alpha)cos(\beta) = \frac{1}{2}sin(\alpha + \beta) + \frac{1}{2}sin(\alpha - \beta)$.

Il segnale demodulato è dunque composto da tre componenti:

- Il segnale originario $\mathsf{m_{I}(t)}$, quest'ultimo in banda base.
- Una copia del segnale $\mathsf{m_{I}(t)}$ traslata a $\mathsf{2f_c}$.
- Una copia del segnale $\mathsf{m_{Q}(t)}$ traslata a $\mathsf{2f_c}$.

Grazie al filtro passa-basso siamo in grado di isolare il segnale in banda base dalle copie (quest'ultime in banda passante) ottenute in fase di demodulazione. L'analisi per il segnale $\mathsf{m_{Q}(t)}$, proveniente dal ramo in *quadratura*, è del tutto analoga a quella appena compiuta per il segnale proveniente dal ramo in *fase*, *mutatis mutandis*.



\newpage{}



# Frequency Modulation (\texttt{FM})

Le modulazioni che abbiamo visto fino ad ora (\texttt{AM} e \texttt{QAM}) sono modulazioni d'ampiezza, ovvero la modulazione imprime il contenuto informativo del segnale nell'ampiezza del segnale modulato. Ora andremo a scoprire le modulazioni ad ampiezza costante: modulazioni il cui compito è mantenere l'ampiezza del segnale modulato costante ed imprimere il contenuto informativo, ad esempio, nella fase del segnale.

Il segnale prodotto da un Trasmettitore \texttt{FM} è il seguente:
$$
    \mathsf{s_{FM}(t) = A_c cos \left( 2 \pi f_c t + 2 \pi k_{f} \int_{- \infty}^{t}m(\tau) d\tau \right)}
$$

A differenza dei due approcci visti in precedenza, studiare la banda del segnale $\mathsf{S_{FM}(f)}$ è più complicato visto che varia in funzione del tempo.

Per poter comprendere l'occupazione di banda del segnale $\mathsf{S_{FM}(f)}$ è necessario studiare un esempio: supponiamo di voler trasmettere il segnale $\mathsf{m(t) = V_m cos(2 \pi f_m t)}$.

Dunque, il segnale modulato è il seguente:
\begin{align*}
    \mathsf{s_{FM}(t)} &= \mathsf{A_c cos \left( 2 \pi f_c t + 2 \pi k_{f} \int_{- \infty}^{t} V_m cos(2 \pi f_m \tau) d\tau \right)} = \\
    &= \mathsf{A_c cos \left( 2 \pi f_c t + 2 \pi k_{f} \frac{V_m}{2 \pi f_m} sin(2 \pi f_m t) \right)} = \\
    &= \mathsf{A_c cos \left( 2 \pi f_c t + \frac{k_{f} V_m}{f_m} sin(2 \pi f_m t) \right)}
\end{align*}

Prima di proseguire lo studio del segnale $\mathsf{s_{FM}(t)}$, introduciamo alcuni indici:
\begin{align*}
    \mathsf{Frequency \text{ } deviation \text{ } f_d(t)} &= \mathsf{\frac{1}{2\pi}\frac{d\phi(t)}{dt} - f_c} \\
    \mathsf{Maximum \text{ } frequency \text{ } deviation \text{ } \Delta f} &= \mathsf{max\{|f_d(t)|\}} \\
    \mathsf{Modulation \text{ } Index \text{ } m_f} &= \mathsf{\frac{\Delta f}{B_{m}}}
\end{align*}

Nel caso in cui $\mathsf{m(t) = V_m cos(2 \pi f_m t)}$, allora abbiamo le seguenti quantità:
\begin{align*}
    \mathsf{f_d(t)} &= \mathsf{\frac{1}{2 \pi} \frac{d}{dt} \left(2 \pi f_c t +  2 \pi k_{f} \int_{- \infty}^{t} V_m cos(2 \pi f_m \tau) d\tau \right) - f_c} \\
    &= \mathsf{\frac{1}{2 \pi} \frac{d}{dt} \left(2 \pi f_c t +  \frac{k_{f} V_m}{f_m} sin(2 \pi f_m t) \right) - f_c} \\
    &= \mathsf{\frac{1}{2 \pi} \left(2 \pi f_c +  \frac{k_{f} V_m}{f_m} 2 \pi f_m cos(2 \pi f_m t) \right) - f_c} \\
    &= \mathsf{f_c +  k_{f} V_m cos(2 \pi f_m t) - f_c} \\
    &= \mathsf{k_{f} V_m cos(2 \pi f_m t)} \\ \\
    \mathsf{\Delta f} &= \mathsf{max\{|f_d(t)|\}} = \mathsf{max\{|f_d(t)|\}} = \\
    &= \mathsf{max\{|k_{f} V_m cos(2 \pi f_m t)|\}} = \mathsf{k_{f} V_m} \\ \\
    \mathsf{m_f} &= \mathsf{\frac{\Delta f}{B_m}} = \mathsf{\frac{k_{f} V_m}{B_m}}
\end{align*}

Ricordando che la banda di un segnale sinusoidale coincide con la sua frequenza (lo spettro di un segnale sinusoidale consiste in due delta centrate in $\mathsf{-f_c}$ e $\mathsf{f_c}$), otteniamo:
$$
    \mathsf{m_f} = \mathsf{\frac{\Delta f}{B_m}} = \mathsf{\frac{k_{f} V_m}{f_m}}
$$

Dunque, il segnale modulato è il seguente:
$$
    \mathsf{s_{FM}(t) = A_c cos ( 2 \pi f_c t + m_f sin(2 \pi f_m t) )}
$$

L'inviluppo complesso del segnale \texttt{FM} è il seguente:
$$
    \mathsf{\tilde{s}_{FM}(t) = A_c exp(j \cdot m_f \cdot sin(2 \pi f_m t))}
$$

Visto che il segnale $\mathsf{s_{FM}(t)}$ è periodico, anche il suo inviluppo complesso è periodico. In particolare, ricordiamo che possiamo rappresentare un segnale periodico come somma di coefficienti di Fourier:
$$
    \mathsf{\tilde{s}_{FM}(t) = A_c \sum_{n} S_n e^{j 2 \pi n f_m t}}
$$
Si può dimostrare che i coefficienti di Fourier $\mathsf{S_n}$ coincidono con le funzioni di Bessel del primo tipo di ordine valutate in $\mathsf{m_f}$. Di conseguenza, otteniamo che l'inviluppo complesso del segnale $\mathsf{s_{FM}(t)}$ può essere rappresentato come segue:
$$
    \mathsf{\tilde{s}_{FM}(t) = A_c \sum_{n} J_n(m_f) e^{j 2 \pi n f_m t}}
$$
A partire da $\mathsf{\tilde{s}_{FM}(t)}$ calcoliamo il segnale passa-banda:
\begin{align*}
    \mathsf{s_{FM}(t)} &= \mathsf{\Re\{\tilde{s}_{FM}(t) e^{j 2 \pi f_c t}\}} = \\
    &= \mathsf{\Re\{A_c \sum_{n} J_n(m_f) e^{j 2 \pi n f_m t} e^{j 2 \pi f_c t}\}} = \\
    &= \mathsf{\Re\{A_c \sum_{n} J_n(m_f) e^{j 2 \pi (f_c + n f_m) t} \}} = \\
    &= \mathsf{\Re\{A_c \sum_{n} J_n(m_f) \left( cos[2 \pi (f_c + n f_m) t] + j sin[2 \pi (f_c + n f_m) t] \right) \}} = \\
    &= \mathsf{A_c \sum_{n} J_n(m_f) cos[2 \pi (f_c + n f_m) t]}
\end{align*}

Quale sarebbe lo spettro del modulo del segnale $\mathsf{A_c \sum_{n} J_n(m_f) cos[2 \pi (f_c + n f_m) t]}$? 

\begin{figure}
    \centering
    \begin{tikzpicture}[scale=1]
        % Draw axis
        \draw[->] (-6,0) -- (6,0) node[right] {$f$};
        \draw[->] (-5.5,-0.5) -- (-5.5,2.5) node[above] {};

        \draw [red] (-4,0) -- (-4,1);
        \draw [red] (-2,0) -- (-2,1.5);
        \draw [red] (0,0) -- (0,2);
        \draw [red] (2,0) -- (2,1.5);
        \draw [red] (4,0) -- (4,1);

        % Add labels
        \foreach \x/\label in {-4/{f_c - 2f_m}, -2/{f_c - f_m}, 0/{f_c}, 2/{f_c + f_m}, 4/{f_c + 2f_m}}
            \node[below] at (\x,0) {$\label$};
    \end{tikzpicture}
    \caption{$\mathsf{A_c\sum_{n}J_{n}(m_f)\delta(f - f_c - nf_m)}$}
    \label{fig:b_FM}
\end{figure} 

Cosa possiamo notare dal plot in Figura \ref{fig:b_FM}? 

1. L'ampiezza delle delta dipende dalle funzioni di Bessel, che a loro volta, dipendono dal modulation index $\mathsf{m_f}$.

2. Le delta sono posizionate ad intervalli regolari, ovvero troviamo una delta ogni $\mathsf{f_c + nf_m}$.

La banda di un segnale \texttt{FM} è funzione del modulation index $\mathsf{m_f}$ e della banda del segnale modulante (si noti che nell'esempio la banda del segnale modulante era $\mathsf{f_m}$).

> **Carson Bandwidth's Rule**: $\mathsf{B_{FM} \approx 2(m_f + 1)B = 2(\Delta f + B)}$



\newpage{}



# Wireless Propagation Channel

La nostra attenzione è sempre stata diretta sui trasmettitori e sui ricevitori. In particolare non ci siamo mai occupati di studiare nel dettaglio il mezzo di propagazione. Quando abbiamo studiato le modulazioni \texttt{PAM} (o \texttt{QAM}) l'ipotesi è sempre stata quella di canale ideale, ovvero $\mathsf{h(t) = \delta (t)}$. In realtà questa ipotesi in alcuni situazioni è abbastanza stretta.

In funzione della frequenza portante utilizzata, le onde elettromagnetiche (ovvero il mezzo con il quale avvengono le comunicazioni wireless) si propagano in modo differente:

* $\mathsf{Ground \text{ } Wave \text{ } Propagation}$. Per onde elettromagnetiche LF (Low Frequency), quest'ultime si propagano seguendo la curvatura terrestre.

* $\mathsf{Sky \text{ } Wave \text{ } Propagation}$. Per onde elettromagnetiche MF/HF (Medium/High Frequency), quest'ultime si propagano ribalzando tra il suolo terrestre e la ionosfera (quest'ultima, per certe frequenza funziona da specchio).

* $\mathsf{Space \text{ } Wave \text{ } Propagation}$. Per le onde elettromagnetiche ad alta e altissima frequenza (HF/VHF), la loro propagazione segue diverse traiettorie. In particolare, nel caso in cui sia presente, le onde elettromagnetiche si diffondono lungo il percorso diretto (Line-Of-Sight path) $\mathsf{Tx}$-$\mathsf{Rx}$, oltre a seguire percorsi alternativi caratterizzati da fenomeni fisici come riflessione, diffrazione e scattering.

I moderni servizi di comunicazione mobile si basano sul modello $\mathsf{space \text{ } wave \text{ } propagation}$. Dunque ora andremo a studiare nel dettaglio le caratteristiche di questo modello. Abbiamo già accennato i diversi fenomeni fisici che entrano in gioco quando le onde elettromagnetiche si propagano "seguendo" il modello in questione. Ma cosa sono questi fenomeni?

* La *riflessione* è un fenomeno che si verifica quando un'onda elettromagnetica incide su una superficie e rimbalza indietro: quando un'onda incontra una superficie, parte dell'energia dell'onda può essere riflessa indietro nel mezzo da cui proviene. La riflessione è un fenomeno importante nelle comunicazioni wireless. Ad esempio, in un ambiente urbano, le onde elettromagnetiche possono riflettersi su edifici e altri ostacoli, influenzando il percorso totale di propagazione

* La *diffrazione* è un fenomeno che si verifica quando un'onda elettromagnetica incide su uno spigolo (in generale su un oggetto che presenta una superficie appuntita): l'onda cambia direzione di propagazione. In altre parole l'onda elettromagnetica viene deviata rispetto al suo precedente percorso.

* Lo *scattering* è un fenomeno che si verifica quando un'onda elettromagnetica incide su una superificie frastagliata: l'impatto crea diverse repliche del segnale, tutte quante attenuate e con una fase diversa dall'onda elettromagnetica originale.

Ovviamente, i fenomeni fisici che abbiamo elencato sopra (e tanti altri che non abbiamo elencato) fanno sì che il canale sia tutto fuorchè ideale. Come modelliamo il canale?

1. La potenza *media* del segnale trasmesso diminuisce all'aumentare della distanza $\mathsf{Tx}$-$\mathsf{Rx}$. Inoltre, due ricevitori che si trovano alla stessa distanza sperimentano da un trasmettitore ricevono lo stesso segnale con una potenza media diversa. Questi due fenomeni possono essere modellati attraverso un modello, detto $\mathsf{Large \text{ } Scale \text{ } Fading}$, composto da una componente deterministica e da una componente aleatoria.

2. I fenomeni fisici (tra i quali riflessione, diffrazione, scattering) fanno sì che il segnale visto dal ricevitore sia composto da più repliche del segnale originario. Ognuna di queste repliche sperimenta una certa attenuazione e arriva con un certo ritardo (ed una certa fase) rispetto al segnale che si propaga seguendo il percorso diretto. Gli effetti appena menzionati possono essere descritti attraverso un modello di propagazione: $\mathsf{Small \text{ } Scale \text{ } Fading}$.


## Large Scale Fading

Prendiamo in considerazione la potenza media del segnale visto dal $\mathsf{Rx}$. In particolare, la potenza media del segnale ricevuto è *1)* proporzionale alla potenza media del segnale trasmesso e *2)* inversamente proporzionale alla distanza $\mathsf{d}$ che separa il ricevitore dal trasmettitore.
$$
    \mathsf{P_{Rx} \propto P_{Tx} \cdot \Gamma(f_{0}, d_{0}) \left( \frac{d_{0}}{d}\right)^{n}}
$$
dove:

1. $\mathsf{d_0 < d}$

2. $\mathsf{\Gamma(f_{0}, d_{0})}$: *Near Field Term*. $\mathsf{\Gamma(f_{0}, d_{0}) \approx \left(\frac{\lambda}{4 \pi d_0}\right)^{2}}$

3. $\mathsf{n}$: *Path Exponent*. Dipende dall'ambiente in cui si svolge la comunicazione wireless. Più l'ambiente è *disturbato*, maggiore è il path exponent.

L'attenuazione del segnale è un fenomeno deterministico e può essere rappresentato tramite un coefficiente moltiplicativo:
$$
    \mathsf{A_{PL} = \frac{P_{Rx}}{P_{Tx}} = \Gamma(f_{0}, d_{0}) \left( \frac{d_{0}}{d}\right)^{n}}
$$
$\mathsf{A_{PL}}$, detto coefficiente di $\mathsf{Path \text{ } Loss}$, descrive l'attenuazione della potenza del segnale in funzione della distanza $\mathsf{d}$ che separa il ricevitore dal trasmettitore. In particolare, se rappresentiamo (in decibel) l'andamento della potenza media del segnale ricevuto in funzione della distanza $\mathsf{d}$, otteniamo una funzione lineare decrescente.
\begin{align*}
    \mathsf{A_{PL}^{dB}} &= \mathsf{\left( \frac{P_{Rx}}{P_{Tx}} \right)_{dB}} = \mathsf{\frac{10 \cdot log_{10} P_{Rx}}{10 \cdot log_{10} P_{Tx}}} = \\
    &= \mathsf{10 \cdot log_{10} P_{Rx} - 10 \cdot log_{10} P_{Tx}} = \\
    &= \mathsf{10 \cdot log_{10} \left( P_{Tx} \cdot \Gamma(f_{0}, d_{0}) \left( \frac{d_{0}}{d}\right)^{n} \right) - 10 \cdot log_{10} P_{Tx}} = \\
    &= \mathsf{10 \cdot log_{10} P_{Tx} + 10 \cdot log_{10} \Gamma(f_{0}, d_{0}) + 10 \cdot log_{10} \left( \frac{d_{0}}{d}\right)^{n} - 10 \cdot log_{10} P_{Tx}} = \\
    &= \mathsf{10 \cdot log_{10} \Gamma(f_{0}, d_{0}) + 10 \cdot log_{10} \left( \frac{d_{0}}{d}\right)^{n}} = \\
    &= \mathsf{10 \cdot log_{10} \Gamma(f_{0}, d_{0}) + n10 \cdot log_{10} d_{0}- n10 \cdot log_{10} d} = \\
    &= \mathsf{ \Gamma(f_{0}, d_{0})_{dB} + n10 \cdot log_{10} d_{0} - n10 \cdot log_{10} d}
\end{align*}

Abbiamo anche detto che la potenza media vista da due ricevitori che si trovano alla stessa distanza $\mathsf{d}$ dal trasmettitore non è la stessa. Questo fenomeno può essere descritto mediante una variabile aleatoria. Il coefficiente di **shadowing** è una variabile aleatoria che segue una distribuzione $\mathsf{log}$-$\mathsf{normale}$ a media nulla e varianza $\mathsf{\sigma_{s}}$:
$$
    \mathsf{f^{dB}_{\alpha_s}(x) = \frac{1}{\sqrt{2 \pi } \sigma_s} exp \left( - \frac{x^2}{2 \sigma_s^2} \right)}
$$


## Small Scale Fading

Lo *small scale fading* tiene conto delle variazioni casuali della potenza istantanea del segnale ricevuto su distanze comparabili alla lunghezza d'onda. A causa dei vari fenomeni di propagazione, un gran numero di onde, ognuna portante una replica del segnale trasmesso, giunge al ricevitore. 

Come modelliamo il canale se vogliamo tenere conto dello small scale fading? Supponiamo che $\mathsf{y(t)}$ rappresenti il segnale visto dal ricevitore. Come già detto in precedenza, visto che il segnale ricevuto è composto da $\mathsf{N_c}$ repliche diverse del segnale originariamente trasmesso $\mathsf{s(t)}$ (ciascuna replica caratterizzata dalla sua attenuazione, fase e ritardo), il segnale $\mathsf{y(t)}$ può essere espresso come segue:
$$
    \mathsf{y(t) = A_{LS} \sum_{i=0}^{N_c - 1} \alpha_{i} e^{j \phi_{i}} s(t - \tau_i)}
$$
dove 

1. $\mathsf{A_{LS}}$ è il coefficiente moltiplicativo che descrive gli effetti sulla potenza media del segnale causati dal *path loss* e dallo *shadowing*.

2. $\mathsf{\alpha_{i}}$ è una variabile aleatoria che segue la distribuzione di Rayleigh.

3. $\mathsf{e^{j \phi_{i}}}$ è una variabile aleatoria distribuita uniformemente in $\mathsf{[0, 2 \pi]}$.

4. $\mathsf{\tau_i}$ rappresenta il ritardo sperimentato dalla replica $\mathsf{i}$-esima.

Sfruttando la proprietà campionatrice della funzione delta ($\mathsf{\delta(t - \tau) * s(t) = s(t - \tau)}$), possiamo modellare il canale attraverso la seguente espressione:
$$
    \mathsf{h(t) = A_{LS} \sum_{i=0}^{N_c - 1} \alpha_{i} e^{j \phi_{i}} \delta(t - \tau_i)}
$$

Quali sono gli effetti della presenza di varie repliche dello stesso segnale. In alcuni casi le repliche che arrivano in ritardo rispetto al tempo di simbolo potrebbero causare interferenza intersimbolica. 

Ripassiamo un po' la teoria dei matched filters:

* Prendiamo in considerazione un sistema di comunicazione \texttt{PAM} caratterizzato dall'impulso sagomatore $\mathsf{g_{T}(t)}$ e dal filtro passa-basso di ricezione $\mathsf{g_{R}(t)}$. Sotto l'ipotesi di canale ideale, (l'inviluppo complesso del) segnale in uscita dal trasmettitore è il seguente:
$$
    \mathsf{\tilde{s}(t) = \sum_{n}a_{n}g_{T}(t - nT_S)}
$$

* Il segnale in ingresso al campionatore del ricevitore è rappresentato dalla seguente espressione:
$$
    \mathsf{r(t) = \sum_{n} a_{n} g(t - nT_S)}
$$
dove $\mathsf{g(t) = g_{T}(t) * h(t) * g_{R}(t)}$. Visto che siamo sotto l'ipotesi di canale ideale:
$$
    \mathsf{g(t) = g_{T}(t) * \delta(t) * g_{R}(t) = g_{T}(t) * g_{R}(t)}
$$

* Il compito del campionatore è di campionare, appunto, il segnale $\mathsf{r(t)}$ ad intervalli di $\mathsf{T_S}$ secondi. Dunque, il segnale in ingresso al decisore (omesso il rumore) è il seguente:
\begin{align*}
    \mathsf{r[m]} &= \mathsf{r(t)|_{t = mT_S}} = \mathsf{\sum_{n}a_{n}g(mT_S - nT_S)} = \\
    &= \mathsf{\sum_{n}a_{n}g[(m - n)T_S]} = \\
    & \mathsf{\text{definiamo } k = m - n} \\
    &= \mathsf{\sum_{k}a_{m-k}g(kT_S)} = \\
    &= \mathsf{a_{n}g(0) + \sum_{k \neq 0}a_{m-k}g(kT_S)}
\end{align*}

* Se non vogliamo avere $\mathsf{ISI}$, la catena $\mathsf{g(t) = g_{T}(t) * g_{R}(t) = \delta(t)}$. Da qui è facile implementare un trasmettitore con un impulso sagomatore adeguato, così come scegliere il filtro di ricezione tale che la condizione sopra presentata (condizione di Nyquist nel tempo) sia soddisfatta.

Non dimentichiamoci dell'elefante nella stanza: se l'ipotesi di canale ideale non è più valida? Supponiamo di avere a che fare con un canale *multipath*. In particolare, la risposta impulsiva del canale ora è la seguente:
$$
    \mathsf{h(t) = A_{LS} \sum_{i=0}^{N_c - 1} \alpha_{i} e^{j \phi_{i}} \delta(t - \tau_i)}
$$

Implementare un sistema $\mathsf{Tx}$-$\mathsf{Rx}$ che rispetti la condizione di Nyquist, ora, non è poi così tanto banale. L'influenza dei simboli $\mathsf{a_{m} \neq a_{n}}$ non dipende soltanto dal filtraggio implementato ma anche dalla natura del canale.

### Delay Spread

Abbiamo detto che il ricevitore riceve diverse repliche del segnale originale a causa della natura del canale. Quando queste repliche introducono disturbo, ovvero quando queste repliche influiscono negativamente sulla decisione presa sul campione $\mathsf{r[m]}$?

* Supponiamo che tutte le repliche del segnale arrivino *tutte* all'interno del periodo di simbolo $\mathsf{T_S}$. In questo caso, l'influenza delle repliche non si propaga sui campioni appartenenti a periodi di simboli successivi a quello corrente.

* Prendiamo in considerazione il caso opposto: le repliche del segnale associato al simbolo $\mathsf{a_{n}}$ arrivano anche dopo che il periodo di simbolo associato è terminato. In questo caso, la variabile decisionale associata a $\mathsf{a_{n+1}}$ dipende non solo da $\mathsf{a_{n+1}}$ ma anche $\mathsf{a_{n}}$, $\mathsf{a_{n-1}}$, eccetera.

\begin{figure}
    \centering
    \begin{tikzpicture}[scale=1]
        % Draw axis
        \draw[->] (-6,0) -- (6.5,0) node[right] {$t$};

        \draw [->, red] (-5.5,0) -- (-5.5,2);
        \draw [->, red] (-2.5,0) -- (-2.5,0.75);
        \draw [->, red] (0.75,0) -- (0.75,1.25);
        \draw [->, red] (3.25,0) -- (3.25,1.35);
        \draw [black, dotted, thick] (6,0) -- (6,2.5);

        % Add labels
        \foreach \x/\label in {-5.5/{t - nT_S}, -2.5/{t - nT_S - \tau_1}, 0.75/{t - nT_S - \tau_2}, 3.25/{t - nT_S - \tau_3}, 6/{T_S}}
            \node[below] at (\x,0) {$\label$};
    \end{tikzpicture}
    \caption{$\mathsf{y(t) = \sum_{i=0}^{N_c - 1} \alpha_{i} e^{j \phi_{i}} s (t - \tau_{i})}$, $\mathsf{\delta_T << T_S}$}
    \label{fig:delay_spread}
\end{figure} 

\begin{figure}
    \centering
    \begin{tikzpicture}[scale=1]
        % Draw axis
        \draw[->] (-6,0) -- (6.5,0) node[right] {$t$};

        \draw [->, red] (-5.5,0) -- (-5.5,2);
        \draw [->, red] (-2.5,0) -- (-2.5,0.75);
        \draw [->, red] (0.75,0) -- (0.75,1.25);
        \draw [->, red] (3.25,0) -- (3.25,1.35);
        \draw [black, dotted, thick] (-4.5,0) -- (-4.5,2.5);

        % Add labels
        \foreach \x/\label in {-5.5/{t - nT_S}, -2.5/{t - nT_S - \tau_1}, 0.75/{t - nT_S - \tau_2}, 3.25/{t - nT_S - \tau_3}, -4.5/{T_S}}
            \node[below] at (\x,0) {$\label$};
    \end{tikzpicture}
    \caption{$\mathsf{y(t) = \sum_{i=0}^{N_c - 1} \alpha_{i} e^{j \phi_{i}} s (t - \tau_{i})}$, $\mathsf{\delta_T > T_S}$}
    \label{fig:delay_spread}
\end{figure}

La dispersione del segnale causata dalla natura del canale è detta *delay spread* $\mathsf{\delta_T}$. Di fatto, l'effetto del ritardo introdotto dal canale su ciascuna replica causa la dispersione del segnale su un intervallo temporale in alcuni casi maggiore del tempo di simbolo $\mathsf{T_S}$. 

Come si stima $\mathsf{\delta_T}$? Il ritardo $\tau$ è una variabile aleatoria: stimare la dispersione del segnale nel dominio del tempo "coincide" con calcolare la deviazione standard della variabile aleatoria $\tau$.
$$
    \mathsf{\delta_T^{2} = E\{(\tau - \eta_{\tau})(\tau - \eta_{\tau})^{*}\} = E\{\tau^2\} - E\{\tau\}^2}
$$

Stimare la distribuzione di probabilità della variabile aleatoria $\tau$ è complicato. Seguiremo un approccio ingegneristico: approssiamo.
\begin{align*}
    \mathsf{\delta_T} &\approx \mathsf{\sqrt{\bar{\tau^{2}} - (\bar{\tau})^{2}}} \\ \\
    \mathsf{\bar{\tau^{2}}} &= \mathsf{\sum_{i=0}^{N_c - 1} \frac{\alpha_{i}^{2}}{\sum_{j=0}^{N_c - 1} \alpha_{j}^2} \tau_{i}^{2}} \\ \\
    \mathsf{\bar{\tau}} &= \mathsf{\sum_{i=0}^{N_c - 1} \frac{\alpha_{i}^{2}}{\sum_{j=0}^{N_c - 1} \alpha_{j}^2} \tau_{i}}
\end{align*}

### Coherence Bandwidth

La *coherence bandwidth* rappresenta l'intervallo di frequenze entro cui il canale può essere considerato "piatto" (costante), o in altre parole, l'intervallo di frequenze su cui è probabile che due frequenze di un segnale sperimentino un'attenuazione comparabile.

La coherence bandwidth può essere approssimata come $\mathsf{B_{c} \approx \frac{1}{5 \sigma_{\tau}}}$.

Cosa succede se mettiamo a paragone la banda del segnale $\mathsf{B_{s}}$ e la coherence bandwidth $\mathsf{B_{c}}$?

* Se $\mathsf{B_{c} >> B_{s}}$ ($\mathsf{\sigma_{\tau} << T_{s}}$) allora il canale può essere considerato constante in relazione alle frequenze occupate dal segnale trasmesso.

* Se $\mathsf{B_{c} \leq B_{s}}$ ($\mathsf{\sigma_{\tau} \geq T_{s}}$) allora il canale è detto *frequency selective* perchè alcune frequenze del segnale sperimentano attenuazioni diverse da altre (il canale non può più essere considerato constante in relazione alle frequenze occupate dal segnale trasmesso).

### BER: Gaussian vs Flat Fading vs Frequency Selective Channel

TODO


### Time Varing Channel

Se il ricevitore (trasmettitore o entrambi) è in *movimento*, l'attenuazione e lo sfasamento sperimentati dalle varie repliche del segnale possono variare nel tempo. In particolare, la risposta del canale cambia in funzione del tempo:
$$
	\mathsf{h(t, \tau)} = \mathsf{A_{LS} \sum_{k=0}^{N_{c}-1} \alpha_{k}(t) e^{j \phi_{k}(t)} h(t - \tau_{k})}
$$
Possiamo notare che i guadagni $\mathsf{\alpha_{k}}$ e le fasi $\mathsf{\phi_{k}(t)}$ cambiano molto più velocemente rispetto ad altri fenomeni (vedi *path loss* $\mathsf{A_{LS}}$  o ritardo $\mathsf{\tau}$).

Tutto ciò è dovuto all'effetto *Doppler*.

#### Effetto Doppler

L'effetto Doppler è un fenomeno fisico che consiste nel cambiamento apparente, rispetto al valore originario, della frequenza (quindi della lunghezza d'onda) percepita da un osservatore raggiunto da un'onda emessa da una sorgente che si trova in movimento rispetto all'osservatore stesso. Facciamo un esempio per capire meglio.

* Supponiamo di avere due osservati. Il primo osservatore si trova nel punto $\mathsf{x_1}$, mentre il secondo osservatore si trova nel punto $\mathsf{x_2 = x_1 + d}$. 

* Supponiamo che un trasmettitore in movimento a velocità costante $\mathsf{v}$ inizi a trasmettere un segnale sonoro quando si trova nel punto $\mathsf{x_1}$. Lo stesso trasmettitore smette di trasmettere il segnale sonoro quando si trova nel punto $\mathsf{x_2}$. 

* Il trasmettitore percepisce il segnale sonoro per un intervallo di tempo dato da $\mathsf{\Delta_T = \frac{d}{v}}$.

* Il primo osservatore, ovvero l'osservatore che si trova nel punto $\mathsf{x_1}$, percepisce il segnale sonoro per $\mathsf{\Delta_{obs1} = \frac{d}{v} + \frac{d}{v_s}}$, dove $\mathsf{v_s}$ è la velocità del suono.

* Il secondo osservatore, ovvero l'osservatore che si trova nel punto $\mathsf{x_2}$, percepisce il segnale sonoro per $\mathsf{\Delta_{obs2} = \frac{d}{v} - \frac{d}{v_s}}$.

L'esempio ci permette di capire che la durata del segnale dipende dal sistema di riferimento preso in considerazione. Se l'osservatore e la sorgente sono in movimento relativo, la durata del segnale aumenta/diminuisce a seconda della direzione del movimento.

Ora facciamo lo stesso esempio nel caso in cui si abbia un ricevitore in movimento a velocità costante $\mathsf{v}$ rispetto al trasmettitore.

* Supponiamo che il trasmettitore trasmetta un segnale sinusoidale diretto al ricevitore in movimento. In particolare $\mathsf{s(t) = cos(2 \pi f_c t)}$.

* Supponiamo che nell'instante di tempo $\mathsf{t}$ il ricevitore si trovi nel punto $\mathsf{x}$ e che veda il segnale $\mathsf{y_{x}(t) = cos(2 \pi f_c t)}$.

* Supponiamo ora che il ricevitore si trovi nell'istante di tempo $\mathsf{t}$ nel punto $\mathsf{\bar{x} = x + d}$. Ora, il segnale visto dal ricevitore è il seguente:
\begin{align*}
    \mathsf{y_{x+d}(t)} &= \mathsf{y_{x}(t - \Delta t) = y_{x} \left(t - \frac{d}{c} \right) = y_{x} \left(t - \frac{vt}{c} \right)} = \\
    &= \mathsf{cos \left(2 \pi f_c t - 2 \pi f_c \frac{v}{c}t \right) = cos \left( 2 \pi \left( f_c - \frac{v f_c}{t} \right) t \right)}
\end{align*}

Possiamo ben notare che la frequenza dei due segnali è diversa! Nel caso di multipath channel, ciò significa che un ricevitore in movimento riceverà diverse copie del segnale originario e ciascuna di queste avrà una frequenza diversa dall'originale.


### Doppler Spread

A causa dell'effetto doppler e del canale multipath, il ricevitore vede diverse repliche del segnale trasmesso, ciascuna di queste traslata in frequenza a causa dell'effetto Doppler.

Assumendo che l'energia ricevuta sia la stessa da tutte le direzioni (dispersione uniforme), il canale tempo-variante è modellato dallo spettro di Jakes.

\begin{figure}
    \centering
    \begin{tikzpicture}[scale=1]
        % Draw axis
        \draw[->] (-1,0) -- (5,0) node[right] {$f$};

        \draw [->, black] (-0,0) -- (0,2);
        \draw [->, red] (3,0) -- (3,1.25);
        \draw [->, red] (4.25,0) -- (4.25,1.25);

        % Add labels
        \foreach \x/\label in {3/{f_c}, 4.25/{f_c + f_d}}
            \node[below] at (\x,0) {$\label$};
    \end{tikzpicture}
    \caption{$\mathsf{h(t) = \delta(f - f_c)}$ : risposta impulsiva del canale ideale. $\mathsf{h(t) = \delta(f - f_c - f_d)}$ : componente associata al Doppler shift.}
    \label{fig:doppler_shift}
\end{figure}

A causa del doppler spread, la potenza del segnale trasmesso viene "dispersa" in frequenza. In altre parole, la potenza del segnale trasmesso viene "dispersa" intorno alla frequenza portante $\mathsf{f_c}$ dall'effetto Doppler.

Definiamo un indice, detto *doppler spread*, che indica di quanto la potenza del segnale trasmesso viene dispersa nell'intorno della frequenza portante $\mathsf{f_c}$:
$$
	\mathsf{Doppler \text{ } shift: \text{ } f_{d} = - \frac{vf_{c}}{c}}
$$
Nel caso di canale tempo-variante, quest'ultimo viene modellato come un processo aleatorio. Per studiare di quanto la potenza del segnale trasmesso si disperde in frequenza è necessario calcolare la densità spettrale di potenza del segnale stesso. Se il segnale trasmesso è $\mathsf{s(t) = cos(2 \pi f_c t)}$, allora la densità spettrale di potenza del segnale è al seguente:
$$
    \mathsf{DSP_{Doppler}(f) = \delta(f - f_c) * \frac{1}{\pi f_d} \left( \sqrt{ 1 - \frac{f^2}{f_d^2}} \right)^{-1} = \frac{1}{\pi f_d} \left( \sqrt{ 1 - \frac{(f - f_c)^2}{f_d^2}} \right)^{-1} }
$$

\begin{figure}
    \centering
    \begin{tikzpicture}
        \begin{axis}[
            xlabel={$f$},
            ylabel={$\mathsf{S(f)}$},
            axis lines=middle,
            width=6cm,
            height=6cm,
            legend pos=north east,
            samples=500,
            domain=1.75:4.25,
        ]

        \addplot[blue,smooth] {1/(pi*1) * (sqrt(1 - ((x-3)/1)^2))^(-1)};

        \addplot[white,smooth] {1};

        \end{axis}
    \end{tikzpicture}
    \caption{Effetto del doppler spread. Segnale trasmesso $\mathsf{s(t) = \cos(2 \pi f_c t)}$}
    \label{fig:doppler_spread}
\end{figure}

Può essere dimostrato che, per un segnale $\mathsf{PAM}$ (o $\mathsf{QAM}$), la densità spettrale di potenza del segnale ricevuto può essere calcolata come:
$$
	\mathsf{S_{y}(f) = S_{s}(f) * S_{D}(f)}
$$

### Channel's Coherence time

Il tempo di coerenza del canale $\mathsf{T_c}$ è definito come l'intervallo di tempo durante il quale il canale può essere approssimato come costante.

Il tempo di coerenza $\mathsf{T_c}$ è il duale nel dominio temporale del *Doppler spread* $\mathsf{f_d}$ ed è utilizzato per caratterizzare la natura variabile nel tempo della dispersione in frequenza del canale. $\mathsf{f_d}$ e il tempo di coerenza sono inversamente proporzionali l'uno all'altro. Vale a dire, $\mathsf{T_c \approx 1/f_d}$.

In particolare, sappiamo che la trasformata inversa di Fourier di $\mathsf{S_{D}(f)}$ può essere calcolata come $\mathsf{J_{0}(2 \pi f_{d} t)}$, dove $\mathsf{J_{0}(x)}$ è una funzione di Bessel di ordine zero. 

In pratica, possiamo pensare che il tempo di coerenza indica l'intervallo temporale per il quale il canale può essere considerato "incorrelato".
$$
	\mathsf{J_{0}(2 \pi f_{d} t) \approx 0 \text{ } \rightarrow f_{d} t = \frac{1}{2} \rightarrow f_{d} T_c = \frac{1}{2} \rightarrow T_c = \frac{1}{2 f_d}}
$$



\newpage{}



# Software Defined Radio (SDR)

Il ricevitore \texttt{SDR} è composto da due blocchi: un blocco realizzato in hardware ed un blocco realizzato in software. Andiamo ad analizzare il primo blocco (Figura \ref{fig:SDR_hw}):

1. Il segnale ricevuto per mezzo dell'antenna viene preliminarmente filtrato con un filtro passa-banda. Il filtraggio compiuto dal filtro passa-banda è utile per eliminare tutte le componenti del segnale che si trovano al di fuori della banda del segnale stesso.

2. Il ricevitore in questione è di tipo *heterodyne*. Rispetto ad un ricevitore *homodyne*, il processo di demodulazione avviene in due step. In realtà non si tratta di una vera e propria demodulazione ma di uno shift in frequenza del segnale ricevuto. Questa operazione viene svolta per shiftare il segnale ad una frequenza intermedia $\mathsf{f_{if} < f_c}$ in modo tale che l'amplificazione eseguita in seguito sia più efficace. Dunque il segnale $\mathsf{y(t)}$ diventa $\mathsf{\bar{y}(t) = y(t) \cdot cos(2 \pi (f_c - f_{if})t)}$.
\begin{align*}
    \mathsf{\bar{y}(t)} &= \mathsf{y(t) \cdot cos(2 \pi (f_c - f_{if})t)} = \\
    &= \mathsf{(m_{I}(t)cos(2 \pi f_c t) - m_{Q}sin(2 \pi f_c t)) \cdot cos(2 \pi (f_c - f_{if})t) } = \\
    &= \mathsf{m_{I}(t)cos(2 \pi f_c t)cos(2 \pi (f_c - f_{if})t) - m_{Q}sin(2 \pi f_c t)cos(2 \pi (f_c - f_{if})t) } \\ \\
    \mathsf{\bar{y}_{I}(t)} &= \mathsf{m_{I}(t)\frac{1}{2} \left( cos(2 \pi f_{if} t) + cos(2 \pi (2f_c - f_{if})t) \right)} \\
    \mathsf{\bar{y}_{Q}(t)} &= - \mathsf{m_{Q}\frac{1}{2} \left( sin(2 \pi f_{if} t) + sin(2 \pi (2f_c - f_{if})t) \right) } 
\end{align*}

3. Lo shift in frequenza ha prodotto due repliche del segnale a $\mathsf{2f_c - f_{if}}$. Per questo motivo abbiamo bisogno di filtrare il segnale $\mathsf{\bar{y}(t)}$ con un filtro passa-basso.

4. Il segnale viene amplificato (\texttt{Low Noise RF Amplifier}).

5. Dopo essere stato nuovamente filtrato con un filtro anti-alias, il segnale viene campionato ad una frequenza $\mathsf{f_s >> f_{if}}$ (condizione di Nyquist rispettata).

6. I campioni in entrata al modulo software sono i seguenti:
$$
    \mathsf{r[m] = r(t)|_{t = m/f_{s}} = m_{I}(t) cos \left( 2 \pi \frac{f_{if}}{f_s} m \right) - m_{Q} sin \left( 2 \pi \frac{f_{if}}{f_s} m \right)}
$$

Il modulo software si occupa della demodulazione finale dei campioni \texttt{I/Q}. In particolare, la demodulazione viene eseguita moltiplicando ciascuno campione in uscita dal modulo hardware con un valore estratto dal segnale sinusoidale associato al ramo da cui proviene il campione (nota: i segnali demodulatori hanno frequenza $\mathsf{\frac{f_{if}}{f_s}}$). 

\begin{figure}
    \centering
    \begin{tikzpicture}[>=stealth, node distance=2cm]

    % Definizione dei blocchi
    \node [draw, regular polygon, regular polygon sides=3, shape border rotate=90] (antenna) {};
    \node [draw, rectangle, below of=antenna, xshift=1cm] (FPB) {PBF};
    \node [draw, rectangle, right of=FPB] (cos) {$\mathsf{cos(2 \pi f_{lo}t)}$};
    \node [draw, rectangle, right of=cos] (FLP) {LPF};
    \node [draw, rectangle, right of=FLP] (ampl) {Ampl.};
    \node [draw, rectangle, right of=ampl] (alias) {Anti-Alias};
    \node [draw, rectangle, right of=alias] (adc) {\texttt{ADC}};

    % Definizione delle frecce
    \draw [->] (antenna) -- node[anchor=south, xshift=-0.25cm] {$\mathsf{y(t)}$} ++(-0.5,0) |- (FPB);
    \draw [->] (FPB) -- (cos);
    \draw [->] (cos) -- (FLP);
    \draw [->] (FLP) -- (ampl);
    \draw [->] (ampl) -- (alias);
    \draw [->] (alias) -- (adc);

    \end{tikzpicture}
    \caption{Diagramma a blocchi di un ricevitore SDR (parte hardware).}
    \label{fig:SDR_hw}
\end{figure}

Supponiamo di voler ricevere un segnale \texttt{FM} mediante un ricevitore SDR. Il segnale demodulato (e già campionato) si presenta così:
$$
    \mathsf{v[n] = A_c e^{j 2 \pi k_{f} \int_{- \infty}^{n/f_s} m(\tau) d\tau} \approx A_c e^{j 2 \pi k_{f} \sum_{k=-\infty}^{n} m(k)T_s}}
$$
Visto e considerato l'approsimazione presentata sopra, notiamo che il rapporto tra un campione ed il precedente produce il seguente risultato: 
$$
    \mathsf{\frac{v[n]}{v[n-1]} = A_{c} e^{j 2 \pi k_{f} \sum_{k=-\infty}^{n} m(k)T_s} \cdot A_{c} e^{- j 2 \pi k_{f} \sum_{k=-\infty}^{n-1} m(k)T_s} = A_{c}^{2} e^{j 2 \pi k_{f} m(n)T_s}}
$$

Ecco qua: per ricavare una stima del segnale $\mathsf{m(t)}$ è sufficiente estrarre la fase del rapporto tra un campione ed il precedente!.
$$
    \mathsf{m(nT_s) \approx \frac{1}{2 \pi k_{f} T_s} \angle \left[ \frac{v[n]}{v[n-1]} \right] }
$$



\newpage{}



# Pulse Amplitude Modulation (PAM)

Andiamo a studiare come funziona un sistema $\mathsf{Tx}$-$\mathsf{Rx}$ PAM.

Trasmettitore: praticamente uguale ad un trasmettitore AM. L'unica differenza sta nel segnale inviato: la sorgente a monte del trasmettitore PAM produce bits (segnale digitale) ma noi vogliamo inviare un segnale analogico (se si inviasse un segnale discreto, come ad esempio un treno di impulsi, la banda del segnale sarebbe infinita). Dunque, ciò che si fa è inviare un segnale ottenuto dall'interpolazione dei campioni da trasmettere mediante un segnale analogico, quest'ultimo detto *impulso sagomatore*. Il segnale in uscita dal trasmettitore PAM è il seguente:
$$
    \mathsf{s(t) = \sum_{n=-\infty} a_{n} g_{T}(t - nT_s) cos(2 \pi f_c t)}
$$
Ricevitore (Figura \ref{fig:PAM_Rx}):

1. Il segnale in ingresso al ricevitore è la somma del segnale trasmesso più il rumore bianco gaussiano introdotto dal canale, ovvero $\mathsf{y(t) = \sum_{n=-\infty} a_{n} \bar{g} (t - nT_s)cos(2 \pi f_c t) + n(t)}$, dove $\mathsf{\bar{g}(t) = g_{T}(t) * h(t)}$. Attenzione: stiamo lavorando sotto l'ipotesi di canale ideale, ovvero $\mathsf{h(t) \delta(t)}$. Ciò implica che il canale non introduce distorsioni sul segnale trasmesso.

2. L'operazione di demodulazione, come ben sappiamo, riporta il segnale in banda base e produce due repliche del segnale centrate (nel dominio della frequenza) in $\mathsf{-2f_c}$ e $\mathsf{2f_c}$. Una degli obiettivi del filtro di ricezione $\mathsf{g_{R}(t)}$ è eliminare le componenti in banda passante del segnale in uscita del demodulatore. Il segnale $\mathsf{x(t)}$ è il seguente:
$$
    \mathsf{x(t) = \sum_{n=-\infty}a_{n}g(t - nT_s) + w(t)}
$$
dove $\mathsf{g(t) = g_{T}(t) * h(t) * g_{R}(t)}$ e $\mathsf{w(t) = n(t) * g_{R}(t)}$.

3. Il campionamento è eseguito alla stessa frequenza di trasmissione $\mathsf{f_s = \frac{1}{T_s}}$, I campioni in uscita dal decisore sono dunque i seguenti:
$$
    \mathsf{x[m] = x(t)|_{t = mT_s} = \sum_{n=-\infty}a_{n}g(mT_s - nT_s) + w(mT_s)}
$$

\begin{figure}
    \centering
    \begin{tikzpicture}[>=stealth, node distance=2cm]

    % Definizione dei blocchi
    \node [draw, regular polygon, regular polygon sides=3, shape border rotate=90] (antenna) {};
    \node [draw, rectangle, below of=antenna, xshift=0.75cm] (FPB) {PBF};
    \node [draw, rectangle, right of=antenna, xshift=0.75cm] (gen_cos) {$\mathsf{2cos(2 \pi f_{c}t)}$};
    \node [draw, circle, right of=FPB] (cos) {$\mathsf{\times}$};
    \node [draw, rectangle, right of=cos] (FLP) {$\mathsf{g_{R}(t)}$};
    \node [draw, rectangle, right of=FLP] (adc) {\texttt{ADC}};
    \node [draw, rectangle, right of=adc] (dec) {\texttt{Dec}};

    % Definizione delle frecce
    \draw [->] (antenna) -- node[anchor=south, xshift=-0.25cm] {$\mathsf{y(t)}$} ++(-0.5,0) |- (FPB);
    \draw [->] (gen_cos) -- (cos);
    \draw [->] (FPB) -- (cos);
    \draw [->] (cos) -- node[yshift=0.25cm] {$\mathsf{r(t)}$} (FLP);
    \draw [->] (FLP) -- node[yshift=0.25cm] {$\mathsf{x(t)}$} (adc);
    \draw [->] (adc) -- node[yshift=0.25cm] {$\mathsf{x[m]}$} (dec);

    \end{tikzpicture}
    \caption{Diagramma a blocchi di un ricevitore PAM.}
    \label{fig:PAM_Rx}
\end{figure}

## Densità spettrale di potenza del segnale $\mathsf{s_{PAM}(t)}$

Sia $\mathsf{\bar{s}_{PAM}(t)}$ il segnale
$$
    \mathsf{\bar{s}_{PAM}(t) = \sum_{n = -\infty} a_{n}g_{T}(t - nT_s)cos(2 \pi f_c t)}
$$
Sia $\mathsf{s_{PAM}(t)}$ l'inviluppo complesso del segnale $\mathsf{\bar{s}_{PAM}(t)}$, ovvero
$$
    \mathsf{s_{PAM}(t) = \sum_{n = -\infty} a_{n}g_{T}(t - nT_s)}
$$
La densità spettrale di potenza del segnale $\mathsf{s_{PAM}(t)}$ può essere calcolata come
$$
    \mathsf{DSP_{PAM}(f) = \frac{S_{a}(f)}{T_s}|G_{T}(f)|^2}
$$
dove $\mathsf{S_{a}(f)}$ è la densità spettrale di potenza della sequenza di simboli $\mathsf{\{a_{n}\}}$ e $\mathsf{G_{T}(f)}$ è la trasformata di Fourier della risposta impulsiva del filtro LTI $\mathsf{g_{T}(t)}$.

Ricordando che la sequenza di simboli $\mathsf{\{a_{n}\}}$ è un processo aleatorio *stazionario, discreto, indipendente e a media nulla*, possiamo calcolare la densità spettrale di potenza della sequenza $\mathsf{\{a_{n}\}}$ sfruttando il teorema di **Wiener**-**Kintchine**:
\begin{align*}
    \mathsf{S_{a}(f)} &= \mathcal{F}\mathsf{\{R_{aa}(m)\}} \\
    \mathsf{\{R_{aa}(m)\}} &= E\{a_{m} a_{m + k} \}
\end{align*}
Il processo aleatorio $\mathsf{\{a_{n}\}}$ è indipendente: questo significa che possiamo calcolare l'autocorrelazione come il prodotto del valore medio della variabile $\mathsf{a_{m}}$ ed il valore medio della variabile $\mathsf{a_{m+k}}$:
\begin{align*}
    \mathsf{E\{R_{aa}(m)\}} &= 
    \begin{cases}
        \mathsf{E\{a_{m}^{2}\} \text{ se } k = 0} \\
        \mathsf{E\{a_{m}\} E\{a_{m + k}\} \text{ se } k \neq 0}
    \end{cases}
\end{align*}
Inoltre. visto che $\mathsf{\{a_{n}\}}$ è stazionario, il valor medio del processo è costante per tutte le realizzazioni del processo.
\begin{align*}
    \mathsf{E\{R_{aa}(m)\}} &= 
    \begin{cases}
        \mathsf{E\{a_{m}^{2}\} \text{ se } k = 0} \\
        \mathsf{E\{a_{m}\} E\{a_{m}\} \text{ se } k \neq 0}
    \end{cases}
\end{align*}
Ora possiamo sfruttare l'ultima proprietà del processo: $\mathsf{\{a_{n}\}}$ è un processo a media nulla!
\begin{align*}
    \mathsf{E\{R_{aa}(m)\}} &= 
    \begin{cases}
        \mathsf{E\{a_{m}^{2}\} \text{ se } k = 0} \\
        \mathsf{0 \text{ se } k \neq 0}
    \end{cases} \\
    \mathsf{E\{R_{aa}(m)\}} &= \mathsf{E\{a_{m}^{2}\}\delta[k]} \rightarrow \mathsf{S_{a}(f)} = \mathsf{E\{a_{m}^{2}\}}
\end{align*}
Dunque, la densità spettrale di potenza del segnale $\mathsf{s_{PAM}(t)}$ può essere espressa come segue:
$$
    \mathsf{DSP_{PAM}(f) = \frac{E\{a_{m}^{2}\}}{T_s}|G_{T}(f)|^2}
$$
Dall'ultima espressione ottenuta per $\mathsf{DSP_{PAM}(f)}$, è facile notare che la banda del segnale dipende dal filtro passa-basso usato per interpolare la sequenza di simboli.




\newpage{}



# Potenza statica e dinamica in un CMOS

La potenza dissipata in una porta logica CMOS è definita come segue:
$$
    \mathsf{P = C_{L}V_{DD}^2f_{0 \rightarrow 1} + t_{sc}V_{DD}I_{peak}f_{0 \rightarrow 1} + V_{DD}I_{leakage}}
$$
In particolare, possiamo esprimere la potenza dissipata come la somma di due termini:
\begin{align*}
    \mathsf{P_{dynamic}} &= \mathsf{C_{L}V_{DD}^2f_{0 \rightarrow 1} + t_{sc}V_{DD}I_{peak}f_{0 \rightarrow 1}} \\
    \mathsf{P_{static}} &= \mathsf{V_{DD}I_{leakage}}
\end{align*}

Analizziamo le due componenti della potenza dinamica:

* $\mathsf{C_{L}V_{DD}^2f_{0 \rightarrow 1}}$ è la potenza dinamica "vera e propria", ovvero la potenza dissipata durante l'utilizzo della porta logica.

* $\mathsf{t_{sc}V_{DD}I_{peak}f_{0 \rightarrow 1}}$ è la potenza dissipata a causa di un fenomeno transitorio. In particolare, quando la tensione vista all'ingresso della porta logica cambia, come ben sappiamo, il passaggio dal livello logico basso al livello logico alto non è instantaneo. Durante il transitorio sia la pull-down network sia la pull-up network si trovano in conduzione: ecco perchè si dissipa potenza.

Come si può diminuire il consumo energetico in un CMOS?

Prendiamo in considerazione la potenza dinamica, in particolare la potenza dissipata a causa dell'attività di switching. Abbiamo 3 componenti:

1. $\mathsf{C_{L}}$. Tale capacità è funzione del numero di output dell'archittetura (fan-out): maggiore il numero di uscite, maggiore la capacità $\mathsf{C_{L}}$.

2. $\mathsf{V_{DD}}$. La tensione di alimentazione rappresenta una componente molto importante per diminuire il consumo energetico: diminuisce quadraticamente! Esistono vari metodi per diminuire la tensione di alimentazione:

    - $\mathsf{Multi}$-$\mathsf{VDD}$: all'interno della rete combinatoria non tutte le porte logiche sono alimentate con la stessa tensione $\mathsf{V_{DD}}$. In particolare, si sceglie di utilizzare delle porte logiche che adoperano tensioni di alimentazioni minori solo nei percorsi non critici. Perchè? Diminuire la tensione di alimentazione causa il peggioramento dei tempi di propagazione (esempio: $\mathsf{t_{p, HL} \propto \frac{K \cdot C}{\beta_{n}(V_{DD} - V_{Tn})}}$).

Un altro modo per diminuire il consumo energetico è adoperare un controllo dinamico della temperatura del chip: *dynamic thermal management*.

La **Dynamic Thermal Management** (DTM) è una strategia utilizzata nei sistemi elettronici per gestire dinamicamente la temperatura operativa al fine di migliorare le prestazioni, il consumo energetico e la sicurezza del sistema.
Il DTM consiste nelle seguenti operazioni:

1. Monitoraggio in tempo reale della temperatura. La strategia DTM prevede il costante monitoraggio della temperatura. Ciò può essere fatto attraverso l'uso di sensori termici distribuiti in punti critici del sistema.

2. Tecniche di mitigazione termica. Quando il sistema rileva un aumento della temperatura, vengono attivate diverse tecniche di mitigazione termica. Queste possono includere la regolazione dinamica della frequenza di clock, la variazione delle tensioni di alimentazione, la modulazione della velocità delle ventole di raffreddamento o addirittura la disattivazione temporanea di alcune componenti non essenziali.

Esempio di **Dynamic Voltage Scaling** (DFS): SpeedStep di Intel.

- Hardware che riduce la frequenza del clock quando l'utente si scollega dall'alimentazione elettrica.

- Diminuzione della frequenza di clock da $\mathsf{650MHz}$ a $\mathsf{500MHz}$.

- La CPU si arresta durante l'adattamento di SpeedStep.

Esempio di strategia che adotta sia DFS sia DVS: LongRun di Transmeta.

- Hardware che applica sia DFS che DVS:
    * 32 livelli di VDD da $\mathsf{1,1V}$ a $\mathsf{1,6V}$.
    * Frequenza di clock da $\mathsf{200MHz}$ a $\mathsf{700MHz}$ ad incrementi di $\mathsf{33MHz}$.

- Il sistema si attiva quando viene rilevato un cambiamento del carico della CPU dal software:
    * Carico in aumento $\mathsf{\rightarrow}$ aumenta $\mathsf{V_{DD}}$ e, quando $\mathsf{V_{DD}}$ stabile, aumenta la frequenza di clock.
    * Carico in diminuzione $\mathsf{\rightarrow}$ rallenta la frequenza di clock; quando il PLL si sincronizza con la nuova frequenza, riduce $\mathsf{V_{DD}}$. La CPU si ferma solo durante la transizione da una frequenza a quella successiva (< 20 microsecondi).
