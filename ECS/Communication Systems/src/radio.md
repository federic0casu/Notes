---
title: Radio Communications
author: Federico Casu
date: Novembre 04, 2023
header-includes: | 
	\usepackage{subcaption}
	\usepackage{tikz}
	\usepackage{tcolorbox}
---


# Analog Communications

## Amplitude Modulation (AM)

La modulazione d'ampiezza è la modulazione analogica più semplice che esista. Il suo funzionamento è il seguente:

> Trasmettitore: $\mathsf{s(t) = A_{c} \cdot m(t) \cdot \cos(2 \pi f_{c} t)}$

> Ricevitore: $\mathsf{m(t) = A_{c} \cdot s(t) \cdot 2\cos(2 \pi f_{c} t)}$

Studiamo nel dettaglio l'aspetto del segnale trasmesso $\mathsf{s(t)}$. In particolare, studiamo lo spettro del modulo di $\mathsf{s(t)}$[^1].
\begin{align*}
	\mathcal{F}\{s(t)\} &= \mathcal{F}\{A_{c} \cdot \mathsf{m(t)} \cdot \mathsf{cos}(2 \pi f_{c} t)\} = \\
	&= A_{c} \cdot  \mathcal{F}\{ \mathsf{m(t)} \cdot \mathsf{cos}(2 \pi f_{c} t)\} = \\
	&= A_{c} \cdot  \mathcal{F}\{ \mathsf{m(t)} \cdot \frac{e^{j 2 \pi f_{c} t} + e^{-j 2 \pi f_{c} t}}{2}\} = \\
	&= A_{c} \cdot  \mathcal{F}\{ \mathsf{m(t)} \cdot \frac{e^{j 2 \pi f_{c} t}}{2} + \mathsf{m(t)} \cdot \frac{e^{-j 2 \pi f_{c} t}}{2}\} \\
	&= \frac{A_{c}}{2} \cdot  \mathcal{F}\{ \mathsf{m(t)} \cdot e^{j 2 \pi f_{c} t}\} + \frac{A_{c}}{2} \cdot \mathcal{F}\{ \mathsf{m(t)} \cdot e^{-j 2 \pi f_{c} t}\}
\end{align*}
Per proseguire lo studio del segnale $\mathcal{F}\{s(t)\}$ è utile ricordare il **teorema della modulazione nel tempo**: 

> Per ogni $f_{0} \in \mathbb{R}$, $\mathcal{F}\{x(t) \cdot e^{j 2 \pi f_{0} t}\} = X(f - f_{0})$, dove $X(f) = \mathcal{F}\{x(t)\}$. 

Possiamo sfruttare il teorema della modulazione nel tempo per calcolare $\mathcal{F}\{\mathsf{m(t)} \cdot e^{j 2 \pi f_{c} t}\}$ e $\mathcal{F}\{\mathsf{m(t)} \cdot e^{-j 2 \pi f_{c} t}\}$:
\begin{align*}
	\mathcal{F}\{s(t)\} &= \frac{A_{c}}{2} \cdot M(f - f_c) + \frac{A_{c}}{2} \cdot M(f + f_c)
\end{align*}

[^1]: Lo spettro del segnale $\mathsf{s(t) = A_{c} \cdot m(t) \cdot \cos(2 \pi f_{c} t)}$ è composto solo dalla componente del modulo in quanto la fase, essendo un segnale reale, è nulla in tutto il dominio della frequenza.

![**(a)** Spettro del segnale $\mathsf{s(t)}$ prima della modulazione. **(b)** Spettro del segnale $\mathsf{s(t)}$ dopo la modulazione.\label{fig:AM_spectrum}](figures/radio/AM_spectrum.png){width="75%"}

Quali sono gli effetti, in frequenza, del prodotto con il coseno (Figura \ref{fig:AM_spectrum})?

1. La **banda** del segnale modulato è *due* volte la banda del segnale in banda base. In Figura \ref{fig:AM_spectrum}, la banda del segnale in banda base è $\mathsf{W}$ mentre la banda del segnale modulato è $\mathsf{2W}$.
2. Grazie alla **simmetria hermitiana**[^2], lo spettro del segnale in banda base, $\mathsf{M(f)}$, è simmetrico rispetto all'origine. La modulazione, che non è altro che il prodotto tra il segnale $\mathsf{m(t)}$ e $\mathsf{cos(2 \pi f_{c} t)}$, produce due copie esatte del segnale in banda base di cui una centrata a frequenza $\mathsf{-f_{c}}$ e l'altra centrata a frequenza $\mathsf{f_{c}}$. Da ciò segue che anche il segnale modulato è simmetrico rispetto all'origine (la modulazione non influisce sulla simmetria hermitiana). Inoltre, il segnale modulato è simmetrico rispetto alla frequenza portante.

[^2]: *Simmetria hermitiana*: se $\mathsf{m(t)}$ è reale allora $\mathsf{M(-f) = M^{*}(f)}$.  $\mathsf{|M(-f)| = |M^{*}(f)|}$.

Traslare un segnale in banda base ad una frequenza diversa da $\mathsf{f=0}$ è necessario per almeno *due* motivi:

1. Se tutte le trasmissioni avenissero in banda base allora i segnali trasmessi interferirebbero tra di loro.
2. Minore è la frequenza di trasmissione, maggiori le dimensioni dell'antenna necessari per trasmettere tali segnali. 

Come si può recuperare il segnale modulante, *i.e.* il segnale che porta informazione? Sono necessarie tre operazioni:

1. Filtraggio mediante un *filtro passa banda*. Un filtro passa banda rimuove tutte le componenti del segnale in input che si trovano a frequenze al di fuori dell'intervallo $\mathsf{[f_{c}-B, f_{c}+B]}$, dove $\mathsf{B}$ è la banda del filtro in banda base.
2. Demodulazione. L'operazione di demodulazione "riporta" il segnale modulato nella sua versione in banda base.
3. Filtraggio mediante un *filtro passa basso*. La demodulazione produce, oltre che il segnale modulante, altre due copie del segnale: una copia centrata a $\mathsf{-2 f_{c}}$ e l'atra copia centrata a $\mathsf{2 f_{c}}$. Mediante il filtro passa basso il ricevitore è in grado di isolare la componente del segnale in banda base dalle componenti *ridondanti* in banda passante.

Demodulazione:
\begin{align*}
	\mathsf{m_{R}(t)} &= \mathsf{s(t)} \cdot 2 \mathsf{cos}(2 \pi f_{c} t) = \mathsf{A_{c} \cdot m(t)} \cdot \mathsf{cos}(2 \pi f_{c} t) \cdot 2 \mathsf{cos}(2 \pi f_{c} t) = \\
	&= \mathsf{A_{c} \cdot m(t)} \cdot 2(\frac{1}{2} + \frac{\mathsf{cos}(2 \pi 2 f_{c} t)}{2}) = \\
	&= \mathsf{A_{c} \cdot m(t)} + \mathsf{A_{c} \cdot m(t)} \cdot \mathsf{cos}(2 \pi 2 f_{c} t) \\ \\
	\mathcal{F}\{\mathsf{m_{R}(t)}\} &= \mathcal{F}\{ \mathsf{A_{c} \cdot m(t)} + \mathsf{A_{c} \cdot m(t)} \cdot \mathsf{cos}(2 \pi 2 f_{c} t) \} = \\
	&= \mathcal{F}\{ \mathsf{A_{c} \cdot m(t)} \} + \mathcal{F}\{ \mathsf{A_{c} \cdot m(t)} \cdot \mathsf{cos}(2 \pi 2 f_{c} t) \} = \\
	&= A_{c} \cdot M(f) + \frac{A_{c}}{2}M(f - 2 f_{c}) + \frac{A_{c}}{2}M(f + 2 f_{c})
\end{align*}

---

## Analog Quadrature Amplitude Modulation (QAM)

La modulazione AM è poco efficiente. Ciò è una diretta conseguenza della *simmetria hermitiana*: il segnale modulante $\mathsf{m(t)}$, essendo reale, quando viene modulato si sdoppia in due componenti identiche centrate a $\mathsf{f_{c}}$ e $\mathsf{-f_{c}}$ per mantenere la proprietà di simmetria. La simmetria, seppur sia una proprietà matematicamente elegante, è inefficiente dal punto di vista dell'occupazione della banda: il segnale modulato occupa il doppio della banda rispetto al segnale in banda base.

I sistemi di comunicazione QAM sfruttano l'ortogonalità del seno e del coseno per raddoppiare il contenuto informativo trasmesso nell'unità di tempo. 

![Schema di trasmissione/ricezione di un sistema QAM analogico.\label{fig:analog_QAM}](figures/radio/analog_QAM.png)

Trasmettitore: $\mathsf{s(t)} = \mathsf{s_{I}(t) \cdot cos(2 \pi f_{c} t)} - \mathsf{s_{Q}(t) \cdot sin(2 \pi f_{c} t)}$

Lato ricevitore il segnale $\mathsf{s(t)}$ viene demodulato sia per mezzo di $\mathsf{2cos(2 \pi f_{c} t)}$ sia $\mathsf{-2sin(2 \pi f_{c} t)}$. Il risultato dell'operazione di demodulazione è il seguente:
\begin{align*}
    \mathsf{s_{I}(t)} &= \mathsf{s(t)} \cdot \mathsf{2cos(2 \pi f_{c} t)} = \\
    &= [\mathsf{s_{I}(t) \cdot cos(2 \pi f_{c} t)} - \mathsf{s_{Q}(t) \cdot sin(2 \pi f_{c} t)}] \cdot \mathsf{2cos(2 \pi f_{c} t)} = \\
    &= \mathsf{s_{I}(t) \cdot cos(2 \pi f_{c} t) \cdot 2cos(2 \pi f_{c} t)} - \mathsf{s_{Q}(t) \cdot sin(2 \pi f_{c} t) \cdot 2cos(2 \pi f_{c} t)} 
\end{align*}
Applicando le formule di Werner[^3] all'equazione di sopra otteniamo il seguente segnale:
\begin{align*}
    &= \mathsf{s_{I}(t) \cdot cos(0)} + \mathsf{s_{I}(t) \cdot cos(2 \pi (2f_{c}) t)} - \mathsf{s_{Q}(t) \cdot sin(0)} - \mathsf{s_{Q}(t) \cdot sin(2 \pi (2f_{c}) t) } = \\
    &= \mathsf{s_{I}(t)} + \mathsf{s_{I}(t) \cdot cos(2 \pi (2f_{c}) t)} - \mathsf{s_{Q}(t) \cdot sin(2 \pi (2f_{c}) t)}
\end{align*}

[^3]: Abbiamo utilizzato $cos(\alpha)cos(\beta) = \frac{1}{2}cos(\alpha + \beta) + \frac{1}{2}cos(\alpha - \beta)$ e $sin(\alpha)cos(\beta) = \frac{1}{2}sin(\alpha + \beta) + \frac{1}{2}sin(\alpha - \beta)$.

Il segnale demodulato è dunque composto da tre componenti:

- Il segnale originario $\mathsf{s_{I}(t)}$, quest'ultimo in banda base.
- Una copia del segnale $\mathsf{s_{I}(t)}$ traslata a $2f_c$.
- Una copia del segnale $\mathsf{s_{Q}(t)}$ traslata a $2f_c$.

Grazie al filtro passa-basso siamo in grado di isolare il segnale in banda base dalle copie (quest'ultime in banda passante) ottenute in fase di demodulazione. L'analisi per il segnale $\mathsf{s_{Q}(t)}$, proveniente dal ramo in *quadratura*, è del tutto analoga a quella appena compiuta per il segnale proveniente dal ramo in *fase*, *mutatis mutandis*.

Ovviamente c'è un prezzo da pagare: nei calcoli fatti sopra abbiamo assunto che i segnali sinusoidali generati dal trasmettitore sono *perfettamente sincronizzati* con i segnali generati dal ricevitore. In pratica, è quasi impossibile ottenere una sincronia di fase perfetta tra trasmettitore e ricevitore. Se è presente uno *sfasamento* allora non è più possibile ricavare una copia fedele del segnale originario a partire dal segnale trasmesso $\mathsf{s(t)}$.

Inoltre, seppur il segnale modulato continua ad essere *reale* (quindi simmetrico rispetto l'origine), con la modulazione QAM si perde la simmetria intorno la frequenza portante. Quest'ultima è una conseguenza della modulazione mediante due segnali sinusoidali ortogonali. 


\newpage{}


## Complex envelope

L'inviluppo complesso di un segnale passa banda è una rappresentazione equivalente traslata in banda base. 

Perchè studiamo l'inviluppo complesso?

1. Studiare l'inviluppo complesso è più semplice di studiare il segnale originale in banda passante. Questo perchè tramite l'inviluppo complesso è possibile "eliminare" dal discorso la frequenza portante.
2. Simulare numericamente l'inviluppo complesso di un segnale passa banda è più semplice visto che la frequenza di campionamento è molto minore rispetto alla frequenza di campionamento del segnale passa banda originale.

Ogni segnale passa banda può essere rappresentato come
\begin{align*}
	\mathsf{s(t)} &= \mathit{Re}\{\mathsf{\tilde{s}(t) \cdot e^{j 2 \pi f_c t}}\} = \\
	&= \mathit{Re}\{\mathsf{[s_{I}(t) + j \cdot s_{Q}(t) ] \cdot [cos(2 \pi f_c t) + j \cdot sin(2 \pi f_c t)]}\} = \\
	&= \mathsf{s_{I}(t) \cdot cos(2 \pi f_c t) - s_{Q}(t) \cdot sin(2 \pi f_c t)}
\end{align*}
dove $\mathsf{\tilde{s}(t) = s_{I}(t) + j \cdot s_{Q}(t)}$ è l'*inviluppo complesso* del segnale $\mathsf{s(t)}$.

---

## Frequency Modulation (FM)

Nella modulazione di frequenza il contenuto informativo è incorporato nella fase del segnale. Il segnale trasmesso è il seguente:
\begin{align*}
	\mathsf{s_{FM}(t) = A_{c} \cdot cos(2 \pi f_{c} t + 2 \pi k_{f} \int^{t}_{- \infty}m(\tau)d\tau)}
\end{align*}

L'inviluppo complesso del segnale $\mathsf{s_{FM}(t)}$ è il seguente:
\begin{align*}
	\mathsf{\tilde{s}_{FM}(t)} = \mathsf{A_{c} \cdot e^{j 2 \pi k_{f} \int^{t}_{- \infty}m(\tau)d\tau}}
\end{align*}

Andiamo a studiare la *frequenza istantanea*[^4] del segnale $\mathsf{s_{FM}(t)}$:
\begin{align*}
	\mathsf{f_{i}(t)} &= \mathsf{\frac{1}{2 \pi} \frac{d \phi(t)}{dt}} =  \\
	&= \mathsf{\frac{1}{2 \pi} \frac{d}{dt}(2 \pi f_c t + 2 \pi k_{f} \int^{t}_{- \infty}m(\tau)d\tau)} = \\
	&= \mathsf{f_{c} + k_{f}m(t)}
\end{align*}

[^4]: La frequenza istantanea è definita come la derivata rispetto al tempo della fase del segnale, *i.e.* $\mathsf{f_{i}(t) = \frac{1}{2 \pi} \frac{d \phi(t)}{dt}}$.

A partire dalla frequenza istantanea, calcoliamo la *frequency deviation*, cioè quanto il segnale si scosta dalla frequenza portante:
\begin{align*}
	\mathsf{f_{d}(t)} = \mathsf{f_{i}(t) - f_{c} = k_{f} m(t)}
\end{align*}

Possiamo notare che, il contenuto informativo da trasmettere, oltre che essere incorporato nella fase del segnale, influisce sullo spettro del segnale: al variare del segnale $\mathsf{m(t)}$, varia la frequenza del segnale portante e di conseguenza varia la sua rappresentazione nel dominio di Fourier. Le modulazioni viste in precedenza (AM e QAM) eseguivano una modulazione d'ampiezza, ovvero incorporavano l'informazione da trasmettere nell'ampiezza del segnale trasmesso. Inoltre, le modulazioni d'ampiezza producono segnali la cui occupazione di banda nel dominio della frequenza è costante nel tempo: stimare la banda di tali segnali è semplice. Con la modulazione di frequenza il gioco è ben diverso: ora la modulazione è ad *ampiezza constante*[^5] mentre ciò che varia nel tempo è la fase del segnale trasmesso. Quest'ultima conseguenza rende difficile stimare l'occupazione di banda del segnale trasmesso. Perché? Il motivo è semplice: non esiste una forma chiusa della trasformata di Fourier del segnale $\mathsf{s_{FM}(t)}$.

[^5]: Una modulazione è detta ad *ampiezza costante* quando l'ampiezza del segnale è costante durante tutta la durata delle trasmissione. 

Studiamo l'occupazione di banda del segnale $\mathsf{s_{FM}(t)}$ mediante un esempio: consideriamo il seguente segnale modulante $\mathsf{m(t) = V_{m} cos(2 \pi f_{m} t)}$.

Il segnale trasmesso dal trasmettitore $\mathsf{FM}$ è il seguente:
\begin{align*}
	\mathsf{s_{FM}(t)} &= \mathsf{A_{c} cos(2 \pi f_c t + 2 \pi k_f \int^{t}_{- \infty} V_{m}cos(2 \pi f_m \tau))} = \\
	&= \mathsf{A_{c} cos(2 \pi f_c t + 2 \pi k_f \cdot \frac{V_m}{2 \pi f_m} sin(2 \pi f_m t))} = \\
	&= \mathsf{A_{c} cos(2 \pi f_c t + \frac{k_f \cdot V_m}{f_m} sin(2 \pi f_m t))}
\end{align*}

Definiamo due indici che ci saranno utili:

1. *Maximum frequency deviation*: $\mathsf{\Delta f = \max \left\{ | f_{d}(t) | \right\} = k_f \cdot \max \left \{ |m(t)| \right\}}$
2. *Modulation index*: $\mathsf{m_{f} = \frac{\Delta f}{B}}$, dove $\mathsf{B}$ è la banda del segnale modulante.

Possiamo notare che, nello sviluppo del segnale $\mathsf{s_{FM}(t)}$, $\mathsf{k_f \cdot V_m}$ coincide con la *maximum frequency deviation* del segnale modulante. Inoltre, ricordando che la banda in frequenza di un segnale sinusoidale è data da $\mathsf{f_m}$, il segnale $\mathsf{s_{FM}(t)}$ può essere riscritto come segue: $$\mathsf{s_{FM}(t) = A_{c} cos(2 \pi f_c t + m_f sin(2 \pi f_m t))}$$

Studiamo l'inviluppo complesso del segnale $\mathsf{s_{FM}(t)}$: $$\mathsf{ \tilde{s}_{FM}(t) = A_{c} \cdot e^{j m_f sin(2 \pi f_{m} t)}}$$

L'inviluppo complesso $\mathsf{\tilde{s}_{FM}(t)}$ è *periodico*. Possiamo rappresentare il segnale $\mathsf{\tilde{s}_{FM}(t)}$ mediante somma di coefficienti di Fourier:
\begin{align*}
	\mathsf{\tilde{s}_{FM}(t)} &= \mathsf{\sum_{n} S_{n} e^{j 2 \pi f_m n t}}
\end{align*}

![Possiamo notare che all'aumentare del modulo di $\mathsf{m_f}$, aumenta il numero di coefficienti $\mathsf{J_{n}(m_f) \neq 0}$.](figures/radio/bessel_function.png){width="50%"}

Non daremo la dimostrazione ma sappiamo che i coefficienti di Fourier $\mathsf{S_n}$ possono essere calcolati come $\mathsf{J_{n}(m_f)}$, dove $\mathsf{J_{n}(x)}$ è una funzione di Bessel del primo tipo di ordine $\mathsf{n}$.
\begin{align*}
    \mathsf{s_{FM}(t)} &= \mathsf{\Re\{\tilde{s}_{FM}(t) e^{j 2 \pi f_c t}\}} = \\
    &= \mathsf{\Re\{A_{c} \sum J_{n}(m_f) e^{j 2 \pi (f_c + n f_m) t}\}} = \\
    &= \mathsf{\Re\{A_{c} \sum J_{n}(m_f) [cos(2 \pi (f_c + n f_m) t) + jsin(2 \pi (f_c + n f_m) t)]\}} = \\
    &= \mathsf{A_{c} \sum J_{n}(m_f) \, cos(2 \pi (f_c + n f_m) t)} = \\
    \mathsf{\mathcal{F}\{s_{FM}(t)\}} &= \mathsf{A_{c} \sum J_{n}(m_f) \cdot [\frac{1}{2}\delta(f - f_c - n f_m) + \frac{1}{2}\delta(f + f_c + n f_m)]}
\end{align*}

![$\mathsf{\mathcal{F}\{s_{FM}(t)\}}$ \label{fig:Fourier_series_FM_signal}](figures/radio/Fourier_series_FM_signal.png){width="50%"}

Lo spettro del segnale trasmesso consiste in un *treno di delta* (Figura \ref{fig:Fourier_series_FM_signal}): è molto difficile stimare la banda del segnale perché il contributo in frequenza del treno di delta può proseguire all'infinito! Possiamo comunque notare che:

1. Le componenti del treno di delta non nulle dipendono dai coefficienti di Fourier $\mathsf{J_{n}(m_{f})}$.
2. La distanza tra una componente e l'altra è data dalla frequenza del segnale modulante $\mathsf{f_m}$.

Dalle considerazioni fatte sopra possiamo dedurre che la banda del segnale modulato dipende da $\mathsf{m_{f}}$.

> **Carson's Rule**: $\mathsf{B_{FM} \approx 2(m_f + 1)B = 2(\Delta f + B)}$

Il segnale modulante può essere recuperato differenziando la fase del segnale modulato. Il ricevitore assume la forma presentata in Figura \ref{fig:FM_rc}.

![Ricevitore $\mathsf{FM}$.\label{fig:FM_rc}](figures/radio/FM_rc.png){width="60%"}

---

## Software Defined Radio

**S**oftware **D**efined **R**adio (**SDR**) è una tecnologia che permette di costruire apparati radio non più strettamente realizzati come puro hardware, ovvero con circuiti e dispositivi elettronici, ma basati su un software in funzione su un PC o un sistema embedded.

In particolare vengono utilizzati ricevitori radio adatti a ricevere uno spettro di frequenze molto ampio e software appositi per gestire le funzioni di modulazione e demodulazione dei segnali. Col metodo SDR si ha un apparato riprogrammabile ogni qualvolta si vuole gestire uno standard di comunicazione diverso. 

Vediamo come funziona il modulo radio (Figura \ref{fig:SDR_hw}):

1. La prima operazione compiuta dal modulo ricevitore è filtrare il segnale proveniente dall'antenna mediante un filtro passa-banda. Grazie a questa operazione, come abbiamo già studiato in precedenza, siamo in grado di isolare le frequenze del segnale centrate in $\mathsf{f_c}$ ed "attenuare" le componenti al di fuori dell'intervallo di frequenze $\mathsf{[f_c - B, f_c + B]}$, dove $\mathsf{B}$ è la banda del segnale che siamo interessati a ricevere.
2. Il ricevitore è diverso rispetto a quello a cui siamo abituati (*homodyne*). I ricevitori di tipo *heterodyne* prendono il segnale proveniente dall'antenna (quest'ultimo centrato alla frequenza portante $\mathsf{f_c}$) e lo traslano ad una frequenza intermedia $\mathsf{0 < f_{if} < f_{c}}$. Traslare il segnale ricevuto ad una frequenza inferiore alla frequenza portante è necessario per semplificare l'amplificazione del segnale. Vediamo, matematicamente, come avviene questa operazione:
\begin{align*}
	\mathsf{f_{lo}} &= \mathsf{f_c - f_{if}} \\
	\mathsf{\bar{s}(t)} &= \mathsf{s(t) cos(2 \pi f_{lo} t)} = \\
	&= \mathsf{[m(t)cos(2 \pi f_c t) - m(t)sin(2 \pi f_c t)]cos(2 \pi f_{lo} t)} = \\
	&= \mathsf{m(t)cos(2 \pi f_c t)cos(2 \pi f_{lo} t) - m(t)sin(2 \pi f_c t)cos(2 \pi f_{lo} t)} = \\
	&= \mathsf{m(t)cos(2 \pi [f_c - f_{lo}] t)} + \mathsf{m(t)cos(2 \pi [f_c + f_{lo}] t)} - \mathsf{m(t)cos(2 \pi [f_c + \frac{\pi}{2} - f_{lo}] t)} - \\
	- & \mathsf{m(t)cos(2 \pi [f_c + \frac{\pi}{2} + f_{lo}] t)} = \\
	&= \mathsf{m(t)cos(2 \pi f_{if} t) + m(t)cos(2 \pi [2 f_c - f_{if}] t)} - \mathsf{m(t)cos(2 \pi [\frac{\pi}{2} + f_{if}] t)} - \\
	- & \mathsf{m(t)cos(2 \pi [2f_c + \frac{\pi}{2} - f_{if}] t)} = \\
	&= \mathsf{m(t)cos(2 \pi f_{if} t) + m(t)cos(2 \pi [2 f_c - f_{if}] t)} \mathsf{ - m(t)sin(2 \pi f_{if} t)} - \\
	- & \mathsf{m(t)sin(2 \pi [2 f_c - f_{if}] t)}
\end{align*}
3. Il segnale $\mathsf{\bar{s}(t)}$ passa attraverso un filtro passa basso che rimuove le componenti del segnale a frequenza $\mathsf{2 f_c - f_{if}}$.
4. Ora il segnale può essere amplificato (*Low Noise RF Amplifier* in Figura \ref{fig:SDR_hw}).
5. Il segnale viene campionato rispettando il teorema di Shannon. In particolare, il segnale $\mathsf{\bar{s}(t)}$ è centrato alla frequenza $\mathsf{f_{if} = 3.57 \text{ } MHz}$. La frequenza di campionamento è $\mathsf{f_{adc} = 28.8 \text{ } MHz}$. Il teorema di Shannon è ampiamente rispettato visto che $\mathsf{f_{adc} >> 2 (f_{if} + B)}$. Il segnale campionato ha la seguente forma: $$\mathsf{\bar{s}[n] = \bar{s}(nT_{s}) = \bar{s}(n / f_{adc})= m(n / f_{adc})cos(2 \pi \frac{f_{if}}{f_{adc}} n) - m(n / f_{adc})sin(2 \pi \frac{f_{if}}{f_{adc}} n)}$$
7. I campioni $\mathsf{I/Q}$ vengono prodotti in seguito alla demodulazione del segnale campionato. Le operazioni di demodulazione sono del tutto uguali a quelle viste per un sistema QAM. L'unica differenza che si può notare è nella forma dei segnali: non abbiamo segnali continui ma segnali discreti! Di conseguenza, il segnale $\mathsf{\bar{s}[n]}$ è moltiplicato per $\mathsf{2cos(2 \pi \frac{f_{if}}{f_{adc}} n)}$ e $\mathsf{-2sin(2 \pi \frac{f_{if}}{f_{adc}} n)}$ per ottenere i campioni $\mathsf{I}$ e $\mathsf{Q}$, rispettivamente.

![Schema a blocchi delle operazioni eseguite dal modulo radio in tecnologia SDR sul segnale ricevuto in banda passante.\label{fig:SDR_hw}](figures/radio/SDR_hw.png)

Una volta ottenuti i campioni $\mathsf{I \text{ e } Q}$, da quest'ultimi andremo ad estrarre il contenuto informativo trasmesso. Come si fa? Ricordiamo che, un trasmetittore FM *imprime* il contenuto informativo nella *fase* del segnale trasmesso. In sostanza, dobbiamo studiare un modo per estrarre la fase del segnale ricevuto dai campioni $\mathsf{I}$ e $\mathsf{Q}$.

L'inviluppo complesso del segnale in entrata al modulo software è il seguente:
\begin{align*}
	\mathsf{\tilde{v}[n]} &= \mathsf{v(n T_{s}) = v(n \frac{1}{f_{adc}})} = \\
	&= \mathsf{A_{c} e^{j 2 \pi k_f \int_{- \infty}^{n T_s}m(\tau)d\tau}} \approx \mathsf{A_{c} e^{j 2 \pi k_f \sum_{k = - \infty}^{n}m(k)T_s}}
\end{align*}

Visto e considerato l'approsimazione presentata sopra, notiamo che il rapporto tra un campione ed il precedente produce il seguente risultato: 
$$
    \mathsf{\frac{v[n]}{v[n-1]} = A_{c} e^{j 2 \pi k_{f} \sum_{k=-\infty}^{n} m(k)T_s} \cdot A_{c} e^{- j 2 \pi k_{f} \sum_{k=-\infty}^{n-1} m(k)T_s} = A_{c}^{2} e^{j 2 \pi k_{f} m(n)T_s}}
$$

Ecco qua: per ricavare una stima del segnale $\mathsf{m(t)}$ è sufficiente estrarre la fase del rapporto tra un campione ed il precedente!.
$$
    \mathsf{m(nT_s) \approx \frac{1}{2 \pi k_{f} T_s} \angle \left[ \frac{v[n]}{v[n-1]} \right] }
$$



\newpage{}



# Digital Communications

I sistemi di comunicazione studiati fino ad ora sono *analogici*: i segnali trasmessi sono continui nel tempo. Come possiamo inviare segnali digitali? Un segnale digitale, purtroppo, è un segnale la cui banda è \underline{infinita}. 

Non è fattibile inviare la sequenza di bits così come si presenta (ovvero un segnale discreto). L'idea che risolve i nostri problemi è molto semplice: trasformiamo la sequenza di bits in un segnale continuo. Tale operazione è detta *interpolazione*.

Dunque, lo schema a blocchi del trasmettitore è il seguente:

\begin{figure}[ht]
    \centering
    \begin{tikzpicture}
        % g_T(t)
        \draw (0,0) rectangle (2,2);
        \node at (1,1) {$\mathsf{g_{T}(t)}$};
        
        % Input bit sequence
        \draw[->] (-5,1) -- (0,1) node[midway, above] {$\mathsf{\{d_k\} = \sum_{n} d_n \delta (t - n T_{b})}$};
        
        % Output continuous signal
        \draw[->] (2,1) -- (7,1) node[midway, above] {$\mathsf{s(t) = \sum_{n} d_n g_{T} (t - n T_{b})}$};
    \end{tikzpicture}
    \caption{Interpolazione dei bit}
    \label{fig:dig_to_analog}
\end{figure}

Attenzione: in generale, per risparmiare energia è meglio trasmettere informazioni a media *nulla* \footnote{Si parla di segnale a media nulla perché i segnali digitali vengono modellati attraverso processi stocastici discreti. In seguito verranno studiati i processi stocastici e perché sono utili per lo studio dei sistemi di comunicazione digitali.}. Per risolvere questo problema possiamo applicare un ulteriore trasformazione ad ogni bit della sequenza:
\begin{equation*}
    0 \, \longrightarrow -1, \, \, 1 \, \longrightarrow 1
\end{equation*}
\noindent Il ragionamento fatto sopra può essere esteso per un insieme di bit. Di fatto, stiamo associando ad una sotto-sequenza di bit un \textit{simbolo}: l'unità di trasmissione, ora, non è più il bit ma è il simbolo.

\begin{figure}[ht]
    \centering
    \begin{tikzpicture}
        \draw (0,0) rectangle (2,2);
        \node at (1,1) {$\mathsf{MAP}$};
        
        % Input bit sequence
        \draw[->] (-2,1) -- (0,1) node[midway, above] {$\mathsf{\{d_k\}}$};
        
        % Output symbol sequence
        \draw[->] (2,1) -- (7,1) node[midway, above] {$\mathsf{\{a_i\} = \sum_{n} a_n \delta (t - n T_{s})}$};
    \end{tikzpicture}
    \caption{Mapping bits $\rightarrow$ simbolo.}
    \label{fig:bit_to_symbol}
\end{figure}

La frequenza di trasmissione dei simboli (*symbol rate*) è \underline{minore} della frequenza di generazione dei bits (*bit rate*). Data la durata del bit $T_b$, poiché ogni simbolo mappa $m$ bit, il tempo del simbolo $T_s$ è:
\begin{equation*}
    \mathsf{T_{s} = m \, T_{b} \, \longrightarrow \, R_{s} = \frac{1}{T_{s}} = \frac{1}{m T_{b}} \leq \frac{1}{T_{b}} = R_{b}}
\end{equation*}

\begin{figure}[ht]
    \centering
    \begin{tikzpicture}[scale=0.95]
        \draw (0,0) rectangle (2,2);
        \node at (1,1) {$\mathsf{MAP}$};
        \draw[->] (-2,1) -- (0,1) node[midway, above] {$\mathsf{\{d_k\}}$};
        \draw[->] (2,1) -- (4,1) node[midway, above] {$\mathsf{\{a_i\}}$};

        \draw (6,0) rectangle (4,2);
        \node at (5,1) {$\mathsf{g_{T}(t)}$};
        \draw[->] (6,1) -- (10.5,1) node[midway, above] {$\mathsf{s(t) = \sum_{n} a_n g_{T} (t - n T_{s})}$};
    \end{tikzpicture}
    \caption{Interpolazione dei simboli}
    \label{fig:interp_symbol}
\end{figure}

---

### Pulse Amplitude Modulation (PAM)

La **P**ulse **A**mplitude **M**odulation (**PAM**) è un tipo di modulazione in cui l'informazione (espressa in bits) è codificata nell'ampiezza di una serie di segnali. La modulazione PAM è ottenuta attraverso due operazioni:

1. Il mapping dei bits in simboli, *i.e.* ad ogni simbolo trasmesso è associata biunivocamente una sottosequenza di bits estratta dalla sequenza di bits da trasmettere. Il mapping dei bits è svolto da un modulo, detto *mapper*, che prende in input i bits $\mathsf{\{d_{k}\}}$ prodotti dalla sorgente e restituisce una sequenza di simboli $\mathsf{\{a_{i}\}}$. 
2. La conversione del segnale discreto composto dalla sequenza dei simboli in segnale analogico mediante l'utilizzo di un filtro passa-basso. Tale filtro è detto *impulso sagomatore*. Il segnale analogico ottenuto dall'interpolazione dei simboli assume la seguente forma: $\mathsf{\tilde{s}_{PAM}(t) = \sum_{n}a_{n}g_{T}(t - n T_s)}$.
3. La modulazione del segnale, *i.e.* la conversione del segnale $\mathsf{\tilde{s}_{PAM}(t)}$ in banda passante. Dunque, il segnale in uscita da un trasmettitore PAM è $\mathsf{s_{PAM} = \sum_{n}a_{n}g_{T}(t - n T_s)cos(2 \pi f_c t)}$.

\begin{figure}[ht]
    \centering
    \begin{tikzpicture}
        \draw (0,0) rectangle (2,2);
        \node at (1,1) {$\mathsf{MAP}$};
        \draw[->] (-2,1) -- (0,1) node[midway, above] {$\mathsf{\{d_k\}}$};
        \draw[->] (2,1) -- (4,1) node[midway, above] {$\mathsf{\{a_i\}}$};

        \draw (6,0) rectangle (4,2);
        \node at (5,1) {$\mathsf{g_{T}(t)}$};
        \draw[->] (6,1) -- (8,1) node[midway, above] {$\mathsf{\tilde{s}_{PAM}(t)}$};

        % Cerchio con X al suo interno
        \draw (8.3,1) circle (0.3);
        \node at (8.3,1) {\texttt{X}};
        \draw[->] (8.6,1) -- (10.5,1) node[midway, above] {$\mathsf{s_{PAM}(t)}$};
        
        % Freccia con etichetta
        \draw[<-] (8.3,1.3) -- (8.3,1.65) node[midway, above] {$\mathsf{\cos(2 \pi f_c t)}$};
    \end{tikzpicture}
    \caption{PAM - Trasmettitore}
    \label{fig:PAM_tr_}
\end{figure}

Il segnale PAM è equivalente al segnale AM-DSB, in cui il segnale modulante (e l'inviluppo complesso) è
\begin{equation*}
    \mathsf{m(t) = \sum_{n} a_n g_{T} (t - n T_{s}) = \tilde{s}_{PAM}(t)}
\end{equation*}

D'ora in avanti ci occuperamo di studiare il sistema PAM prendendo in considerazione l'inviluppo complesso del segnale $\mathsf{s_{PAM}(t)}$. Per semplicità, ometteremo il simbolo `~`: $\mathsf{s_{PAM}(t) = \sum_{n} a_{n} g_{T}(t - n T_s)}$.

Dal punto di vista del ricevitore, il segnale $\mathsf{s_{PAM}(t)}$ è un *processo stocastico*. Per capire l'occupazione di banda del segnale $\mathsf{s_{PAM}(t)}$ dovremo studiarne il suo spettro in frequenza. Invece, quando si ha a che fare con processi aleatori, lo studio della banda viene compiuto analizzando lo spettro della densità di potenza.

\begin{tcolorbox}[colback=blue!5!white,colframe=blue!75!white,title=\textcolor{black}{\textbf{Densità spettrale di potenza segnale PAM}}]
    \begin{equation*}
        \mathsf{DSP_{PAM}(f) = \frac{S_{a}(f)}{T_s}|G_{T}(f)|^{2}}
    \end{equation*}
    dove $\mathsf{T_s}$ è il periodo di emissione di un simbolo, $\mathsf{S_{a}(f)}$ è la densità spettrale di potenza della sequenza dei simboli $\mathsf{\{a_i\}}$ e $\mathsf{G_{T}(f) = \mathcal{F}\{g_{T}(t)\}}$.
\end{tcolorbox}

La densità spettrale di potenza della sequenza di simboli $\mathsf{\{a_{n}\}}$ può essere calcolata sfruttando il teorema di *Wiener-Kintchine*: la sequenza $\mathsf{\{a_{n}\}}$ è un processo stocastico $\mathsf{discreto}$, $\mathsf{stazionario}$ e $\mathsf{indipendente}$ per cui le ipotesi del teorema sono rispettate.
\begin{align*}
    \mathsf{S_{a}(f)} &= \mathsf{\mathcal{F}\{R_{aa}(m)\}} \\
    (1) \; \; \; \; \mathsf{R_{aa}(m)} &= \mathsf{E\{a_i a_{i+m}\}} = \text{indipendenza} =
        \begin{cases}
            \mathsf{E\{a_{i}^{2}\}} & \text{se } m = 0 \\
            \mathsf{E\{a_{i}\}E\{a_{i+m}\}} & \text{altrimenti} 
        \end{cases} \\ 
	&= \text{stazionarietà (in senso lato)} =  
	\begin{cases}
	    \mathsf{E\{a_{i}^{2}\}} & \text{se } m = 0\\
	    \mathsf{E\{a_{i}\}E\{a_{i}\}} & \text{altrimenti}
	\end{cases} \\
        &= \mathsf{E\{a_{i}^{2}\} \delta[m] + \sum_{k \neq 0} E\{a_{i}\}^{2} \delta[m+k]} \\
    (2) \; \; \; \; \; \; \mathsf{E\{a_{i}\}} &= \sum_{n=1}^{M} a_{n} \frac{1}{M} = \frac{1}{M} \sum_{n=1}^{M} (2n - 1 - M) = \\
    &= \frac{2}{M} \frac{M(M+1)}{2} - \sum_{n=1}^{M} \frac{M+1}{M} = (M+1) - (M+1) = 0 \rightarrow E\{a_{i}\}^{2} = 0\\
    (3) \; \; \; \mathsf{R_{aa}(m)} &= \mathsf{E\{a_{i}^{2}\} \delta[m]} \\
    (4) \; \; \; \; \; \, \mathsf{S_{a}(f)} &= \mathsf{\mathcal{F}\{R_{aa}(m)\} = E\{a_{i}^{2}\} = A}
\end{align*}

\begin{tcolorbox}[colback=green!5!white,colframe=green!75!white,title=\textcolor{black}{\textbf{Densità spettrale di potenza segnale PAM standard}}]
    \begin{equation*}
        \mathsf{DSP_{PAM}(f) = \frac{E\{a_{i}^{2}\}}{T_s}|G_{T}(f)|^{2}}
    \end{equation*}
\end{tcolorbox}

Dall'ultima espressione ottenuta per $\mathsf{DSP_{PAM}(f)}$, è facile notare che la banda del segnale dipende dal filtro passa-basso usato per interpolare la sequenza di simboli. Come ben sappiamo, se un segnale ha una durata finita del tempo allora occupa una banda infinita in frequenza (e viceversa). Come scegliamo l'impulso sagomatore?

Come al solito, ci troviamo di fronte ad un compromesso:

- spettro compatto $\mathsf{\rightarrow}$ grande quantità di interferenza nel dominio temporale (Scelta estrema: $\mathsf{rect}$ nel dominio delle frequenze e $\mathsf{sinc}$ nel tempo).
- spettro ampio $\mathsf{\rightarrow}$ la maggior parte dell'energia del simbolo è contenuta all'interno di un intervallo di simbolo (Scelta estrema: $\mathsf{rect}$ nel dominio temporale e $\mathsf{sinc}$ nelle frequenze).

**PAM - Ricevitore**. Prima di studiare come è strutturato il modulo ricevitore in una PAM, studiamo il sistema nella sua interezza introducendo le caratteristiche del \textit{canale}.

\begin{figure}[ht]
    \centering
    \includegraphics[width=0.85\textwidth]{figures/radio/pam.png}
    \caption{Schema a blocchi rappresentante un sistema di comunicazione PAM.}
    \label{fig:pam_system}
\end{figure}

- Il canale di propagazione è generalmente modellato come un filtro lineare tempo-invariante (LTI) con risposta impulsiva $\mathsf{h(t)}$. Quando il canale è ideale, allora $\mathsf{h(t) = \delta(t)}$.
- La componente di rumore $\mathsf{w(t)}$ è un processo stazionario gaussiano, a media nulla e bianco, con densità spettrale di potenza $\mathsf{S_{N}(f) = \frac{N_{0}}{2}}$.

Attenzione: lo schema a blocchi rappresentato in Figura \ref{fig:pam_system} è un modello equivalente in banda base (il segnale $\mathsf{s(t)}$ è, in realtà, l'inviluppo complesso del segnale trasmesso da `Tx`). Il segnale in entrata al ricevitore `Rx` è il seguente:
\begin{align*}
    \mathsf{r(t)} &= \mathsf{y(t) + w_{BP}(t)} = \\
    &= \mathsf{\tilde{s(t)} \circledast h(t) + n(t)} \\
    &= \mathsf{m(t)cos(2 \pi f_{c} t) \circledast h(t) + w_{BP}(t)} \\
    &= \mathsf{\sum_{n} a_{n} g_{T}(t - n T_{s}) cos(2 \pi f_{c} t) \circledast h(t) + w_{BP}(t)}
\end{align*} 

dove $\mathsf{w_{BP}(t)}$ è la versione traslata in banda passante del rumore $\mathsf{w(t)}$, *i.e*, $\mathsf{w_{BP}(t) = w(t)cos(2 \pi f_c t)}$.

L'equivalente in banda base del segnale $\mathsf{r(t)}$ è il seguente:
\begin{align*}
    \mathsf{r(t)} &= \mathsf{y(t) + w(t)} = \\
    &= \mathsf{s(t) \circledast h(t) + w(t)} \\
    &= \mathsf{m(t) \circledast h(t) + w(t)} \\
    &= \mathsf{\sum_{n}a_{n}g_{T}(t - nT_{s}) \circledast h(t) + w(t)}
\end{align*} 

\begin{figure}[ht]
    \centering
    \includegraphics[width=0.85\textwidth]{figures/radio/pam_rc_baseband.png}
    \caption{PAM - Ricevitore equivalente in banda base.}
    \label{fig:pam_rc_bb}
\end{figure}

Il ricevitore \texttt{Rx} PAM, per recuperare i bits trasmessi con $\mathsf{s(t)}$, esegue le seguenti operazioni:

1. Filtra il rumore e le interferenze dal segnale ricevuto $\mathsf{r(t)}$. Il filtraggio viene eseguito da un filtro passa-basso lineare e tempo-invariante (LTI) $\mathsf{g_{R}(t)}$.
2. Campiona il segnale filtrato $\mathsf{x(t)}$. Il periodo di campionamento coincide con il periodo del simbolo, \textit{i.e.}, $\mathsf{T_{s}}$.
3. Recupera i bit trasmessi dai campioni del segnale $\mathsf{x[m]}$. Tale operazione viene eseguita da un modulo chiamato *decisore*.

Studiamo il segnale in uscita dal filtro passa-basso $\mathsf{g_{R}(t)}$ (Figura \ref{fig:PAM_gr}):
\begin{align*}
	\mathsf{x(t)} &= \mathsf{r(t) \circledast g_{R}(t) + w(t) \circledast g_{R}(t)}    = \\
	&= \mathsf{s(t) \circledast h(t) \circledast g_{R}(t) + w(t) \circledast g_{R}(t)} = \\
	&= \mathsf{\sum_{n=0} a_{n} g_{T}(t - n T_s) \circledast h(t) \circledast g_{R}(t) + w(t) \circledast g_{R}(t)}
\end{align*}

Definiamo $\mathsf{n(t) = w(t) \circledast g_{R}(t)}$, ovvero il rumore gaussiano (ora non più bianco) presente nel segnale in uscita dal filtro di ricezione. Inoltre, se definiamo $\mathsf{g(t) = g_{T}(t) \circledast h(t) \circledast g_{R}(t)}$, il segnale $\mathsf{x(t)}$ può essere riscritto come:
\begin{align*}
	\mathsf{x(t)} = \mathsf{\sum_{n=0} a_{n} g(t - n T_s) + n(t)}
\end{align*}

\begin{figure}[ht]
    \centering
    \begin{tikzpicture}
        % Cerchio con X al suo interno
        \draw[->] (0.6,1) -- (2,1) node[midway, above] {$\mathsf{r(t)}$};

        \draw (2,0) rectangle (4,2);
        \node at (3,1) {$\mathsf{g_{R}(t)}$};
        \draw[->] (4,1) -- (5.5,1) node[midway, above] {$\mathsf{x(t)}$};
    \end{tikzpicture}
    \caption{PAM: filtro passa-basso.}
    \label{fig:PAM_gr}
\end{figure}

Ora vogliamo studiare l'aspetto del segnale in uscita dal campionatore, *i.e.* $\mathsf{x[m] = x(t)|_{t = m T_s}}$:
\begin{align*}
	\mathsf{x[m]} = \mathsf{\sum_{n} a_{n} g(m T_s - n T_s) + n(m T_s)}
\end{align*}

Per semplicità, per ora non consideriamo il rumore presente nel segnale $\mathsf{x[m]}$. In particolare prendiamo in considerazione il seguente segnale:
\begin{equation*}
	\mathsf{x[m] = \sum_{n=- \infty}^{+ \infty} a_{n}g(mT_s - nT_s) = \sum_{n=- \infty}^{+ \infty} a_{n}g((m-n)T_s)}
\end{equation*}

Definiamo l'indice $\mathsf{k=m-n}$:
\begin{equation*}
	\mathsf{x[m] = \sum_{k=- \infty}^{+ \infty} a_{m-k}g(kT_s) = a_{m}g(0) + \sum_{k, k \neq 0} a_{m-k}g(kT_s)}
\end{equation*}

Attenzione: il segnale $\mathsf{x[m]}$ è composto da due componenti ma soltanto una di queste può essere utilizzata per ricostruire il simbolo trasmesso all'istante $\mathsf{mT_s}$:

1. $\mathsf{a_{m}g(0)}$ componente utile.
2. $\mathsf{\sum_{k, k \neq 0} a_{m-k}g(kT_s)}$ componente non utile: *interferenza intersimbolica* (ISI).

Da queste considerazioni nasce il *criterio di Nyquist*: vorremo che la risposta nel tempo del filtro $\mathsf{g(t)}$ sia:
\begin{equation*}
    \mathsf{g(kT_s)} =
        \begin{cases}
            \mathsf{1} & \text{se } \mathsf{k = 0} \\
            \mathsf{0} & \text{altrimenti} 
        \end{cases}
\end{equation*}

Da cui segue $\mathsf{g(t) = \delta (t)}$. Se il criterio di Nyquist è rispettato allora, all'entrata del decisore, avremo la seguente sequenza:
\begin{equation*}
	\mathsf{x[m] = a_{m} + n(mT_s)}
\end{equation*}

Esiste una condizione che traduce il criterio di Nyquist in frequenza? Si, eccola qui:

1. Consideriamo il segnale $\mathsf{G(f) = \mathcal{F}\{g(t)\}}$.
2. Visto che campionare nel tempo causa periodicità nel dominio della frequenza, la trasformata di Fourier del segnale $\mathsf{g(m) = g(t)|_{t = mT_s}}$ è periodica.

\begin{equation*}
	\mathsf{\mathcal{F}\{g(m)\} = \frac{1}{T_s} \sum_{k} G(f - \frac{k}{T})}
\end{equation*}

Ora non ci rimane che sfruttare ciò che ci dice il criterio di Nyquist: $\mathsf{g(t) = \delta (t) \rightarrow \mathcal{F}\{g(t)\} = \mathcal{F}\{\delta (t)\} = 1 \text{ } \forall \text{ } f}$.
\begin{equation*}
	\mathsf{\mathcal{F}\{g(t)\} = \frac{1}{T_s} \sum_{k} G(f - \frac{k}{T_s}) = 1} 
\end{equation*}
oppure, equivalentemente:
\begin{equation*}
	\mathsf{\mathcal{F}\{g(t)\} = \sum_{k} G(f - \frac{k}{T_s}) = T_s} 
\end{equation*}

![Criterio di Nyquist (nel tempo ed in frequenza).](figures/radio/Nyquist_criterion.png){width="80%"}

Prima di poter studiare le caratteristiche in termini di banda, potenza ed energia per simbolo trasmesso del segnale PAM dobbiamo studiare come implementare un filtro che permette di *1)* avere zero-ISI e *2)* massimizzare il *signal-to-noise* ratio. 

**Raised Cosine Filters** - La funzione coseno rialzato è spesso utilizzata per l'implementazione di filtri passa-basso per la modulazione digitale grazie alla sua capacità di \underline{annullare} l'ISI. Il nome deriva dal fatto che la porzione non nulla dello spettro in frequenza della sua forma più semplice ($\beta=1$) è una funzione coseno, 'rialzata' di 1.

\begin{figure}[ht]
    \centering
    \begin{subfigure}{0.75\textwidth}
        \centering
        \includegraphics[width=\linewidth]{figures/radio/rc_time.png}
        \caption{Dominio del tempo.}
        \label{fig:rc_time}
    \end{subfigure}
    
    \begin{subfigure}{0.75\textwidth}
        \vspace{0.25cm}
        \centering
        \includegraphics[width=\linewidth]{figures/radio/rc_frequency.png}
        \caption{Dominio della frequenza}
        \label{fig:rc_frequency}
    \end{subfigure}
    \caption{Raised Cosine Filter}
    \label{fig:raised_cosine_filter}
\end{figure}

La banda del filtro è data dalla seguente espressione:
\begin{equation*}
    \mathsf{B_{RC}(\beta) = \frac{\beta + 1}{2T_s}}
\end{equation*}
dove $\mathsf{T_s}$ è il periodo di campionamento e $\mathsf{\beta}$ è detto fattore *roll-off*. Il fattore roll-off è un parametro di design e ci dice quanto rapidamente la risposta del filtro diventa nulla.

**Additive white Gaussian noise** - Trascurando, per il momento, l'effetto del canale, l'altro principale problema nel ricevitore è la presenza di rumore gaussiano bianco. La densità spettrale di potenza del rumore $\mathsf{w(t)}$  è costante in tutto il dominio della frequenza: $\mathsf{S_{w}(f) = \frac{N_{0}}{2}}$. La densità spettrale dell'inviluppo complesso di $\mathsf{w(t)}$, ovvero $\mathsf{\tilde{w}(t) = w^{I}(t) + jw^{Q}(t)}$, è la seguente:
\begin{equation*}
    \mathsf{S_{\tilde{w}}(f) = 2N_{0} \; \; \rightarrow \; \; S_{\tilde{w}}^{(I)}(f) = S_{\tilde{w}}^{(Q)}(f) = N_{0}}
\end{equation*}

L'azione del filtro $\mathsf{g_{R}(t)}$ cambia l'aspetto del rumore $\mathsf{w(t)}$: il risultato del filtraggio è sempre un processo aleatorio stazionario e gaussiano ma la densità spettrale di potenza non è più costante! La componente di rumore in uscita dal filtro $\mathsf{g_{R}(t)}$ non è più bianca:
\begin{align*}
    & \mathsf{S_{n}(f) = S_{w}(f)|G_{R}(f)|^2 = \frac{N_{0}}{2}|G_{R}(f)|^2} \\
    & \mathsf{S_{\tilde{n}}(f) = S_{\tilde{w}}(f)|G_{R}(f)|^2 = 2 N_{0} |G_{R}(f)|^2 \; \; \text{(inviluppo complesso)}} \\
\end{align*}

\begin{tcolorbox}[colback=green!5!white,colframe=green!75!white,title=\textcolor{black}{\textbf{Matched filters}}]
    Il filtro $\mathsf{g_{R}(t)}$ che \textbf{massimizza} il rapporto $\mathsf{signal}$-$\mathsf{to}$-$\mathsf{noise}$ (quindi minizza l'effetto del rumore) è dato da:
    \begin{equation*}
        \mathsf{g_{R}(t) = g_{T}(-t) \; \; \longrightarrow \; \; G_{R}(f) = G_{T}^{*}(f)}
    \end{equation*}    
\end{tcolorbox}

Se $\mathsf{g_{T}(t)}$ è \textit{pari}, allora $\mathsf{g_{R}(t) = g_{T}(t)}$.

Vediamo l'idea sul quale si basa il concetto di matched filters:

1. Prendiamo in esame la Figura \ref{fig:not_matched_filters}. Supponiamo di avere il filtro $\mathsf{G_{R}(f) = rect(f)}$. Come è facile notare, il filtro riesce a tagliare tutte le componenti di rumore che si trovano al di fuori della banda del segnale $\mathsf{S(f)}$. 
2. Tale approccio non è ottimale: quando il segnale $\mathsf{S(f)}$ ha ampiezza minore del rumore (esempio: nell'intervallo $\mathsf{[0.25, 0.50]}$), il compito del decisore diventa molto arduo perchè la componente di rumore è molto maggiore della componente informativa che si vorrebbe estrarre.
3. Analizziamo la Figura \ref{fig:matched_filters}. Il filtro è una esatta copia del segnale ricevuto (quindi è un'esatta copia dell'impulso sagomatore). Cosa possiamo notare? Nelle zone in cui il segnale è "meno forte" del rumore, il filtro riesce ad attenuare l'effetto del rumore, di fatto massimizzando il rapporto $\mathsf{signal}$-$\mathsf{to}$-$\mathsf{noise}$.

![\label{fig:not_matched_filters}](figures/radio/not_matched_filters.png){width="75%"}

![\label{fig:matched_filters}](figures/radio/matched_filters.png){width="75%"} 

**Root Raised Cosine Filter** - I filtri RRC sono filtri la cui risposta in frequenza è la radice quadrata di un coseno rialzato, \textit{i.e.}, $H_{RRC}(\beta, f) = \sqrt{H_{RC}(\beta, f)}$. Perché gli studiamo? Perché i filtri RRC sono delle implementazioni utili a rispettare il criterio dei \textit{matched filters}! Vediamo perché:
\begin{align*}
    \mathsf{G_{T}(f)} &= \mathsf{G_{R}(f) = H_{RCC}(\beta, f)} \\ \\
    \text{(1) Nyquist Criterion: } \mathsf{g(t)} &= \mathsf{g_{T}(t) * h(t) * g_{R}(t) = } \\
    &= \mathsf{h_{RRC}(\beta, t) * \delta(t) * h_{RRC}(\beta, t) = } \\
    &= \mathsf{h_{RRC}(\beta, t) *  h_{RRC}(\beta, t)} \\ \\
    \mathsf{\mathcal{F}\{h_{RRC}(\beta, t) *  h_{RRC}(\beta, t)\}} &= \mathsf{H_{RRC}(\beta, f) \cdot H_{RRC}(\beta, f) = H_{RRC}(\beta, f)^2} \\
    &= \mathsf{(\sqrt{H_{RC}(\beta, f)})^2 = H_{RC}(\beta, f)} \rightarrow \text{ Nyquist criterion OK } \\ \\
    \text{(2) Matched filters: } \; \; \; \; & \mathsf{H_{RRC}(\beta, f) \text{ is even } \longrightarrow G_{R}(f) = G_{T}^*(f) = G_{T}(f)} \text{ OK!}
\end{align*}

**Banda di un segnale PAM con filtraggio RRC** - La densità spettrale di potenza del segnale PAM, come visto in precedenza, è data dalla seguente espressione:
\begin{equation*}
    \mathsf{S_{s}(f) = \frac{1}{T_s} S_{a}(f) |G_{T}(f)|^2}
\end{equation*}

Ora, viste e considerate le proprietà della sequenza di simboli $\mathsf{\{a_{n}\}}$, possiamo dire che la banda di un segnale PAM dipende solamente dalla banda dell'impulso sagomatore. Vediamo perchè:
\begin{align*}
	\mathsf{R_{aa}(m)} &= \mathsf{E\{a_{i}a_{i+m}\}} \rightarrow \mathsf{indipendenza} \rightarrow \mathsf{E\{a_{i}\}E\{a_{i+m}\}} = \\
	&= 
	\begin{cases}
		\mathsf{E\{a_{i}^2\}} & \text{ } \mathsf{m = 0} \\
		\mathsf{E\{a_{i}\}^2} & \text{ } \mathsf{m \neq 0} \text{ } \rightarrow \text{ } \mathsf{stazionarietà}
	\end{cases} \\
	&= \mathsf{E\{a_{i}^2\} \delta[m] + \sum_{k \neq 0} E\{a_{i}\}^2 \delta[m+k]} \\ \\
	\mathsf{E\{a_{i}\}} &= \mathsf{0} \rightarrow \mathsf{E\{a_{i}\}^2 = 0} \\ \\ 
	\mathsf{E\{a_{i}^{2}\}} &= \frac{M^2 - 1}{3} \\ \\
	\mathsf{DSP_{PAM}(f)} &= \mathsf{\frac{E\{a_{i}^{2}\}}{T_{s}}|G_{T}(f)|^2}
\end{align*}

Supponiamo di implementare l'impulso sagomatore mediante l'utilizzo di $\mathsf{h_{RRC}(\beta, t)}$. Da ciò segue $$\mathsf{G_{T}(f) = H_{RCC}(\beta, f) \rightarrow DSP_{PAM}(f) = \frac{E\{a_{i}^{2}\}}{T_{s}}|H_{RRC}(f)|^2 = \frac{E\{a_{i}^{2}\}}{T_{s}} H_{RC}(f)}$$ La banda del segnale $\mathsf{H_{RC}(\beta, f)}$ è data dalla seguente espressione: $$\mathsf{Banda \text{ } del \text{ } segnale \text{ } H_{RC}(\beta, f) = \frac{\beta + 1}{2 T_{s}}}$$

Visto che abbiamo usato il segnale $\mathsf{H_{RC}(\beta, f)}$, quest'ultimo in banda base, per ottenere l'occupazione del segnale PAM in banda passante è sufficiente raddoppiare la banda del segnale in banda base, *cioè*, $$\mathsf{Banda \text{ } del \text{ } segnale \text{ } H_{RC}^{BP}(\beta, f) = \frac{\beta + 1}{T_{s}}}$$

Attenzione: la banda del segnale PAM dipende dal *bit-rate*. Vediamo come:
\begin{align*}
	\mathsf{f_s} &= \mathsf{\frac{1}{T_s}} \mathsf{\text{    } (symbol \text{ } rate)} \\
	\mathsf{T_s} &= \mathsf{log_{2}M \cdot T_b} \\
	\mathsf{f_{s}} &= \mathsf{\frac{1}{log_{2}M \cdot T_b} = \frac{R_{b}}{log_{2}M}} \\ \\
	\mathsf{Banda \text{ } del} & \text{ } \mathsf{segnale \text{ } H_{RC}(\beta, f)} = \mathsf{(\beta + 1)\frac{R_{b}}{log_{2}M}}
\end{align*}

**Potenza di un segnale PAM con filtraggio RRC** -  Esistono due modi per calcolare la potenza:

1. $\mathsf{\int_{- \infty}^{+ \infty}DSP_{PAM}(f)df}$

2. $\mathsf{R_{ss}(0)}$

Ovviamente la via più facile da percorrere è la seconda. In particolare, grazie al teorema di Wiener-Kintchine e alla biunivocità della trasformata continua di Fourier, sappiamo che l'antitrasformata della densità spettrale di potenza del segnale PAM è proprio l'autocorrelazione del segnale.
\begin{equation}\label{eq:DSP_PAM}
	\mathsf{\mathcal{F}^{-1}\{DSP_{PAM}(f)\}} = \mathsf{\mathcal{F}^{-1}\{\frac{E\{a_{i}^2\}}{T_s}|G_{T}(f)|^2\}}
\end{equation}

Supponiamo di applicare il principio di *matched filters* e di utilizzare $\mathsf{H_{RRC}(\beta, f)}$ come impulso sagomatore e filtro di ricezione. La densità spettrale di potenza del segnale PAM è dunque la seguente:
\begin{equation}\label{eq:H_RC}
	\mathsf{DSP_{PAM}(f) = \frac{E\{a_{i}^2\}}{T_s} H_{RC}(\beta, f)}
\end{equation}

Sostituiamo l'espressione ottenuta nell'Equazione \ref{eq:H_RC} dentro l'Equazione \ref{eq:DSP_PAM}. Il risultato è il seguente:
\begin{equation*}
	\mathsf{\mathcal{F}^{-1}\{DSP_{PAM}(f)\} = \mathcal{F}^{-1}\{ \frac{E\{a_{i}^2\}}{T_s} H_{RC}(\beta, f)\} = \frac{E\{a_{i}^2\}}{T_s} h_{RC}(\beta, t)}
\end{equation*}

La potenza del segnale PAM può essere calcolata come
\begin{equation*}
	\mathsf{\frac{E\{a_{i}^2\}}{T_s} h_{RC}(\beta, 0)} = \frac{E\{a_{i}^2\}}{T_s} = \frac{M^2 - 1}{3 T_s}
\end{equation*}

**Energia per simbolo trasmesso** - L'energia per simbolo è calcolata come la potenza del segnale a cui si moltiplica la durata del simbolo, *cioè*
\begin{equation*}
	\mathsf{E_{s} = P_s \cdot T_s = \frac{E\{a_{i}^2\}}{2T_s} \cdot T_s = E\{a_{i}^2\} = \frac{M^2 - 1}{6}}
\end{equation*}

**Strategia di decisione** - La nostra variabile decisionale è $\mathsf{x[m] = a_{m} + n[m]}$, dove

1. $\mathsf{a_{m}}$ è il simbolo trasmesso all'istante $\mathsf{m T_s}$ in assenza di ISI. A tale risultato ci siamo arrivati grazie a $\mathsf{g_{T}(t) = g_{R}(t) = h_{RRC}(\beta, t)}$ e all'ipotesi di canale ideale, $\mathsf{h(t) = \delta (t)}$.

2. $\mathsf{n[m]}$ è una variabile aleatoria ottenuta in seguito al campionamento del rumore filtrato. In altre parole, $\mathsf{n[m] = n(t)|_{t = m T_s} = [w(t) * g_{R}(t)]|_{t = m T_s}}$. 

Visto che $\mathsf{n(t)}$ è un processo aleatorio gaussiano a media nulla, allora $\mathsf{n[m]}$ è una variabile aleatoria gaussiana a media nulla. La varianza $\mathsf{\sigma_{n}^{2}}$ della variabile aletaoria $\mathsf{n[m]}$ può essere calcolata come $\mathsf{\sigma_{n}^{2} = E\{n[m]^2\} - E\{n[m]\}^2}$. Visto che $\mathsf{n[m]}$ è una variabile aleatoria a media nulla, allora la varianza $\mathsf{\sigma_{n}^{2}}$ coincide con il valore quadratico medio $\mathsf{E\{n[m]^2\}}$.
\begin{align*}
	\mathsf{E\{n[m]^2\}} &= \mathsf{E\{n[m]n^{*}[m]\}} = \mathsf{E\{n[m]n^{*}[m+k]\}}|_{k=0} = \mathsf{R_{nn}(k)|_{k=0}} \\ \\
	\mathsf{R_{nn}(k)|_{k=0}} &= \mathsf{\int_{- \infty}^{+ \infty} S_{n}(f) df} = \mathsf{ 2 N_{0} \int_{- \infty}^{+ \infty} |G_{R}(f)|^2 df}
\end{align*}

L'ultimo osservazione da fare è la seguente, visto che il filtro di ricezione è $\mathsf{h_{RRC}(\beta, t)}$, allora sappiamo calcolare $\mathsf{\int_{- \infty}^{+ \infty} |G_{R}(f)|^2}$:
\begin{equation*}
	\mathsf{\int_{- \infty}^{+ \infty} |G_{R}(f)|^2} = \mathsf{\int_{- \infty}^{+ \infty} H_{RC}(f)} = \mathsf{\int_{- \infty}^{+ \infty} H_{RC}(f) e^{j 2 \pi f t} \text{ } |_{t=0} = h_{RC}(\beta, 0) = 1}
\end{equation*}

Ora che abbiamo caraterizzato, *statisticamente*, la variabile decisionale $\mathsf{x[m]}$, possiamo definire la strategia decisionale che implementa il decisore.

\begin{tcolorbox}[colback=green!5!white,colframe=green!75!white,title=\textcolor{black}{\textbf{Optimal decision strategy}}]
    La strategia ottima è quella che sceglie il simbolo della costellazione $\mathsf{a^{(i)}}$ che $\mathsf{massimizza}$ la probabilità $\mathsf{P(a^{(i)} \text{ } | \text{ } x[m])}$.
    \begin{equation*}
        \mathsf{\hat{a}_{m} = arg \text{ } max \{ P(a^{(i)} \text{ } | \text{ } x[m]) \} } 
    \end{equation*}
\end{tcolorbox}

Calcolare la probabilità $\mathsf{P(a^{(i)} \text{ } | \text{ } x[m])}$ è molto difficile. Possiamo sfruttare il teorema di Bayes:
\begin{equation*}
	\mathsf{P(a^{(i)} \text{ } | \text{ } x[m]) = \frac{P(x[m] \text{ } | \text{ } a^{(i)})P(a^{(i)})}{P(x[m])}}
\end{equation*}

Se i simboli $\mathsf{a^{(i)}}$ sono equiprobabili, allora possiamo notare che la probabilità condizionata $\mathsf{P(a^{(i)} \text{ } | \text{ } x[m])}$ è proporzionale alla probabilità condizionata $\mathsf{P(x[m] \text{ } | \text{ } a^{(i)})}$: massimizzare $\mathsf{P(x[m] \text{ } | \text{ } a^{(i)})}$ equivale a massimizzare $\mathsf{P(a^{(i)} \text{ } | \text{ } x[m])}$!

\begin{tcolorbox}[colback=green!5!white,colframe=green!75!white,title=\textcolor{black}{\textbf{Maximum likelihood decision strategy}}]
    La strategia ottima è quella che sceglie il simbolo della costellazione $\mathsf{a^{(i)}}$ che $\mathsf{massimizza}$ la probabilità $\mathsf{P(x[m] \text{ } | \text{ } a^{(i)})}$.
    \begin{equation*}
        \mathsf{\hat{a}_{m} = arg \text{ } max \{ P(x[m] \text{ } | \text{ } a^{(i)}) \} } 
    \end{equation*}
\end{tcolorbox}

Come si applica la strategia di decisione *maximum likelihood* al campione $\mathsf{x[m]}$? Per prima cosa, studiamo la densità di probabilità della variabile $\mathsf{x[m]}$.
\begin{equation*}
	\mathsf{x[m]} = \mathsf{x_{I}[m] + j x_{Q}[m]} = \mathsf{a_{m} + n_{I}[m] + j n_{Q}[m]}
\end{equation*}

Visto che il segnale PAM è *reale* (il contenuto informativo è trasmesso mediante il ramo in fase del sistema), possiamo considerare solamente la parte reale della variabile decisionale $\mathsf{x[m]}$:
\begin{equation*}
	\mathsf{x_{I}[m]} = \mathsf{\Re\{x[m]}\} = \mathsf{\Re\{a_{m} + n_{I}[m] + j n_{Q}[m]\}} = \mathsf{a_{m} + n_{I}[m]}
\end{equation*}

Ecco fatto! Diamo uno sguardo a $\mathsf{x[m]}$:

1. $\mathsf{a_{m}}$ è una costante (non è una variabile aleatoria).

2. $\mathsf{n_{I}[m]}$ è una variabile aleatoria *gaussiana*, a media nulla e con varianza $\mathsf{\sigma_{I}^{2} = N_{0}}$.

Ecco che abbiamo trovato la densità di probabilità della variabile $\mathsf{x_{I}[m]}$:
\begin{equation*}
	\mathsf{f(x[m])} = \mathsf{\frac{1}{\sqrt{2 \pi} \sigma} exp \left( -\frac{(x[m] - a_{m})^2}{2 \sigma^{2}} \right)}
\end{equation*}

> Massimizzare $\mathsf{P(x[m] \text{ } | \text{ } a^{(i)})}$ equivale a minimizzare la distanza $\mathsf{|x[m] - a^{(i)}|}$ visto che la variabile decisionale è $\mathsf{\mathcal{N}(a^{(i)}, \sigma^{2} = N_{0})}$.

Adottando il criterio della massima verosimiglianza, possiamo suddividere lo spazio del segnale in regioni di decisione, dove la regione $\mathsf{Z^{(i)}}$ è l'insieme di punti che sono più vicini al simbolo $\mathsf{a^{(i)}}$ che a qualsiasi altro simbolo.

![Strategia di decisione PAM (simboli equiprobabili). Regioni di decisione.](figures/radio/PAM_decision_regions.png)


## PAM Error Probability

La strategia di decisione presentata in precedenza, seppur *ottima*, comunque può produrre degli errori a causa del rumore. Vediamo un esempio: 

* Supponiamo di avere una $\mathsf{2 \text{-} PAM}$ la cui costellazione è composta dai simboli $\mathsf{1 \text{ e } -1}$, equiprobabili, associati a $\mathsf{0 \text{ e } 1}$, rispettivamente. 

* Supponiamo che il trasmettitore abbia inviato il simbolo $\mathsf{a_{m} = -1}$ (quindi il bit inviato risulta essere $\mathsf{0}$). Al ricevitore, a causa del "forte" rumore, la variabile decisionale $\mathsf{x[m]}$ risulta essere $\mathsf{0.75}$.

* Il decisore implementa la strategia di decisione a *minima distanza*:
    - $\mathsf{|x[m] - a^{(0)}| = |0.75 + 1| = 1.75}$
    - $\mathsf{|x[m] - a^{(1)}| = |0.75 - 1| = 0.25}$

* Il decisore sceglie il simbolo $\mathsf{\hat{a}_{m} = 1}$: errore!

Definiamo la probabilità d'errore del simbolo $\mathsf{a^{(i)}}$, $\mathsf{P( e \text { | } a^{(i)})}$, come la probabilità che la variabile decisionale $\mathsf{x[m]}$ non cada nella regione $\mathsf{Z^{(i)}}$, sapendo di aver trasmesso $\mathsf{a^{(i)}}$, cioè:
\begin{align*}
    \mathsf{P( e \text { | } a^{(i)})} &= \mathsf{P(x[m] \notin Z^{(i)} \text{ } | \text{ } a_{m} = a^{(i)})} = \\
    &= \mathsf{1 - P(x[m] \in Z^{(i)} \text{ } | \text{ } a_{m} = a^{(i)})}
\end{align*}

Se il decisore implementa una strategia *maximum likelihood*, lo scopo del decisore è massimizzare la probabilità $\mathsf{P(x[m] \in Z^{(i)} \text{ } | \text{ } a_{m} = a^{(i)})}$: quest'ultima rappresenta la probabilità di successo del decisore.

La probabilità d'errore totale può essere calcolata come la media di tutte le probabilità d'errore dei simboli $\mathsf{a^{(i)}}$, cioè:
$$
    \mathsf{P_{e} = \frac{1}{M} \sum_{i=0}^{M-1} P( e \text { | } a^{(i)}) \approx \lim_{N^{(s)} \rightarrow \infty} \frac{N^{(s)}_{e}}{N^{(s)}}}
$$

In precedenza abbiamo studiato che la variabile decisionale $\mathsf{x[m]}$ è una variabile gaussiana $\mathsf{\mathcal{N}(a_{m}, \sigma^{2} = N_{0})}$. Calcolare la probabilità d'errore $\mathsf{P( e \text { | } a^{(i)})}$ equivale a calcolare l'area sottesa dalla funzione $\mathsf{f(x[m] \text { | } a^{(i)})}$ che non appartiene alla regione $\mathsf{Z^{(i)}}$.

![Probabilità di errore associata al simbolo $\mathsf{a^{(1)} = 1}$](figures/radio/q_function.png)

Supponiamo di voler calcolare la probabilità d'errore associata al simbolo $\mathsf{a^{(0)} = -1}$ in una $\mathsf{2 \text{-} PAM}$. Il decisore a minima distanza di una $\mathsf{2 \text{-} PAM}$ utilizza una sola soglia di decisione, $\mathsf{t = 0}$. Dunque, calcolare la probabilità d'errore associata al simbolo $\mathsf{a^{(0)} = -1}$ equivale a calcolare l'area sottesa dalla funzione $\mathsf{f(x[m] \text { | } a^{(0)})}$ nell'intervallo $\mathsf{[0, \infty]}$ (la soglia di decisione è $\mathsf{t=0}$).
\begin{align*}
    \mathsf{P( e \text { | } a^{(0)})} &= \mathsf{\int_{t}^{\infty} f(x \text { | } a^{(0)}) dx} = \\
    &= \mathsf{\int_{t}^{\infty} \frac{1}{\sqrt{2 \pi} \sigma} e^{-\frac{(x - a^{(0)})^2}{2 \sigma^2}} dx} = \\
\end{align*}
Applichiamo la sostituzione $\mathsf{y = \frac{x - a^{(0)}}{\sigma}}$. Segue:
\begin{align*}
    \mathsf{dy} &= \mathsf{\frac{dx}{\sigma} \text{ } \rightarrow dx = \sigma dy} \\ \\ 
    &= \mathsf{\frac{1}{\sqrt{2 \pi} \sigma} \int_{\frac{t - a^{(i)}}{\sigma}}^{\infty} e^{-\frac{y^2}{2}} dx \sigma} = \\
    &= \mathsf{\frac{1}{\sqrt{2 \pi}} \int_{\frac{t - a^{(i)}}{\sigma}}^{\infty} e^{-\frac{y^2}{2}} dx}
\end{align*}
Notiamo che $\mathsf{f(y) = \frac{1}{\sqrt{2 \pi}} e^{-\frac{y^2}{2}}}$ non è altro che la distribuzione di probabilità di una variabile aleatoria Normale, $\mathsf{\mathcal{N}(0,1)}$.

Definiamo la funzione coda $\mathsf{Q(x)}$ come:
$$
    \mathsf{Q(x) = \int^{\infty}_{x} \frac{1}{\sqrt{2 \pi}} e^{-x^{2}/2} dx}
$$

Alcune proprietà utili della funzione coda:

1. $\mathsf{Q(x)|_{x \rightarrow - \infty} = 1}$

2. $\mathsf{Q(x)|_{x = 0} = \frac{1}{2}}$ (è simmetrica rispetto l'origine).

3. $\mathsf{Q(-x) = 1 - Q(x)}$

Dunque, la probabilità d'errore $\mathsf{P( e \text { | } a^{(0)})}$ può essere calcolata come:
$$
    \mathsf{P( e \text { | } a^{(0)})} = \mathsf{Q(\frac{t - a^{(0)}}{\sigma})} = \mathsf{Q(\frac{0 - (-1)}{\sigma})} = \mathsf{Q(\frac{1}{\sigma})}
$$
Come si calcola la probabilità d'errore $\mathsf{P( e \text { | } a^{(1)})}$? Semplice: il decisore compie un errore quando il campione ricevuto è minore di $\mathsf{t=0}$ se il simbolo trasmesso in origine era $\mathsf{a_{m} = a^{(1)} = 1}$: da ciò segue che la probabilità d'errore $\mathsf{P( e \text { | } a^{(1)})}$ si può calcolare come l'aria della funzione $\mathsf{f(x[m] \text { | } a^{(1)})}$ nell'intervallo $\mathsf{[-\infty, 0]}$.
\begin{align*}
    \mathsf{P( e \text { | } a^{(1)})} &= \mathsf{\int_{- \infty}^{0} f(x \text { | } a^{(1)}) dx} = \\
    &= \mathsf{\int_{- \infty}^{0} \frac{1}{\sqrt{2 \pi} \sigma} e^{-\frac{(x - 1)^2}{2 \sigma^2}} dx} = 
\end{align*}
Applichiamo la sostituzione $\mathsf{y = \frac{x - 1}{\sigma}}$. Segue:
\begin{align*}
    \mathsf{dy} &= \mathsf{\frac{dx}{\sigma} \text{ } \rightarrow dx = \sigma dy} \\ \\ 
    &= \mathsf{\frac{1}{\sqrt{2 \pi} \sigma} \int_{\frac{- 1}{\sigma}}^{\infty} e^{-\frac{y^2}{2}} dx \sigma} = \\
    &= \mathsf{\frac{1}{\sqrt{2 \pi}} \int_{\frac{- 1}{\sigma}}^{\infty} e^{-\frac{y^2}{2}} dx} = Q(\frac{- 1}{\sigma})
\end{align*}
Ricordando che il decisore è simmetrico, allora $\mathsf{Q(\frac{1}{\sigma}) = Q(\frac{-1}{\sigma})}$:
$$
    \mathsf{P( e \text { | } a^{(1)})} = Q(\frac{- 1}{\sigma}) = Q(\frac{1}{\sigma})
$$

La probabilità d'errore totale, dunque, può essere calcolata mediante la funzione $\mathsf{Q(x)}$:
\begin{align*}
    \mathsf{P_{e}^{2-PAM}} &= \mathsf{\frac{1}{2}P( e \text { | } a^{(0)}) + \frac{1}{2}P( e \text { | } a^{(1)})} = \\
    &= \mathsf{\frac{1}{2}Q(\frac{1}{\sigma}) + \frac{1}{2}Q(\frac{1}{\sigma})} = \mathsf{Q(\frac{1}{\sigma})}
\end{align*}

Calcoliamo la probabilità d'errore totale per una $\mathsf{4-PAM}$:

* I simboli trasmessi sono $\mathsf{a^{(i)} = 2i - 3}$, $\mathsf{i = 0, ..., 3}.$

* Se i simboli sono equiprobabili allora possiamo implementare un decisore a *minima distanza*. In particolare, le soglie $\mathsf{t^{(i)}}$ del decisore coincidono con il punto intermedio di ciascuno segmento $\mathsf{\bar{a^{(i)}a^{(i+1)}}}$, $\mathsf{0 <  i < 3}$. In altre parole, le soglie di decisione saranno $\mathsf{t^{(0)} = -2}$, $\mathsf{t^{(1)} = 0}$ e $\mathsf{t^{(2)} = 2}$. 

* $\mathsf{P( e \text { | } a^{(0)}) = Q(\frac{|t^{(0)} - a^{(0)}|}{\sigma}) = Q(\frac{|-2 - (-3)|}{\sigma}) = Q(\frac{1}{\sigma})}$

* $\mathsf{P( e \text { | } a^{(3)}) = Q(\frac{|t^{(2)} - a^{(3)}|}{\sigma}) = Q(\frac{|2 - 3|}{\sigma}) = Q(\frac{1}{\sigma})}$

* $\mathsf{P( e \text { | } a^{(1)}) = Q(\frac{|t^{(0)} - a^{(1)}|}{\sigma}) + Q(\frac{|t^{(1)} - a^{(1)}|}{\sigma}) = 2Q(\frac{1}{\sigma})}$

* $\mathsf{P( e \text { | } a^{(2)}) = Q(\frac{|t^{(1)} - a^{(2)}|}{\sigma}) + Q(\frac{|t^{(2)} - a^{(2)}|}{\sigma}) = 2Q(\frac{1}{\sigma})}$

La probabilità totale d'errore per una $\mathsf{4-PAM}$ è la seguente:
$$
    \mathsf{P_{e}^{4-PAM} = \frac{1}{4} \sum_{i=0}^{3} P( e \text { | } a^{(i)}) = \frac{3}{2}Q(\frac{1}{\sigma})}
$$


### PAM error probability as a function of $\mathsf{SNR}$

![](figures/radio/total_p(e)_SNR.png){width="85%"}


### PAM bit error probability

Abbiamo definito la probabilità d'errore totale come il limite del rapporto tra il numero di simboli errati ricevuti e il numero di simboli inviati. Se il nostro sistema è ben progettato (Gray mapping) possiamo assumere che (in media) un errore sul simbolo si traduce in un solo bit errato.
$$
    \mathsf{P_{e}^{(b)} = \lim_{N^{(b)} \rightarrow \infty} \frac{N^{(b)}_{e}}{N^{(b)}} \approx \lim_{N^{(s)} \rightarrow \infty} \frac{N^{(s)}_{e}}{log_{2}(M)N^{(s)}} = \frac{1}{log_{2}(M)} \lim_{N^{(s)} \rightarrow \infty} \frac{N^{(s)}_{e}}{N^{(s)}} = \frac{1}{log_{2}(M)} P_{e}}
$$

![Andamento della probabilità d'errore sul bit in funzione del rapporto segnale/rumore.](figures/radio/2PAM_4PAM_8PAM_p(e)_SNR.png)

\newpage{}



# (Appendice A) Processi Stocastici

Si definisce *processo stocastico* (o *processo aleatorio*) una famiglia di variabili aleatorie $\mathsf{\{X(t), \, t \in T \subseteq  \mathbb{R}_{+} \}}$ dipendenti da una variabile, che nel nostro caso è il *tempo*, definite su uno spazio campione $\mathsf{\Omega}$. Fissando un istante di tempo $\mathsf{t_0}$, è possibile individuare valori generalmente differenti, ognuno relativo a una determinata realizzazione e quindi ad un elemento dello spazio campione: $\mathsf{X(t_0)}$ è allora una variabile aleatoria e rappresenta la "fotografia" del processo stocastico in un determinato istante. Quindi, rispetto a una semplice variabile aleatoria, $\mathsf{X(t_0)}$ fornisce anche un'informazione relativa all'evoluzione temporale.

\begin{figure}[ht]
    \centering
    \includegraphics[width=0.5\textwidth]{figures/radio/stoc_proc.png}
    \caption{Realizzazioni di un processo aleatorio.}
    \label{fig:stoc_proc}
\end{figure}

Ogni realizzazione $\mathsf{X(t_{i})}$ di un processo aleatorio può essere descritta attraverso le stesse funzioni che descrivono una variabile aleatoria: *distribuzione di probabilità* e *densità di probabilità*.

$\mathsf{Distribuzione \text{ } di \text{ } probabilità}$: 
\begin{align*}
    \mathsf{F_{X}(x,t) = P\{X(t) \leq x\}}    
\end{align*}

$\mathsf{Densità \text{ } di \text{ } probabilità}$: 
\begin{align*}
    \mathsf{f_{X}(x,t) = \frac{d}{dx}F_{X}(x,t)}    
\end{align*}

$\mathsf{Valor \text{ } medio}$: 
\begin{align*}
    \mathsf{\mu_{X}(t_0) = E\{X(t_0)\} = \int_{- \infty}^{+ \infty} x f_{X}(x,t_0) dx}
\end{align*}

$\mathsf{Autocorrelazione}$:
\begin{align*}
    R_{XX}(t_1,t_2) &= E\{X(t_1)X^{*}(t_2)\} = \int_{- \infty}^{+ \infty} \int_{- \infty}^{+ \infty} x_1 x^{*}_2 f_{X(t_1),X(t_2)}(x_1,x_2) dx_1 dx_2
\end{align*}

**Stazionarietà** - Un *processo stazionario* è un processo stocastico la cui distribuzione di probabilità congiunta non cambia se viene traslata nel tempo:
\begin{equation*}
    \mathsf{F_{X(t_1), ..., X(t_n)}(x_1, ..., x_n) = F_{X(t_1 + \tau), ..., X(t_n + \tau)}(x_1, ..., x_n) \, \forall \, \tau \in \mathbb{R}}
\end{equation*}

Esistono diversi *ordini* di stazionarietà. Esempio: **Stazionarietà del 1° ordine** ($\mathsf{n=1}$). Un processo $\mathsf{X(t)}$ è detto stazionario del primo ordine se le proprietà statistiche del processo sono uguali alle proprietà statistiche del processo $\mathsf{X(t + c)}$, per ogni $\mathsf{c}$. Da tale proprietà possiamo dire che un processo stazionario del 1° ordine ha valore medio costante e indipendente da $\mathsf{t}$:
\begin{align*}
    f_{X}(x, t) = f_{X}(x, t + c) \, \forall \, c \in \mathbb{R} \: \longrightarrow \: E\{X(t)\} = E\{X(t + c)\} = \mu_X
\end{align*}

Possiamo utilizzare una definizione più ampia di stazionarietà. Un processo $\mathsf{X(t)}$ è detto **stazionario in senso lato** (wide sense stationarity, **WSS**) se vengono soddisfatte solo queste due condizioni:

- $\mathsf{E\{X(t)\} = \mu \: \: \forall \, t \in \mathbb{R}}$
- $\mathsf{R_{XX}(t_1, t_2) = R_{XX}(t_2 - t_1)}$

Per un processo WSS, la media è costante e la funzione di autocorrelazione dipende solo dalla differenza tra gli indici di tempo.

**Densità spettrale di potenza** - La densità spettrale di potenza $\mathsf{S_{X}(f)}$ di un processo stocastico WSS $\mathsf{X(t)}$ descrive la distribuzione di potenza nelle componenti di frequenza che compongono il segnale. $\mathsf{S_{X}(f)}$ è misurata in watt per hertz (W/Hz). Possiamo calcolare $\mathsf{S_{X}(f)}$ sfruttando il teorema di *Wiener-Kintchine*.

\begin{tcolorbox}[colback=blue!5!white,colframe=blue!75!white,title=\textcolor{black}{\textbf{Teorema di Wiener-Kintchine}}]
    \textbf{Enunciato}
    \begin{equation*}
        \mathsf{S_{X}(f) = \mathcal{F}\{R_{XX}(\tau)\} = \int_{- \infty}^{+ \infty} R_{XX}(\tau) e^{-j2 \pi f \tau} \, d\tau} 
    \end{equation*}
\end{tcolorbox}

La potenza $\mathsf{P_{X}}$ del segnale $\mathsf{X(t)}$ può essere calcolata integrando la densità spettrale di potenza $\mathsf{S_{X}(f)}$:
\begin{equation*}
    \mathsf{ P_{X} = \int_{-\infty}^{+ \infty} S_{X}(f) \, df }
\end{equation*}
