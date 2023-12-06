---
title: Wireless Propagation Channel
author: Federico Casu
date: Novembre 30, 2023
header-includes: | 
	\usepackage{subcaption}
	\usepackage{tikz}
	\usepackage{tcolorbox}
---



# Signal Propagation in the air

Ci sono, almeno, tre modelli utilizzati per rappresentare il canale di comunicazione.

> **Ground wave propagation model**. L'onda si propaga seguendo la curvatura della Terra raggiungendo ricevitori oltre l'orizzonte (in alcuni casi i ricevitori possono trovarsi a centinaia di chilometri dai trasmettitori).

> **Sky wave propagation model**. In alcuni intervalli di frequenza (circa $\mathsf{10 \text{ } MHz}$), la ionosfera funge da specchio e riflette i segnali verso la Terra. Rimbalzando tra la Terra e la ionosfera, il segnale può propagarsi fino a diverse migliaia di chilometri. Valido principalmente per le alte frequenze (HF).

> **Space wave propagation model**. Per frequenze superiori a $\mathsf{30 \text{ } MHz}$, le onde elettromagnetiche si propagano quando è presente un percorso di comunicazione diretto tra il trasmettitore e il ricevitore. Il segnale ricevuto è composto dalla componente diretta più delle "copie" generate dal segnale trasmesso che viene riflesso dal suolo terrestre e/o da altri oggetti che si trovano lungo il percorso di trasmissione (esempio: palazzi). Maggiore è la frequenza, maggiore è l'attenuazione della propagazione.

![Space wave propagation model.](figures/channel/space_wave_propagation.png){width="100%"}

Dato che i servizi mobili di comunicazione utilizzano principalmente range di frequenze da $\mathsf{30 \text{ } MHz}$ a $\mathsf{30 \text{ } GHz}$, studieremo più nel dettaglio il terzo modello di propagazione (*space wave*). I principali fenomeni fisici responsabili dell'attenuazione del segnale trasmesso sono: *riflessione*, *diffrazione*, *scattering*.

> $\mathsf{Riflessione}$. La riflessione avviene quando un'onda elettromagnetica (nel nostro caso il segnale trasmesso dal trasmettitore) colpisce un ostacolo che ha dimensioni molto grandi rispetto alla lunghezza d'onda $\mathsf{\lambda}$ del segnale. A seconda della superficie riflettente (superficie dell'ostacolo), il segnale (oltre, ovviamente, ad essere riflesso) subisce un'ulteriore attenuazione.

> $\mathsf{Diffrazione}$. La diffrazione avviene quando, sul percorso tra il trasmettitore e il ricevitore, l'onda elettromagnetica emessa incontra il bordo appuntito di un ostacolo, e le dimensioni dell'oggetto sono significativamente più grandi della lunghezza d'onda. Nella diffrazione, l'onda elettromagnetica si "spezza" sul bordo appuntito dell'ostacolo e continua a propagarsi, attenuata. Il fenomeno della diffrazione consente la trasmissione del segnale dal trasmettitore al ricevitore anche se c'è un ostacolo impenetrabile tra di loro e non c'è visibilità ottica diretta.

> $\mathsf{Scattering}$. Lo scattering avviene quando il mezzo attraverso il quale l'onda si propaga è composto da oggetti le cui dimensioni sono piccole rispetto alla lunghezza d'onda, e dove il numero di ostacoli per unità di volume è elevato. Quando un'onda elettromagnetica incontra tali oggetti, l'energia riflessa viene dispersa in tutte le direzioni. Gli ostacoli di questo tipo possono essere: segnali stradali, lampioni, *fogliame*, ecc.



# Fading Model

In generale, con il termine *fading* si intende la diminuzione della potenza del segnale (dunque la conseguente attenuazione del segnale) lungo il suo percorso dal trasmettitore al ricevitore, a causa di diversi fattori. Prendere in considerazione ciascuno di questi fattori sarebbe troppo oneroso. In pratica si modellano i fattori di attenuazione mediante due "classi" di fading: **large scale fading** e **small scale fading**.

Possiamo modellare il large scale fading combinando due fenomeni di attenuazione: **path loss** e **shadowing**.

![](figures/channel/large_vs_scale_fading.jpg){width="85%"}


## Path Loss (LS Fading)

In generale, il *path loss* esprimere la diminuzione della potenza media del segnale trasmesso in funzione della distanza $\mathsf{Tx \text{-} Rx}$. Se consideriamo solamente il path loss, la potenza media del segnale ricevuto può essere espressa come:
$$
	\mathsf{P_{Rx} \propto P_{Tx} \Gamma(f_{0}, d_{0}) (\frac{d_{0}}{d})^{n} \, \, \, \, d > d_{0}} 
$$
dove $\mathsf{\Gamma (f_{0}, d_{0}) \approx (\frac{\lambda}{4 \pi d_{0}})^2}$. Si noti che l'esponente $\mathsf{n}$ (*path loss exponent*) dipende dall'ambiente in cui avviene la trasmissione del segnale: "peggiore" è l'ambiente (in termini di presenza di ostacoli), maggiore è il path loss exponent $\mathsf{n}$ (Figura \ref{fig:path_loss_exponent}).

![Path loss exponent.\label{fig:path_loss_exponent}](figures/channel/path_loss_exponent.png){width="50%"} 

Il coefficiente di attenuazione associato al path loss può essere espresso come rapporto tra la potenza del segnale trasmesso e la potenza del segnale ricevuto, *cioè*
$$
	\mathsf{A_{PL} = \frac{P_{Tx}}{P_{Rx}} = \Gamma(f_{0}, d_{0}) (\frac{d_{0}}{d})^{n} }
$$

A frequenze più elevate ($\mathsf{f_c > 6 \text{ } GHz}$), l'attenuazione del canale dipende da altri fenomeni fisici come l'assorbimento dell'ossigeno e del vapore acqueo. I fenomeni fisici sopra menzionati, visto che le lunghezze d'onda in gioco sono molto (molto) piccole, agiscono come una sorta di filtro passa-basso.

Supponiamo di voler rappresentare l'attenuazione del segnale causato dal path loss in funzione della distanza $\mathsf{Tx \text{-} Rx}$. In particolare vogliamo rappresentare il path loss in decibel:
$$
	\mathsf{A_{PL}^{(dB)}} = \mathsf{10 log_{10} (\frac{P_{Tx}}{P_{Rx}})}
$$
Grazie alle proprietà dei logaritmi possiamo sviluppare $\mathsf{10 log_{10} (\frac{P_{Tx}}{P_{Rx}})}$:
$$
	\mathsf{A_{PL}^{(dB)}= 10 log_{10} (P_{Tx}) - 10 log_{10} (P_{Rx})}
$$
Ricordiamo che $\mathsf{P_{Rx} \propto P_{Tx} \Gamma(f_{0}, d_{0}) (\frac{d_{0}}{d})^{n}}$. Sostituendo l'espansione di $\mathsf{P_{Rx}}$ si ottiene:
\begin{align*}
	\mathsf{A_{PL}^{(dB)}} &= \mathsf{10 log_{10} (P_{Tx}) - 10 log_{10} (P_{Tx} \Gamma(f_{0}, d_{0}) (\frac{d_{0}}{d})^{n})} = \\
	&= \mathsf{10 log_{10} (P_{Tx}) - 10 log_{10} (P_{Tx}) - 10 log_{10} \Gamma(f_{0}, d_{0}) - 10 log_{10} (\frac{d_{0}}{d})^{n}} = \\
	&= \mathsf{- 10 log_{10} \Gamma(f_{0}, d_{0}) - 10 log_{10} (d_{0}) + n 10 log_{10} (d)} = \\
	&= \mathsf{\Gamma_{dB}(f_0, d_0) - n 10 log_{10} (d)}
\end{align*}
dove $\mathsf{\Gamma_{dB}(f_0, d_0) = 10 log_{10} \Gamma(f_{0}, d_{0}) + 10 log_{10} (d_{0})}$.

Possiamo notare che il path loss è un fattore di attenuazione *deterministico* e *linerare*.


## Shadowing (LS Fading)

Due punti che si trovano alla stessa distanza $\mathsf{d}$ dal trasmettitore sono soggetti, teoricamente, alla stessa attenuazione del segnale dovuta al path loss. Tuttavia la loro attenuazione media può comunque differire notevolmente. Lo *shadowing* tiene conto delle variazioni casuali dell'attenuazione media del canale.

> Shadowing $\mathsf{A_{S}}$ is a random variable *log-normally distributed* with parameters $\mathsf{\mu = 0}$ and $\mathsf{\sigma_{S}}$, expressed in $\mathsf{dB}$. The probability density function (in $\mathsf{dB}$) of $\mathsf{A_{S}}$ is $$\mathsf{p(A_{S}) = \frac{1}{\sqrt{2 \pi }\sigma_{S}} exp(-\frac{A_{S}^2}{2 \sigma_{S}^2})}$$



# Small Scale Fading

Lo *small scale fading* tiene conto delle variazioni casuali della potenza istantanea vista dal ricevitore su distanze dell'ordine della lunghezza d'onda.

Come possiamo rappresentare il segnale $\mathsf{y(t)}$ visto dal ricevitore tenendo conto dello small scale fading?

I fenomeni fisici come la riflessione, diffrazione e lo scattering (oltre a causare l'attenuazione del segnale) generano delle copie del segnale cui ciascuna arriva in ritardo e con uno sfasamento rispetto al segnale originale trasmesso dall'antenna del $\mathsf{Tx}$.

Vediamo un esempio (Figura \ref{fig:small_scale_fading_scenario}):

1. Il segnale \textcolor{blue}{blu} si propaga seguendo un percorso diretto: è ragionevole assumere che l'unico fattore che attenua il segnale è dovuto al *path loss*. In pratica, il segnale ha ritardo "nullo" e nessuna attenuazione dovuta a fenomeni quali riflessione, diffrazione o scattering.
2. Il segnale \textcolor{red}{rosso} non si propaga seguendo un percorso diretto ma arriva all'antenna del dispositivo destinatario dopo essere stato oggetto, ad esempio, di riflessione. In questo caso il segnale arriva con un ritardo non nullo ed ulteriormente attenuato oltre all'attenuazione causata dal path loss.
3. Le stesse considerazioni fatte per il segnale \textcolor{red}{rosso} sono valide per il segnale \textcolor{green}{verde}.

![Small scale fading effect on the transmitted signal.\label{fig:small_scale_fading_scenario}](figures/channel/small_scale_fading_scenario.png){width="75%"}

Di conseguenza, viste le osservazioni fatte sopra, possiamo rappresentare il segnale $\mathsf{y(t)}$ come la somma di segnali cui ciascuno di essi ha il suo ritardo e il suo sfasamento rispetto al segnale $\mathsf{s(t)}$:
$$
	\mathsf{y(t) = A_{PL} \sum_{k=0}^{N - 1} a_{k} e^{j \phi_{k}} s(t - \tau_{k})}
$$

Visto che un copia del segnale trasmesso avente ritardo $\mathsf{\tau}$ può essere espressa come la convoluzione nel tempo del segnale $\mathsf{s(t)}$ e $\mathsf{\delta(t - \tau)}$, la risposta impulsiva del canale può essere espressa come segue:
$$
	\mathsf{h(t) = A_{PL} \sum_{k=0}^{N - 1} a_{k} e^{j \phi_{k}} \delta(t - \tau_{k})}
$$

* I coefficienti di attenuazione $\mathsf{a_{k}}$ sono modellati come variabili aleatorie. Noi ci occuperemo di studiare questi coefficienti di attenuazione nel caso in cui sono caratterizzati dalla distribuzione di *Rayleigh*.

* Le fasi $\mathsf{\phi_{k}}$ sono modellate come variabili aleatorie *uniformemente distribuite* nell'intervallo $\mathsf{[0, 2 \pi]}$.


## Multipath Channel

Abbiamo visto che l'effetto dello small scale fading è la dispersione nel tempo del segnale originariamente trasmesso. Tutto ciò cosa significa? Le varie repliche del segnale possono generare $\mathsf{ISI}$.

Ricordiamo che, nel caso in cui la risposta impulsiva del canale sia $\mathsf{h(t) = \delta(t)}$, il segnale in uscita dal filtro di ricezione $\mathsf{g_{R}(t)}$ è il seguente:
\begin{align*}
	\mathsf{x(t)} &= A_{LS} \sum_{n} a_{n} g(t - n T_s) + n(t)
\end{align*}
Per semplicità consideriamo $\mathsf{A_{LS} = 1}$ e non consideriamo il rumore $\mathsf{n(t)}$. Allora, se il canale è *ideale* e la risposta impulsiva della cascata $\mathsf{g_{T}(t) * g_{R}(t) = \delta(t)}$ (criterio di Nyquist), il campione del segnale ricevuto in entrata al decisore è il seguente:
$$
	\mathsf{x(m)} = \mathsf{x(t)|_{t = m T_s}} = \mathsf{\sum_{n} a_{n} g(mt - n T_s)} = \mathsf{\sum_{n} a_{n} g((m - n)T_s)}
$$
Definiamo $\mathsf{k = m - n}$:
$$
	\mathsf{x(m)} = \mathsf{\sum_{k} a_{m-k} g(kT_s)} = \mathsf{a_{m}g(0) + \sum_{k \neq 0} a_{m-k} g(kT_s)}
$$
Ora possiamo compiere la sostituzione $\mathsf{g(t) = g_{T}(t) * g_{R}(t) = \delta(t)}$:
$$
	\mathsf{x(m)} = \mathsf{a_{m}\delta(0) + \sum_{k \neq 0} a_{m-k} \delta(kT_s)} = \mathsf{a_{m}}
$$
Che figata! Non abbiamo ISI. Purtroppo le cose si complicano un po' se prendiamo in considerazione lo small scale fading. 

> $\mathsf{h(t) = A_{LS}} \sum_{l=0}^{L-1} \alpha_{l} \delta (t - \tau_{l}) \neq \delta(t) \rightarrow \mathsf{\sum_{k \neq 0} a_{m-k} g(kT_s) \neq 0}$.

### Delay Spread

La dispersione del segnale nel tempo causata dal canale è misurata dal *delay spread* $\mathsf{\sigma_{\tau}}$. Di fatto, il delay spread ci dice quanto le varie repliche del segnale "generate" dal canale influiscono sull'ISI.

* Se il delay spread $\mathsf{\sigma_{\tau}}$ è molto minore del periodo di emissione del simbolo $\mathsf{T_{s}}$ allora le varie repliche del segnale arrivano all'interno di un tempo di simbolo e non costituiscono un serio problema in termini di ISI.

* Se il delay spread $\mathsf{\sigma_{\tau}}$ è comparabile o maggiore del periodo di emissione del simbolo $\mathsf{T_{s}}$ allora alcune repliche del segnale associate al simbolo $\mathsf{a_{m}}$ arrivano durante l'intervallo di tempo associato al simbolo $\mathsf{a_{m+1}}$ causando, appunto, ISI.

Come si calcola il delay spread? Partiamo dalla definizione di delay spread: è una misura che rappresenta la dispersione temporale delle varia repliche del segnale trasmesso. Dal punto di vista statistico, visto che il ritardo $\mathsf{\tau}$ è una variabile aleatoria, il delay spread può essere approssimato come la *deviazione standard* della variabile $\mathsf{\tau}$.
$$
	\mathsf{\sigma_{\tau}} = \mathsf{\sqrt{\sigma_{\tau}^{2}}} = \mathsf{\sqrt{E\{\tau^{2}\} - E\{\tau\}^{2}}}
$$
Per poter calcolare $\mathsf{\sigma_{\tau}}$ occorre conoscere la densità di probabilità della variabile $\mathsf{\tau}$: difficile! 

Allora come si fa? Siamo ingegneri: approssimiamo.
\begin{align*}
	\mathsf{\sigma_{\tau}} &= \mathsf{\sqrt{\bar{\tau^2} - \bar{\tau}^2}} \\ \\
	\mathsf{\bar{\tau}} &= \mathsf{\sum_{h=0}^{L-1}\frac{\alpha_{h}^{2}}{\sum_{k=0}^{L-1}\alpha_{k}^{2}}\tau_{h}} \\ \\
	\mathsf{\bar{\tau^{2}}} &= \mathsf{\sum_{h=0}^{L-1}\frac{\alpha_{h}^{2}}{\sum_{k=0}^{L-1}\alpha_{k}^{2}}\tau_{h}^{2}}
\end{align*}


### Coherence Bandwidth

La *coherence bandwidth* rappresenta l'intervallo di frequenze entro cui il canale può essere considerato "piatto" (costante), o in altre parole, l'intervallo di frequenze su cui è probabile che due frequenze di un segnale sperimentino un'attenuazione comparabile.

La coherence bandwidth può essere approssimata come $\mathsf{B_{c} \approx \frac{1}{5 \sigma_{\tau}}}$.

Cosa succede se mettiamo a paragone la banda del segnale $\mathsf{B_{s}}$ e la coherence bandwidth $\mathsf{B_{c}}$?

* Se $\mathsf{B_{c} >> B_{s}}$ ($\mathsf{\sigma_{\tau} << T_{s}}$) allora il canale può essere considerato constante in relazione alle frequenze occupate dal segnale trasmesso.

* Se $\mathsf{B_{c} \leq B_{s}}$ ($\mathsf{\sigma_{\tau} \geq T_{s}}$) allora il canale è detto *frequency selective* perchè alcune frequenze del segnale sperimentano attenuazioni diverse da altre (il canale non può più essere considerato constante in relazione alle frequenze occupate dal segnale trasmesso).


### Esempio: Two-rays ground-reflection channel model

Il modello di canale $\mathsf{two \text{-} rays \text{ } ground \text{-} reflection}$ è un modello di propagazione *multipath* che predice l'attenuazione del segnale causata dal *path loss* tra un'antenna trasmittente e un'antenna ricevente quando esiste un path diretto (*line of sight*, LOS). In generale, le due antenne hanno altezze diverse. Il segnale ricevuto ha due componenti, la componente diretta e la componente di riflessione formata principalmente da una singola onda riflessa dal suolo (Figura \ref{fig:two_rays_model}).

![Schema del modello two-rays ground-reflection.\label{fig:two_rays_model}](figures/channel/two_rays_model.png){width="75%"}

In questo scenario, il segnale ricevuto assume la seguente forma:
$$
	\mathsf{y(t)} = \mathsf{A_{LS} (\alpha_{0}s(t - \tau_{0}) + \alpha_{1}s(t - \tau_{1}))}
$$

**Esempio 1**:
\begin{align*}
	\mathsf{\alpha_{0}} &= \mathsf{1} \text{, } \mathsf{\alpha_{1}} = \mathsf{0.9} \\
	\mathsf{\tau_{0}} &= \mathsf{0} \text{, } \mathsf{\tau_{1}} = \mathsf{0.1T}
\end{align*}

Calcoliamo il delay spread nello scenario presentato sopra:
\begin{align*}
	\mathsf{p_{0}} &= \mathsf{\frac{1}{1 + (0.9)^2} \approx 0.55 } \\
	\mathsf{p_{1}} &= \mathsf{\frac{(0.9)^2}{1 + (0.9)^2} \approx 0.45 } \\
	\mathsf{\bar{\tau}} &= \mathsf{\frac{(0.9)^2}{1 + (0.9)^2}(0.1T) \approx 0.045T} \\
	\mathsf{\bar{\tau^2}} &= \mathsf{\frac{(0.9)^2}{1 + (0.9)^2}(0.1T)^2 \approx 0.0045T} \\
	\mathsf{\sigma_{\tau}} &= \mathsf{\sqrt{\bar{\tau^2} - \bar{\tau}^2}} = \mathsf{0.05T}
\end{align*}

Visto che il delay spread è molto (molto) minore del tempo di simbolo $\mathsf{T}$ allora il canale può essere considerato costante.

**Esempio 2**:
\begin{align*}
	\mathsf{\alpha_{0}} &= \mathsf{1} \text{, } \mathsf{\alpha_{1}} = \mathsf{0.9} \\
	\mathsf{\tau_{0}} &= \mathsf{0} \text{, } \mathsf{\tau_{1}} = \mathsf{1.5T}
\end{align*}

Calcoliamo il delay spread nello scenario presentato sopra:
\begin{align*}
	\mathsf{p_{0}} &= \mathsf{\frac{1}{1 + (0.9)^2} \approx 0.55 } \\
	\mathsf{p_{1}} &= \mathsf{\frac{(0.9)^2}{1 + (0.9)^2} \approx 0.45 } \\
	\mathsf{\bar{\tau}} &= \mathsf{\frac{(0.9)^2}{1 + (0.9)^2}(1.5T) \approx 0.675T} \\
	\mathsf{\bar{\tau^2}} &= \mathsf{\frac{(0.9)^2}{1 + (0.9)^2}(1.5T)^2 \approx 1.0125T} \\
	\mathsf{\sigma_{\tau}} &= \mathsf{\sqrt{\bar{\tau^2} - \bar{\tau}^2}} = \mathsf{0.622T}
\end{align*}

Ora il delay spread è comparabile al tempo di simbolo $\mathsf{T}$: il canale *non* può essere considerato costante (esempio di canale *frequency selective*).



# Time-Varying Channel 

Se il ricevitore (trasmettitore, entrambi) è in *movimento*, l'attenuazione e lo sfasamento sperimentati dalle varie repliche del segnale possono variare nel tempo. In particolare, la risposta del canale cambia in funzione del tempo:
$$
	\mathsf{h(t, \tau)} = \mathsf{A_{LS} \sum_{k=0}^{N_{c}-1} \alpha_{k}(t) e^{j \phi_{k}(t)} h(t - \tau_{k})}
$$
Possiamo notare che i guadagni $\mathsf{\alpha_{k}}$ e le fasi $\mathsf{\phi_{k}(t)}$ cambiano molto più velocemente rispetto ad altri fenomeni (vedi *path loss* $\mathsf{A_{LS}}$  o ritardo $\mathsf{\tau}$).


## Effetto Doppler

L'effetto Doppler è un fenomeno fisico che consiste nel cambiamento apparente, rispetto al valore originario, della frequenza (quindi della lunghezza d'onda) percepita da un osservatore raggiunto da un'onda emessa da una sorgente che si trova in movimento rispetto all'osservatore stesso. Facciamo un esempio per capire meglio.

* Prendiamo in considerazione un sistema composto da due osservatori ed un'ambulanza. L'ambulanza viaggia ad una velocità costante di $\mathsf{120 \text{ } Km/h}$. La distanza tra i due osservatori è di $\mathsf{1 \text{ } Km}$.

* Supponiamo che l'ambulanza accenda la sirena quando si trova esattamente di fronte al primo osservatore. Visto che la velocità del suono è di circa $\mathsf{1200 \text{ } Km/h}$, il secondo osservatore (che si trova ad un chilometro di distanza dal primo osservatore) sentirà la sirena con un ritardo di $\mathsf{3}$ secondi.

* L'ambulanza impiega $\mathsf{30}$ secondi a percorrere la distanza trai i due osservatori. Nell'istante in cui si trova davanti al secondo osservatore, l'ambulanza spegne la sirena. Il primo osservatore continuerà a ricevere le onde sonore per altri 3 secondi ("l'ultima" onda sonora emessa deve percorre un chilometro nella direzione del primo osservatore).

* Conclusione: l'autista dell'ambulanza sente la sirena per un totale di $\mathsf{30}$ secondi, il primo osservatore sente la sirena per $\mathsf{33}$ secondi ed il secondo osservatore sente la sirena per $\mathsf{27}$ secondi.


## Doppler shift

Vediamo cosa succede al segnale trasmesso quando quest'ultimo è indirizzato ad un ricevitore in movimento. In particolare, prendiamo in considerazione il segnale che arriva al ricevitore se si trovasse in un punto $\mathsf{x}$ all'istante $\mathsf{t}$ ed il segnale ricevuto nel medesimo istante $\mathsf{t}$ se il ricevitore si trovasse nel punto $\mathsf{y = x + d}$.

1. Supponiamo che il ricevitore veda all'istante $\mathsf{t}$, in $\mathsf{x}$, il segnale $\mathsf{s_{x}(t) = sin(2 \pi f_{c} t)}$.

2. Visto che i due punti si trovano a distanza $\mathsf{d}$, il segnale che il ricevitore in movimento avrebbe visto all'istante $\mathsf{t}$ se si fosse trovato nel punto $\mathsf{y}$ è in ritardo rispetto al segnale ricevuto in $\mathsf{x}$ nello stesso istante $\mathsf{t}$:
\begin{align*}
	\mathsf{s_{y}(t)} &= \mathsf{s_{x}(t - \Delta \tau) = sin(2 \pi f_c (t - \Delta \tau))} = \\
	&= \mathsf{sin(2 \pi f_c (t - \frac{d}{c})) = sin(2 \pi f_c t - 2 \pi f_c \frac{vt}{c}) = sin(2 \pi (f_c - \frac{vf_c}{c})t)}
\end{align*}

Possiamo ben notare che la frequenza dei due segnali è diversa! Nel caso di multipath channel, ciò significa che un ricevitore in movimento riceverà diverse copie del segnale originario ed ciascuna di queste avrà una frequenza diversa dall'originale.
$$
	\mathsf{Doppler \text{ } shift: \text{ } f_{d} = - \frac{vf_{c}}{c}}
$$

L'effetto Doppler causa l'*allargamento* dello spettro del segnale. Tale effetto è chiamato **Doppler Spread**.

Se il segnale trasmesso è un sinusoide, cioè $\mathsf{s(t) = sin(2 \pi f_{c} t)}$, il segnale ricevuto $\mathsf{y(t)}$ è un processo stocastico che assume la forma $\mathsf{y(t) = a(t)sin(2 \pi f_{c} t)}$. La densità spettrale di potenza del processo $\mathsf{y(t)}$ è data dalla convoluzione della DSP del segnale $\mathsf{s(t)}$, $\mathsf{S_{s}(f)}$, e dalla DSP del canale, $\mathsf{S_{D}(f)}$. La densita spettrale di potenza del canale tempo-variante può essere modellata mediante il $\mathsf{Jakes \text{ } Doppler \text{ } spectrum}$.

![](figures/channel/doppler_spectrum_sinusoidal_tone.png)

$$
	\mathsf{Jakes \text{ } Doppler \text{ } spectrum: S_{D}(f) = \frac{1}{\pi f_d} \left( \sqrt{1 - \frac{(f - f_d)^2}{f_{d}^{2}}} \right) ^{-1}}
$$

Può essere dimostrato che, per un segnale $\mathsf{PAM}$ (o $\mathsf{QAM}$), la densità spettrale di potenza del segnale ricevuto può essere calcolata come:
$$
	\mathsf{S_{y}(f) = S_{s}(f) * S_{D}(f)}
$$


## Channel's Coherence time

Il tempo di coerenza del canale $\mathsf{T_c}$ è definito come l'intervallo di tempo durante il quale il canale può essere approssimato come costante.

Il tempo di coerenza $\mathsf{T_c}$ è il duale nel dominio temporale del *Doppler spread* $\mathsf{f_d}$ ed è utilizzato per caratterizzare la natura variabile nel tempo della dispersione di frequenza del canale. $\mathsf{f_d}$ e il tempo di coerenza sono inversamente proporzionali l'uno all'altro. Vale a dire, $\mathsf{T_c \approx 1/f_d}$.

In particolare, sappiamo che la trasformata inversa di Fourier di $\mathsf{S_{D}(f)}$ può essere calcolata come $\mathsf{J_{0}(2 \pi f_{d} t)}$, dove $\mathsf{J_{0}(x)}$ è una funzione di Bessel di ordine zero. 

In pratica, possiamo pensare che il tempo di coerenza indica l'intervallo temporale per il quale il canale può essere considerato "incorrelato".
$$
	\mathsf{J_{0}(2 \pi f_{d} t) \approx 0 \text{ } \rightarrow f_{d} t = \frac{1}{2} \rightarrow f_{d} T_c = \frac{1}{2} \rightarrow T_c = \frac{1}{2 f_d}}
$$