---
title: Public Key Cryptography
author: Federico Casu
date: December 21, 2023
header-includes: | 
    \usepackage{tikz}
    \usetikzlibrary{shapes,arrows,positioning}
---


# Public Key Cryptography: a brief introduction

\tikzset{
  block/.style = {draw, rectangle, text width=2.5cm, align=center, minimum height=1.5cm},
  arrow/.style = {thick,->,>=stealth},
  line/.style = {thick,-},
}

\begin{figure}
    \centering
    \begin{tikzpicture}[node distance=3cm]

        % Alice's block
        \node [block] (Alice) {Alice knows $\mathsf{K_{pub}^{(B)}}$};

        % Bob's block
        \node [block, right=of Alice] (Bob) {Bob holds $\mathsf{\langle K_{priv}^{(B)}, K_{pub}^{(B)}\rangle}$};

        % Encryption and Decryption blocks
        \node [block, below=of Alice, yshift=1cm] (encrypt) {$\mathsf{y = E(K_{pub}^{(B)}, x)}$};
        \node [block, below=of Bob, yshift=1cm] (decrypt) {$\mathsf{\hat{x} = E^{-1}(K_{priv}^{(B)}, y)}$};

        % Messages
        \node [below=0.25cm of encrypt] {$\mathsf{y}$: Encrypted Message};
        \node [below=0.25cm of decrypt] {$\mathsf{\hat{x}}$: Decrypted Message};

        % Arrows
        \draw [arrow] (Alice) -- node[above, xshift=0.75cm, yshift=-0.25cm] {$\mathsf{x \text{, } K_{pub}^{(B)}}$} (encrypt);
        \draw [arrow] (encrypt) -- node[left, xshift=1.45cm, yshift=0.25cm] {Insecure Channel} (decrypt);
        \draw [arrow] (Bob) -- node[above, xshift=0.75cm, yshift=-0.25cm] {$\mathsf{K_{priv}^{(B)}}$} (decrypt);

    \end{tikzpicture}
    \caption{Public Key Cryptography - Simple communication scenario.}
    \label{fig:pub_key_scenario}
\end{figure}

As we are becoming familiar with it, let's study how *public key cryptography* is used to protect communications. Figure \ref{fig:pub_key_scenario} illustrates the fundamentals of a public key-based communication system:

- Alice, who wants to send a confidential message to Bob, knows Bob's public key $\mathsf{K_{pub}^{(B)}}$. To encrypt the message $\mathsf{x}$, Alice executes the encryption algorithm $\mathsf{E(\cdot)}$, taking as input the plaintext $\mathsf{x}$ and Bob's public key $\mathsf{K_{pub}^{(B)}}$.

- Bob, who wants to read the incoming message (and correctly decrypt $\mathsf{y}$), executes the decryption algorithm $\mathsf{E^{-1}(\cdot)}$, taking as input the ciphertext $\mathsf{y}$ and Bob's private key $\mathsf{K_{priv}^{(B)}}$.
