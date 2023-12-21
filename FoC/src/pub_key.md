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

        % Alice
        \node [block] (Alice) {Alice knows $\mathsf{K_{pub}^{B}}$};

        % Bob
        \node [block, right=of Alice] (Bob) {Bob holds $\mathsf{\langle K_{priv}^{B}, K_{pub}^{B}\rangle}$};

        % Encryption and Decryption blocks
        \node [block, below=of Alice, yshift=1cm] (encrypt) {$\mathsf{y = E(K_{pub}^{B}, x)}$};
        \node [block, below=of Bob, yshift=1cm] (decrypt) {$\mathsf{\hat{x} = E^{-1}(K_{priv}^{B}, y)}$};

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

As we are becoming familiar with it, let's study how $\mathsf{public \text{ } key \text{ } cryptography}$ is used to protect communications. Figure \ref{fig:pub_key_scenario} illustrates the fundamentals of a public key-based communication system:

- Alice, who wants to send a confidential message to Bob, knows Bob's public key $\mathsf{K_{pub}^{B}}$. To encrypt the message $\mathsf{x}$, Alice executes the encryption algorithm $\mathsf{E(\cdot)}$, taking as input the plaintext $\mathsf{x}$ and Bob's public key $\mathsf{K_{pub}^{B}}$.

- Bob, who wants to read the incoming message (and correctly decrypt $\mathsf{y}$), executes the decryption algorithm $\mathsf{E^{-1}(\cdot)}$, taking as input the ciphertext $\mathsf{y}$ and Bob's private key $\mathsf{K_{priv}^{B}}$.

Let's give a formal definition of a public key encryption scheme.

> A public key encryption scheme is composed of a triple of algorithms, $\mathsf{\langle E, D, G\rangle}$, such that they fulfill the following properties:

> $\mathsf{G}$ is a randomized algorithm that outputs a pair of keys, namely $\mathsf{\langle K_{pub}, K_{priv} \rangle}$.
$$
    \mathsf{G \text{ } : \text{ } \{0,1\}^{k} \rightarrow K = \{0,1\}^{n} \times \{0,1\}^{n}}
$$
> $\mathsf{E}$ is a randomized algorithm that, given inputs of a plaintext $\mathsf{x \in X}$ and a public key $\mathsf{K_{pub}}$, outputs a ciphertext $\mathsf{y \in Y}$.
$$
    \mathsf{E \text{ } : \text{ } K \times X \rightarrow Y}
$$
> $\mathsf{D}$ is a **deterministic** algorithm that, given inputs of a ciphertext $\mathsf{y \in Y}$ and a private key $\mathsf{K_{priv}}$, outputs a plaintext $\mathsf{x \in X}$.
$$
    \mathsf{D \text{ } : \text{ } K \times Y \rightarrow X}
$$
> The encryption scheme fulfills the **consistency property**, *i.e.*
$$
    \mathsf{\forall \text{ } \langle K_{pub}, K_{priv} \rangle \text{, } \forall \text{ } x \in X \rightarrow E^{-1}(K_{priv}, E(K_{pub}, x)) = x}
$$

Being an encryption scheme, the previously defined public key scheme must provide certain security properties. We would like to give you an informal definition of the security properties of a public key encryption scheme:

1. Given any ciphertext $\mathsf{y}$ and the public key used to encrypt it, $\mathsf{K_{\text{pub}}}$, it must be infeasible to obtain the plaintext $\mathsf{x}$ such that $\mathsf{y = E(K_{\text{pub}}, x)}$.

2. Given any public key, it must be infeasible to obtain the corresponding private key.
 
Such properties rely on some algebraic constructs. In particular, public key cryptography exploits a certain type of mathematical function called **one-way functions**.

The *one-wayness* property states that a function $\mathsf{f}$ is said to be one-way if:

- $\mathsf{f \text{ } is \text{ } easy \text{ } to \text{ } compute}$

- $\mathsf{f^{-1} \text{ } is \text{ } hard \text{ } to \text{ } compute}$

In what way $\mathsf{one\text{-}way}$ functions are related to public key cryptography? Let's make an example:

- The $\mathsf{RSA}$ cryptosystem exploits the integer factorization as the underlying $\mathsf{one\text{-}way}$ function: multiplying two primes is easy but factoring the resulting product is computationally infeasible.