# Pills of Applied Cryptography

## Symmetric Cryptography

**Could you explain what it means for a cipher to be perfect?**

Beforehand, we should describe the threat model we are dealing with:

1. Consider an adversary with ciphertext-only capability, i.e., an adversary who can mount only ciphertext-only attacks.

2. In such a scenario, an adversary can eavesdrop on the channel and record any ciphertext she wants. Additionally, she may have some information about the encryption scheme being used, as well as some information about the plaintext. It's important to note that the latter does not mean that the adversary knows the plaintext being encrypted, but she may know (for example) the probability that the sender will encrypt a particular plaintext. Keep in mind that the only piece of information the adversary should not know is the symmetric key being used to encrypt/decrypt data.

So, a cipher is said to be "perfect" if has a well defined property called **perfect secrecy**.

Definition [1](#symmetric-cryptography).1 [**Perfect Secrecy**]. Consider an symmetric encryption scheme defined over a plaintext space $\mathcal{M}$ and a ciphertext space $\mathcal{C}$. Let $\mathsf{M}$ and $\mathsf{C}$ be random variables:

- $\mathsf{M}$ is random variable defined over $\mathcal{M}$ with no hypothesis regarding the distribution.

- $\mathsf{C}$ is random variable defined over $\mathcal{C}$ with no hypothesis regarding the distribution.

The perfect secrecy property states:

> For any plaintext $\mathsf{m}$, for any ciphertext $\mathsf{c}$ with $\mathsf{Pr(C = c) > 0}$, it holds $\mathsf{Pr(M = m \; | \; C = c) = Pr(M = m)}$

What does the perfect secrecy property aim to model? If a cipher is perfect, the encryption process should not reveal any additional information about the plaintext merely by inspecting the eavesdropped ciphertext. In terms of probability, the additional information we are discussing is represented by the a-posteriori probability: the likelihood of any plaintext should not increase or decrease after eavesdropping on any ciphertext.

There is a very famous theorem that allow us to check whether a cipher is perfect or not.

Theorem [1](#symmetric-cryptography).1 [**Shannon's Theorem**]. In a perfect cipher, $|\mathcal{K}| \geq |\mathcal{M}|$, i.e., the number of keys cannot be smaller than the 
number of messages.

*Proof*: We are going to prove Shannon's theorem by contradiction.

1. Assume $|\mathcal{K}| < |\mathcal{M}|$.

2. A cipher must be invertible. Thus, $|\mathcal{C}| \geq |\mathcal{M}|$. If $|\mathcal{C}| < |\mathcal{M}|$, then it would exist a pair $\mathsf{\langle m_1, m_2 \rangle}$, $\mathsf{m_1, \neq m_2 }$, such that $\mathsf{E(k, m_1) = E(k, m_2) = c}$: if it were the case, $\mathsf{c}$ could not be univoquely decrypted.

3. From (1) and (2) follows $|\mathcal{K}| < |\mathcal{C}|$. Now, we chose a plaintext $\mathsf{\hat{m}}$ such that $\mathsf{Pr(M = \hat{m}) \neq 0}$. Since $|\mathcal{K}| < |\mathcal{C}|$, it exists at least a ciphertext $\mathsf{\hat{c}}$ that is *not* image of  $\mathsf{\hat{m}}$ by means of any key $\mathsf{k_i \; \in \; \mathcal{K}}$

4. We found a contradiction: $\mathsf{Pr(M = \hat{m} \; | \; C = \hat{c}) = 0 \neq Pr(M = \hat{m})}$.

---

**Describe OTP.**

One-Time-Pad (OTP) is a stream cipher that works as follows:

1. Let $\mathcal{X} = \mathsf{\{0,1\}^{n}}$ the plaintext space.

2. Let $\mathcal{K} = \mathsf{\{0,1\}^{n}}$ the key space. Any key in $\mathcal{K}$ must be *truly random* generated, that is each bit of the key-stream must be truly random chosen. 

3. Any key must be used just once.

4. **Encryption**: $\mathsf{y_i = x_i \oplus k_i = x_i + k_i \text{ } mod \text{ } 2}$, $\mathsf{i = 1, ..., n}$

    4.1 $\mathsf{x_i, k_i, y_i \in \{0,1\}}$ (i.e. they are binary digits).

5. **Decryption**: $\mathsf{x_i = y_i \oplus k_i = y_i + k_i \text{ } mod \text{ } 2}$, $\mathsf{i = 1, ..., n}$

Obviously, to be a cipher, OTP must fulfills the *consistency property*:
> For any $\mathsf{x \in}$ $\mathcal{X}$, for any $\mathsf{k \in}$ $\mathcal{K}$, it holds $\mathsf{D(k, E(k, x)) = x}$.

*Proof*: 

- $\mathsf{D(k, y) = y_i + k_i \; mod \; 2}$ for each $\mathsf{i = 1, ..., n}$.

- $\mathsf{D(k, y) = (x_i + k_i) + k_i \; mod \; 2 = x_i + (k_i + k_i) \; mod \; 2 = x_i + 2k_i \; mod \; 2 = x_i}$ for each $\mathsf{i = 1, ..., n}$.

Is OTP perfect? YES:

1. To prove that OTP is perfect, we should prove that $\mathsf{Pr(X = x \; | \; Y = y) = Pr(X = x)}$ for any $\mathsf{x \in}$ $\mathcal{X}$ and for any $\mathsf{y \in}$ $\mathcal{Y}$.

2. Consider $\mathsf{Pr(X = x \; | \; Y = y)}$:

    2.1 $\mathsf{Pr(X = x \; | \; Y = y) = \frac{Pr(Y = y \; | \; X = x) \cdot Pr(X = x)}{Pr(Y = y)}}$ (we exploit Bayes' Theorem).

    2.2 $\mathsf{Pr(Y = y \; | \; X = x) = Pr(y = k \oplus x) = Pr(k = y \oplus x) = 2^{-k}}$ because any key is equally likely.

3. Consider $\mathsf{Pr(Y = y)}$:

    3.1 $\mathsf{Pr(Y = y) = \sum_{i} Pr(Y = y \; | \; X = x_i)Pr(X = x_i)}$

    3.2 Substitute $\mathsf{Pr(Y = y \; | \; X = x_i)}$ with $\mathsf{Pr(y = k \oplus x_i) = Pr(k = y \oplus x_i) = 2^{-k}}$

    3.3 $\mathsf{Pr(Y = y) = \sum_{i} 2^{-k} \cdot Pr(X = x_i) = 2^{-k}}$

4. Put all together:

    4.1 $\mathsf{Pr(X = x \; | \; Y = y) = \frac{Pr(Y = y \; | \; X = x) \cdot Pr(X = x)}{Pr(Y = y)} = \frac{2^{-k} \cdot Pr(X = x)}{2^{-k}} = Pr(X = x)}$

Why should one not use key-streams more than once? Because an attacker could mount the following attack:

- Consider an attacker with ciphertext-only capability. Then, (s)he can eavesdrop on the channel and record two ciphertexts.

- If the sender had used the same key for both messages, the attacker can gain something useful by XORing the eavesdropped ciphertexts:

$$\mathsf{c_1 \oplus c_2 = (m_1 \oplus k) \oplus (m_2 \oplus k) = m_1 \oplus m_2 \oplus (k \oplus k) =}$$
$$\mathsf{= m_1 + m_2 + 2\cdot k \; mod \; 2 = m_1 + m_2 \; mod \; 2 = m_1 \oplus m_2}$$

Even if it's not a matter of confidentiality, OTP is malleable, i.e., an attacker can apply a perturbation that lead to a predictable transformation of the plaintext and that perturbation goes undetected.

Let's se what an attacker can do:

1. Consider an attacker that was able to eavesdrop on a ciphertext $\mathsf{c}$. (S)he does not know the plaintext but (s)he would like to substitute the original plaintext with one of her/his choice. Well, the attacker wont end up substituting a message at his/her willing but for sure (s)he can do bad things. 

2. Consider a perturbation $\mathsf{r}$. If the attacker applies the perturbation to the eavesdropped ciphertext, the receiver will end up decrypting $\mathsf{y' \oplus k = (y \oplus r) \oplus k = (x \oplus k) \oplus r \oplus k = x \oplus r \oplus (k \oplus k) = x \oplus r}$.

---

**How to build practical stream ciphers?**

If we would like to send an encrypted file using OTP, we would have to generate a truly random key-stream as long as the file: it would be very impractical. 

A more efficient way to build stream ciphers is to make use of pseudo-random number generators. Let's present the encryption scheme:

- Let $\mathsf{Gen(k)}$ be an efficient algorithm defined as $\mathsf{Gen : \{0,1\}^k \rightarrow \{0,1\}^n}$, $\mathsf{k < n}$.

- **Encryption**: $\mathsf{y = E(k, x) = Gen(k) \oplus x}$

- **Decryption**: $\mathsf{x = E(k, y) = Gen(k) \oplus y}$

We should stress that $\mathsf{Gen(k)}$ is a deterministic algorithm: the only source of randomness is given by the input (called seed). So, from the perspective of an attacker, $\mathsf{Gen(k)}$ behaves 'randomly' because (s)he does not know the seed, i.e., the attacker does not know the encryption key! From the perspective of the legitimate communicating parties, the key-stream generator is a deterministic algorithm.

---

### Pseudo Random Generators

Speaking about practical implementations of stream ciphers, we introduced a new concept: **pseudo-random number generators**. Let's start defining what is a pseudo-random bit generator.

Definition [1](#symmetric-cryptography).2 [**Random Bit Generator**]. A random bit generator is an algorithm that outputs a sequence of bits which are *statistically independent* and *unbiased*.

- Statistically independent means that the probability of emitting a bit value (either 1 or 0) does not depend on the previous bits.

- Unbiased means that the probability of emitting a certain bit value is equal (or very very close to) $\mathsf{0.5}$

Random Bit Generators (RBGs) can be used to generate uniformly distributed random numbers: a random number in the interval [0, n] can be obtained by 
generating a bit sequence of length $\mathsf{log(n) + 1}$ and converting it to an integer.

---

## Block Ciphers

---

## Public Key Cryptography

Definition [2](#public-key-cryptography).1 [**Public Key Encryption Scheme**]. Let $\mathsf{\langle Gen, E, D \rangle}$ be a triple of efficient algorithms, i.e., each one of them have a polynomial running time as well as the following properties:

1. $\mathsf{Gen}$ is a randomized algorithm: $$\mathsf{Gen: \text{ } \{0,1\}^{k} \rightarrow \{0,1\}^{n} \times \{0,1\}^{n}}$$ $\mathsf{Gen}$ produces a pair of keys $\mathsf{\langle k_{priv}, k_{pub} \rangle}$, namely private and public key, as a function of truly random number. For the sake of brevity, we'll refer to the key space as $\mathsf{K_{priv} \times K_{pub} = \{0,1\}^{n} \times \{0,1\}^{n}}$.

2. $\mathsf{E}$ is a randomized encryption algorithm, defined as $$\mathsf{E: \text{ } K_{pub} \times M \rightarrow C}$$ where $\mathsf{M}$ is the plaintext space and $\mathsf{C}$ is the ciphertext space.

3. $\mathsf{D}$ is a *deterministic* decryption algorithm, defined as $$\mathsf{D: \text{ } K_{priv} \times C \rightarrow M}$$

4. Fulfills the *consistency property*.


Informal security properties for a PK encryption scheme:

[1] **Non message recovery**. Given any ciphertext $\mathsf{c}$ and their related public keys $\mathsf{k_{pub}}$, it must be **computationally infeasible** to find the message $\mathsf{m}$ such that $\mathsf{c = E(k_{pub}, m)}$.

[2] **Non key recovery**. Given any public key $\mathsf{k_{pub}}$, it must be **computationally infeasible** to find the private key $\mathsf{k_{priv}}$.

Fact 1 [**PK Encryption Scheme is not perfect**]. Consider an attacker with ciphertext-only capability. Since, by definition, any public key used to encrypt ciphertext is publicly known, an attacker can always mount the following attack:

1. The attacker chooses a plaintext $\mathsf{x}$ such that $\mathsf{Pr(X = x) \neq 0}$.
2. The attacker encrypts $\mathsf{x}$ using the public key $\mathsf{k_{pub}}$.
3. If $\mathsf{E(k_{pub}, m) == y}$, then the attacker can claim that $\mathsf{Pr(X = x | Y = y) = 1}$. Otherwise, they can claim $\mathsf{Pr(X = x | Y = y) = 0}$. In both cases, the attacker gains some additional knowledge compared to the knowledge they had before attempting the attack.

Public key cryptography is based on the concept of *one-way function*:

> A function f() is a **one-way function** if 1) $\mathsf{y = f(x)}$ is computationally easy, and 2) $\mathsf{x = f^{-1}(y)}$ is computationally infeasible.

Definition [2](#public-key-cryptography).2 [**Digital Envelope**]. Public key cryptography is 2-3 orders of magnitude slower than symmetric key cryptography. The main application of public key cryptography is not doing bulk encryption/decryption but it is widely used to securely exchange session keys. A very simple implementation of a hybrid protocol which exploits both symmetric and public key cryptography could be described as follows:

```
              Alice                                       Bob
                |                                          |
            (Kpub, Kpriv)                             Ks = random()
                |                                          |
                |              "Alice", Kpub               | 
                +------------------------------------------>
                |                                          |
                |             M1 = E(Kpub, Ks)             |
                <------------------------------------------+
        Ks = D(Kpriv, M1)                                  |
                |                                          |
                |             M2 = AES(Ks, ...)            |
                +------------------------------------------>
                |                                          |
                .                                          .
                .                  Session                 .
                .                                          .
```

Problem 1 [**Key Authentication**] There is no hard proof that the identy of Alice is somehow linked to her public key $\mathsf{K^{A}_{pub}}$. With such a scheme, an active adversary can mount a MITM attack, successfully impersonating Alice w.r.t Bob (and vice-versa). Let's see hot the attack works:

```
        Alice                       Oscar                          Bob
        |                             |                             |
  (KpubA, KprivA)                (KpubO, KprivO)                 K' = random()
        |                             |                             |
        |                             |                             |
        |       "Alice", KpubA        |        "Alice", KpubO       |
        +-----------------------------/----------------------------->
        |                             |                             |
        |                             |                             |
        |                             |      M1 = E(KpubO, K')      |
        |                             /-----------------------------+
        |                     K' = E(KprivO, M1)                    |
        |                             |                             |
        |      M1' = E(KpubA, K')     |                             |
        <-----------------------------+                             |
 K' = D(Kpriv, M1')                   |                             |
        |                             |                             |
        |      M2 = AES(K', ...)      |      M2' = AES(K', ...)     |
        +-----------------------------/----------------------------->
        |                             |                             |
        .                             .                             .
        .                             .                             .
        .                             .                             .
```

Such a problem will be solved by *certificates*.


Why PK encryption schemes are vulnerable against small plaintext space attacks? Let's consider the following scenario:

1. An auctioneer holds a pair of $\mathsf{\langle K_{priv}^{A} \text{, } K_{pub}^{A}\rangle }$. All the participants know the legit auctioneer's public key (MITM attacks are not a problem).

2. Oscar, who is very smart, would like to win the auction spending the least amount of money he can. To pursue his malicious goal, he mount the following attack:

    a. Oscar intercept all the bids $\mathsf{b_i}$ encrypted by means of auctioneer public key $\mathsf{K_{pub}^{A}}$, resulting in $\mathsf{y_i}$.
    b. Each bid $\mathsf{b_i}$ consists of an unsigned integer $\mathsf{i \in \{0,1\}^{32}}$.
    c. For each bid, he tries to decrypt iterating through all the possible integers in $\mathsf{\{0,1\}^{32}}$ until he finds $\mathsf{i}$ such that $\mathsf{y_i == E(K_{pub}^A, i)}$. 
    d. Once he found the highest bid, namely $\mathsf{b^{\star}}$, he sends $\mathsf{E(K_{pub}^{A}, b^{\star} + 1)}$, winning the auction.

Is it computationally feasible such an attack? Absolutely fucking yes:

1. To "decrypt" one bid, Oscar should (at maximum) performs $\mathsf{2^{32}}$ attempts.

2. Total attack complexity: $\mathsf{O(2^{32}) + ... + O(2^{32}) = n \cdot O(2^{32}) \approx O(2^{32})}$

Is there a way to prevent the bidders from being scammed? Yes: **salting**.

1. Each bidder generates a random number $\mathsf{g \text{ } \in \text{ } \{0,1\}^{r}}$.

2. Now, instead of encrypting only the bid, the message to be encrypted is the following: $\mathsf{x' = x \text{ } | \text{ } g}$.

3. Oscar, to obtain the bid $\mathsf{x}$ from the encrypted message, has to try all the possible $\mathsf{g}$ as well: attack's complexity gets multiplied by $\mathsf{2^r}$.

---

## RSA Encryption Scheme

The RSA cryptosystem exploits the integer factorization as one-way function. Let's see how RSA primitives works.

Definition [3](#rsa-encryption-scheme).1 [**RSA Key Generation Algorithm**]

1. Find $\mathsf{p}$ and $\mathsf{q}$ two large prime integers.
2. Compute $\mathsf{n = p \cdot q}$.
3. Compute Euler's phi function, i.e., $\mathsf{\phi(n) = (p-1)(q-1)}$
4. Compute public exponent $\mathsf{e}$ such that $\mathsf{1 < e < \phi(n)}$ and $\mathsf{gcd(e, \phi(n)) = 1}$.
5. Compute private exponent $\mathsf{d}$ such that $\mathsf{1 < d < \phi(n)}$ and $\mathsf{e \cdot d \equiv 1 \text{ } mod \text{ } n}$.

    [5.a] Private key: $\mathsf{\langle d, n \rangle}$
    
    [5.b] Public key: $\mathsf{\langle e, n \rangle}$ 

How computationally difficult is it to generate a pair of RSA keys? Let's find out.

First things first, we need to address the types of operations required during the execution of the RSA Key Generation Algorithm:

1. We need to find large primes. The complexity of this task depends on the size of the set of integers we are examining and the computational cost of determining whether an integer is prime or not.

    1.a] There is a theorem, which are not going to prove, that describes how large primes are distributed across $\mathcal{\Z}$:

    > Given a large integer $\mathsf{x}$. The number of integers $\mathsf{\hat{x}}$ which are prime and in $\mathsf{[0,x]}$ is approximately $\mathsf{x/log(x)}$

    If we look only for odd integers (even integers are obviously not prime numbers), the probability to find a prime integer in $\mathsf{[0,x]}$ is $\mathsf{Pr(\hat{x} \text{ } is \text{ } prime) = \frac{x/log(x)}{x/2} = \frac{2}{log(x)}}$. On average, we need to look for $\mathsf{O(log(x))}$ integers in order to find a large prime.

    The task is not completed: how can be sure that an integer is also a prime? We need to run a primality test. Primality tests are computationally much easier than  factorization.

2. We need to find public and private exponent: the two of them can be simultaneously performed by means of EEA (Extend Euclidean Algorithm):

    2.a] Chose $\mathsf{e'}$ such that $\mathsf{1 < e < \phi(n)}$. Execute EEA, thus obtaining $\mathsf{e \cdot t + s \cdot \phi(n)}$: if $\mathsf{e \cdot t + s \cdot \phi(n) \equiv 1}$ (called Diophantine equation) then we found both public and private exponent ($\mathsf{t \equiv d \text{ } mod \text{ } \phi(n)}$).

    2.b] The number of steps required to execute EEA is approximately equals to the number of digits of the input parameter, i.e., $\mathsf{O(log(\phi(n)))}$.

Definition [3](#rsa-encryption-scheme).2 [**RSA encryption**]. Let $\mathsf{x}$ be an integer such that $\mathsf{x \in [0,n-1]}$. The RSA encryption algorithm produces $\mathsf{y \in [1,n-1]}$ as follows: 
$$\mathsf{y = x^e \text{ } mod \text{ } n}$$

Definition [3](#rsa-encryption-scheme).3 [**RSA decryption**]. Let $\mathsf{y}$ be an integer such that $\mathsf{y \in [0,n-1]}$. The RSA decryption algorithm produces $\mathsf{x \in [1,n-1]}$ as follows: 
$$\mathsf{x = y^d \text{ } mod \text{ } n}$$

How much is the cost of RSA encryption/decryption? Both algorithm work with modular exponentiation. Grade-school discrete exponentiation requires $\mathsf{n-1 = O(n)}$ multiplications with $\mathsf{log(n)}$-bit long operands. It's quite costly: if $\mathsf{n}$ were around $\mathsf{O(2^{1024})}$ then we would need to execute $\mathsf{1024}$-bit long multiplication. 

We need to find a more efficient way to compute modular exponentiation: **square-and-multiply** algorithm. Let's present the algorithm:

1. Consider an integer $\mathsf{x \in [0, n-1]}$. Consider an integer exponent $\mathsf{e}$. If we want to efficiently compute $\mathsf{x^e \text{ } mod \text{ } n}$, we should take a look at the binary representation of $\mathsf{e}$

2. Given $\mathsf{e = 2^{3}e_3 + 2^{2}e_2 + 2^{1}e_1 + e_0}$, where $\mathsf{e_i}$ are binary digits.

    > $\mathsf{x^e \text{ } mod \text{ } n}$ = \
    > = $\mathsf{x^{2^{3}e_3 + 2^{2}e_2 + 2^{1}e_1 + e_0} \text{ } mod \text{ } n}$ = \
    > = $\mathsf{x^{2^{3}e_3} \cdot x^{2^{2}e_2} \cdot x^{2^{1}e_1} \cdot x^{e_0} \text{ } mod \text{ } n}$ = \
    > = $\mathsf{(x^{2^{3}e_3} \cdot x^{2^{2}e_2} \cdot x^{2^{1}e_1}) \cdot x^{e_0} \text{ } mod \text{ } n}$ = \
    > = $\mathsf{(x^{2^{2}e_3} \cdot x^{2^{1}e_2} \cdot x^{e_1})^2 \cdot x^{e_0} \text{ } mod \text{ } n}$ = \
    > = $\mathsf{((x^{2e_3} \cdot x^{e_2})^2 \cdot x^{e_1})^2 \cdot x^{e_0} \text{ } mod \text{ } n}$ = \
    > = $\mathsf{(((x^{e_3})^2 \cdot x^{e_2})^2 \cdot x^{e_1})^2 \cdot x^{e_0} \text{ } mod \text{ } n}$ (1.1)

3. By looking at (1.1) we can see that to compute $\mathsf{x^e \text{ } mod \text{ } n}$ we need to perform 3 discrete exponentiations (square operations) and at maximum 4 multiplications: the number of multiplications depends on the Hamming weight of the exponent, i.e., the number of ones in its binary representation.

```python
c = 1
for (i = k-1; i >= 0; k--) {
    c = c**2 mod n
    if (e[i] == 1)
        c = c * x mod n
}
```

Overall, the complexity of the square-and-multiply algorithm is $\mathsf{O(\log(e))}$ square operations plus, on average, $\mathsf{0.5 \cdot O(\log(e))}$ integer multiplications modulo $n$.

RSA encryption could be optimized by using small exponents, i.e., public exponents with low Hamming weight. It's important to note that using small $\mathsf{e}$ is not inherently insecure (at least under certain assumptions). However, small private exponents should be avoided. If a small $\mathsf{d}$ is used, RSA could be brute-forced by enumerating all possible private exponents.

Let's examine the bit complexity of the decryption algorithm:

1. Consider a modulus $\mathsf{n}$ which is approximately $\mathsf{O(2^{2048})}$.

2. If we were using the square-and-multiply algorithm, we would need to perform (on average) $\mathsf{1.5 \cdot \log(n) = 2048 + 1024 = 3072}$ integer multiplications in order to decrypt a ciphertext (encryption with a small exponent is much more efficient).

3. Each operand requires $\mathsf{2048 / r}$ registers, where $\mathsf{r}$ is the register's bit-size (either 32 or 64). Assuming we were using a 64-bit CPU, each operand would require $\mathsf{2048 / 64 = 32}$ registers.

4. For one 2048-bit integer multiplication, we would need to compute $\mathsf{32^2}$ integer 64-bit multiplications.

5. For one 2048-bit integer modulo reduction, we would need to compute $\mathsf{32^2}$ integer 64-bit modulo reductions.

The total cost for one run of the RSA decryption algorithm would be $\mathsf{3072 \cdot (32^2 + 32^2)}$ integer 64-bit operations.

Fact 2 [**Schoolbook RSA is malleable**]. 

- Schoolbook RSA is malleable. An encryption scheme is said to be malleable if an attacker is able to apply a perturbation to a ciphertext which lead to a predictable transformation of the ciphertext. \
Consider the following example:
    
    a. Alice sends to Bob $\mathsf{y = x^e \text{ } mod \text{ } n}$. \
    b. Oscar, a bad guy, knows both $\mathsf{e}$ and $\mathsf{n}$ (keep in mind that $\mathsf{\langle e, n \rangle}$ is the RSA public key). Oscar then choses $\mathsf{s}$ such that $\mathsf{gcd(s,n) = 1}$ and computes $\mathsf{y' = y \cdot s^e \text{ } mod \text{ } n}$. Oscar forwards $\mathsf{y'}$ to Bob. \
    c. Bob receives $\mathsf{y'}$ and decrypts it obtaining $\mathsf{(y')^d \text{ } mod \text{ } n = (x^e \cdot s^e)^d \text{ } mod \text{ } n = x \cdot s}$.

---

## Diffie-Hellman Key Exchange Protocol

The Diffie-Hellman Key Exchange protocol (DHKE) is a key establishment protocol based on public key cryptography and, more specifically, exploits the discrete logarithm problem as a one-way function. Let's delve into the details of the protocol.

**Setup phase** - Before the legitimate communicating parties (from now on Alice and Bob) can exchange some piece of information to build a shared secret session key, they need to agree on a set of parameters (called Diffie-Hellman public parameters):

- Alice and Bob agree on a *large* prime $\mathsf{p}$.

- Alice and Bob agree on a integer $\mathsf{1 < g < p}$ such that it is a generator for $\mathsf{Z^{\star}_{p}}$. What does it mean "generator"? An integer $\mathsf{\alpha \in \{2, ..., p-2\}}$ is said to be a generator of the group $\mathsf{Z^{\star}_{p}}$ if its order is *maximum*, i.e., $\mathsf{ord(\alpha) = |Z^{\star}_{p}| = p-1}$.

**Key establishment phase** - The protocol runs with just two messages:

- Alice --> Bob: "Alice", $\mathsf{Y_{A} = g^a \; mod \; p}$

- Bob --> Alice: "Bob", $\mathsf{Y_{B} = g^b \; mod \; p}$

Parameters $\mathsf{1 < a < p-1}$ and $\mathsf{1 < b < p-1}$ are two truly-random integers generated by Alice and Bob, respectively. 

The shared session key is then computed as:

- Alice computes $\mathsf{(g^b)^a \; mod \; p = g^{ba} \; mod \; p = g^{ab} \; mod \; p}$.

- Bob computes $\mathsf{(g^a)^b \; mod \; p = g^{ab} \; mod \; p}$.

Pay attention: $\mathsf{a}$ and $\mathsf{b}$ **must** be kept private. If either $\mathsf{a}$ or $\mathsf{b}$ were to be disclosed, an attacker could solve the discrete logarithm problem. Let's talk about what are the security aspects of the protocol.

1. From a passive adversary perspective, the security of the DHKE protocol relies on the difficulty of the Discrete Logarithm Problem (DLP). The DLP involves finding the exponent in the modular exponentiation equation, such as $\mathsf{log_{g}(Y_A) \; mod \; p}$ or $\mathsf{ log_{g}(Y_B) \; mod \; p}$, where $\mathsf{Y_A}$ and $\mathsf{Y_B}$ are public keys. If $p$ is chosen to be large enough, solving the DLP becomes computationally infeasible, providing security against passive eavesdropping.

2. From an active adversary perspective, the DHKE protocol is insecure due to its lack of authentication. An active adversary can mount a MITM attack, impersonating Bob w.r.t. Alice's perspective and vice versa. Let's study how the attack works:

    2.1 Oscar, a malicious actor, intercepts Alice's message \{"Alice", $\mathsf{Y_{A} = g^a \; mod \; p}$\}. Then, he chooses $\mathsf{c \in \{2, ..., p-2\}}$ and substitutes $\mathsf{Y_{A}}$ with $\mathsf{Y_{A}' = g^c \; mod \; p}$. 

    2.2 Once Bob receives \{"Alice", $\mathsf{Y_{A}' = g^c \; mod \; p}$\}, he believes that Alice is attempting to establish a shared secret key with him. Bob computes the shared secret key as $\mathsf{K_{AB}' = (g^c)^b \; mod \; p = g^{cb} \; mod \; p}$.

    2.3 Upon Bob sends his public key, i.e., \{"Bob", $\mathsf{Y_{B} = g^b \; mod \; p}$\}, Oscar intercepts it and substitutes $\mathsf{Y_{B}}$ with $\mathsf{Y_{B}' = g^c \; mod \; p}$. 

    2.4 Once Alice receives \{"Bob", $\mathsf{Y_{B}' = g^c \; mod \; p}$\}, she believes that Bob replies to her. Alice computes the shared secret key as $\mathsf{K_{AB}'' = (g^c)^a \; mod \; p = g^{ca} \; mod \; p}$.

    2.5 As Oscar controls $\mathsf{c}$, he can compute both $\mathsf{K_{AB}'}$ and $\mathsf{K_{AB}''}$. For each message sent by Alice, Oscar decrypts   it with $\mathsf{K_{AB}''}$, reads the plaintext, and forwards the plaintext to Bob by encrypting it with $\mathsf{K_{AB}'}$.

---

**Why we should not work on $\mathsf{Z_{p}^{\star}}$?**

DLP can be attacked from various angles, with both generic and non-generic algorithms having been studied to solve the problem as efficiently as possible. Now, let's delve into the details.

**Generic Algorithms**:

- **Brute Force** - As usual, one can solve DLP by just enumerating all the possible elements in the group $\mathsf{G}$. Complexity: $\mathsf{O(|G|)}$.

- **Shank's Baby-Step Giant-Step Method** - More efficient than brute forcing, but not very efficient in terms of storage complexity. Running time: $\mathsf{O(\sqrt{|G|})}$. Storage complexity: $\mathsf{O(\sqrt{|G|})}$. 

- **Pollard's Rho Method** - More efficient than *Shank's Baby-Step Giant-Step Method* in terms of storage complexity. Running time: $\mathsf{O(\sqrt{|G|})}$. Storage complexity: negligible.

- **Pohlig-Hellman Method** - It exploits the CRT and the factorization of the group's cardinality. Complexity: $\mathsf{O[\sum_{i} e_i \cdot (log(|G|) + \sqrt{p_i})]}$, where $\mathsf{|G| = \prod_{i} p_i ^{e_i}}$.

So, what is the problem with $\mathsf{Z_{p}^{\star}}$? Since $\mathsf{p}$ is prime, $\mathsf{p-1}$ is even. This property opens the possibility that the Pohlig-Hellman method could be efficient in $\mathsf{Z_{p}^{\star}}$ because the factors of $\mathsf{|G| = p-1}$ may not be large. We know that at least 2 is a factor of $\mathsf{|G|}$, and there is a good chance that there are many small factors in $\mathsf{|G|}$, potentially diminishing the complexity of solving the DLP. To prevent the attack the smallest factor of |G| must be in the range $\mathsf{2^{160}}$.

**Small Subgroup Confinement Attack**

A (small) subgroup confinement attack on a cryptographic method that operates in a large finite group is where an attacker attempts to compromise the method by forcing a key to be confined to an unexpectedly small subgroup of the desired group.

Theorem [4](#diffie-hellman-key-exchange-protocol).1 - Given a group $\mathsf{G}$ whose cardinality is $\mathsf{|G| = n}$, for each $\mathsf{k}$ that divides $\mathsf{n}$, we can define a subgroup $\mathsf{H}$ whose cardinality is $\mathsf{|H| = k}$. Therefore, for each subgroup $\mathsf{H}$ exists a generator $\mathsf{g_{h}}$ defined as $\mathsf{g_{h} = \alpha^{n/k}}$, where $\mathsf{\alpha}$ is a generator of $\mathsf{G}$.

We have everything we need to mount a small confinement attack!

1. A malicious actor, Oscar, intercepts Alice's message {"Alice", $\mathsf{Y_{A}' = g^a \; mod \; p}$}. Since $\mathsf{p}$ is publicly known, Oscar can compute $\mathsf{|G| = p-1}$. Once he determines $\mathsf{n = |G|}$, he can find a 'small' integer $\mathsf{k}$ that divides $\mathsf{n}$.

2. Oscar finds $\mathsf{k}$ and substitutes $\mathsf{Y_{A}}$ with $\mathsf{Y_{A}' = (g^a)^{n/k} \; mod \; p}$. If Oscar has enough computational power, he could compute $\mathsf{(g^{n/k})^{a} \; mod \; p}$, which is less "difficult" than $\mathsf{(g^a) \; mod \; p}$.

---

## Perfect Forward Secrecy

What is the problem of perfect forward secrecy? Consider the following key-establishment protocol:

> [**M1**] Alice --> Bob: $\mathsf{E(K_{AB}, K_{s})}$
>
> [**M2**] Bob --> Alice: $\mathsf{E(K_{s}, session)}$

What happens if the long-term shared secret $\mathsf{K_{AB}}$ were to be disclosed? Both previous and next runs of the protocol are not longer secure because an attacker can decrypt message M1 and (s)he can recover the session key $\mathsf{K_{s}}$ being used for the encryption.

Definition [5](#perfect-forward-secrecy).1 [**Perfect Forward Secrecy**]. A protocol has the perfect forward secrecy property if, even in the scenario in which some long-term secret keying material has been disclosed, the security (i.e., the confidentiality) of previous runs of the protocol is not compromised.

Let's explore examples of protocols that fulfill perfect forward secrecy.


(**1**) Pre-Shared-Key Diffie-Hellman Key Exchange (**PSK-DHKE**):

> [**M1**] Alice --> Bob: $\mathsf{E(K_{AB}, \; g^a \; mod \; p)}$
>
> [**M2**] Bob --> Alice: $\mathsf{E(K_{AB}, \; g^b \; mod \; p)}$
>
> Alice computes $\mathsf{K_{s} = (g^b)^a \; mod \; p = g^{ab} \; mod \; p}$, Bob computes $\mathsf{K_{s} = (g^a)^b \; mod \; p = g^{ab} \; mod \; p}$. Both $\mathsf{a}$ and $\mathsf{b}$ should be deleted after the session key has been computed.
>
> [**M3**] Alice --> Bob: $\mathsf{E(K_{s}, session)}$

Please note that even if an attacker were able to obtain $\mathsf{K_{AB}}$, (s)he would need to solve the discrete logarithm problem because the decrypted messages do not carry $\mathsf{a}$ or $\mathsf{b}$.

(**2**) Ephemeral RSA (**RSAE**)

> Alice computes a pair of ephemeral RSA keys $\mathsf{\langle T_{pub}, T_{priv} \rangle}$ as well as Alice holds a pair of long-term $\mathsf{\langle K_{pub}, K_{priv} \rangle }$. 
>
> [**M1**] Bob --> Alice: Request
>
> [**M2**] Alice --> Bob: $\mathsf{T_{pub} \text{, } Sign(K_{priv}, \; T_{pub} \: || \: Request) \text{, } Cert_{A}}$.
>
> [**M3**] Bob --> Alice: $\mathsf{E(T_{pub}, K_{s})}$
>
> Alice computes $\mathsf{K_{s} = D(T_{priv}, M2)}$. Ephemeral RSA keys should be deleted after the session key has been computed.
>
> [**M4**] Alice --> Bob: $\mathsf{E(K_{s}, session)}$

Defintion [5](#perfect-forward-secrecy).2 [**Direct Authentication**]. To prove the peer the knowledge of the session key $\mathsf{K_{s}}$.

If a key exchange protocol does not fulfil direct authentication, this authentication is achieved at the first application message. PSK-DHKE and RSAE don’t fulfil direct authentication!

(**3**) Station-to-Station protocol (**STS**)

> **Setup phase**: Alice and Bob agreed on a set of public parameters $\mathsf{\langle g \text{, } p \rangle}$.
>
> [**M1**] Alice --> Bob: $\mathsf{"Alice" \text{, } g^a \; mod \; p}$
>
> [**M2**] Bob --> Alice: $\mathsf{"Bob" \text{, } g^b \; mod \; p \text{, } E(K_{s}, Sign\{K_{priv}^{B}, g^a \: || \: g^b \}) \text{, } Cert_{B}}$
>
> [**M3**] Alice --> Bob: $\mathsf{E(K_{s}, Sign\{K_{priv}^{A}, g^b \: || \: g^a \}) \text{, } Cert_{A}}$
>
> Alice checks that Bob holds the session key thanks to message **M2**. On the other hand, Bob checks that Alice holds the session key $\mathsf{K_{s}}$ thanks to message **M3**.
>
> [**M4**] Bob --> Alice: $\mathsf{E(K_{s}, session)}$