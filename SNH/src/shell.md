---
title: Linux Shell
author: Federico Casu
date: February 29, 2024
---

# Come funziona la shell di Linux

La shell è, probabilmente, lo strumento più importante a disposizione di un hacker. Per tale motivo dobbiamo studiare con attenzione come questo strumento funziona in modo da capirne le potenzialità e, purtroppo, le vulnerabilità. Iniziamo con il dire che la shell, come qualsiasi altro programma in Linux, non è magico: tutto ciò che la shell è in grado di offrirci può essere replicato anche da noi! La shell non è altro che un programma che interpreta e, eventualmente, esegue i commandi inseriti dall'utente. Vediamo come una (semplice) shell può essere implementata:

```C
#include <stdio.h>
#include <stdlib.h>     // for perror()
#include <unistd.h>
#include <sys/wait.h>

int main()
{
    char buffer[MAX_LINE];
    int n = 0;
    
    while ( (n = read(0, buffer, 1024)) > 1) 
    {    
        buffer[n-1] = '\0';   
        if (fork()) {
            wait(0);
        } else {
            execl(buffer, buffer, NULL);
            perror(buffer);
            exit(1);
        }
    }
    return 0;
}
``` 

Il codice è molto semplice: 

1. Il programma, ciclicamente, legge il comando inserito dall'utente.
2. Crea un nuovo processo che, tramite la funzione `execl()`, esegue il comando inserito dall'utente. Sia che l'esecuzione del comando vada a buon fine, sia nel caso opposto, il processo figlio termina.
3. Il processo padre attende che l'esecuzione del comando termini e si pone in attesa di un nuovo comando. 

Dal punto di vista dell'utente, la shell si comporta come un interprete. Attenzione: la shell non interpreta i comandi impartiti ma si limita a 1) creare un nuovo processo e 2) eseguire il programma specificato dal comando nel contesto del nuovo processo. 

Proviamo ad eseguire il programma `sh0`:

```zsh
(kali@kali)-[~/Documents/SNH/myUnix-2.1.3]$ shell/sh0
/bin/ls
lib  README.md  root  shell  src  util
/bin/pwd
/home/kali/Documents/SNH/myUnix-2.1.3
```

Funziona! Oltre al normale funzionamento, questa shell di base implementa alcune funzionalità di editing come, ad esempio, `Ctrl+W`[^1]. Ma in quale parte di `sh0.c` tale funzionalità è implementata? In realtà, la shell non implementa alcuna funzionalità di editing (in `sh0.c` non c'è riga di codice che si occupa di gestire `Ctrl+W`). Di fatto, è il kernel ad implementare le funzionalità di editing e, grazie alla syscall `read()`, il programma `sh0` "eredita" queste funzionalità.

[^1]: `Ctrl+W` cancella tutti i caratteri fino all'ultimo backspace inserito.

Nella prima prova di `sh0` abbiamo specificato il path assoluto per ogni comando impartito. Cosa succede se inseriamo solamente il nome del comando?

```zsh
(kali@kali)-[~/Documents/SNH/myUnix-2.1.3]$ shell/sh0
ls
ls: No such file or directory
```

Perchè non funziona? Ragioniamo: 

1. `sh0` passa "`ls`" come primo e secondo argomento alla funzione `execl()`. Quest'ultima è una funzione della libreria `<unistd.h>` che semplifica l'utilizzo della syscall `execv()`. Dal nostro punto di vista, `execl()` si comporta come il seguente codice:

```C
	char* argv[] = {"ls", NULL};
	execv(argv[0], argv, NULL);	// execv("ls", argv, NULL);
```

2. Quando il flusso di esecuzione passa alla primitiva `execv()`, quest'ultima compie la ricerca del programma "`ls`" applicando le regole di risoluzione dei path previste dal kernel. `ls` è un *path relativo*: il kernel non trova alcun file (o directory) nominato `ls` nella working directory corrente.

Esiste una soluzione per aggirare questo problema? Ovviamente si: `execlp()`. Vediamo come usarla:

```C
int execlp(const char *file, const char *arg, ... /*, NULL */);
```

**Descrizione** [`man execlp`]. This function duplicates the actions of the shell in searching  for an executable file if the specified filename does not contain a slash (`/`) character. The file is sought in the colon‐separated list of directory pathnames specified in the PATH  environment variable. If this  variable isn’t defined, the path list defaults to a list that includes the directories returned by  `confstr(_CS_PATH)` (which typically returns  the value "`/bin:/usr/bin`") and possibly also the current working directory. If the searching process completes succesfully, then the resolved path will be passed to the syscall `execv()`.

Dunque, modifichiamo `sh0.c` rimpiazzando `execl()` con `execlp()`. Ecco che otteniamo il programma `sh1.c`.

```C
#include <stdio.h>
#include <stdlib.h>     // for perror()
#include <unistd.h>
#include <sys/wait.h>

int main()
{
    char buffer[MAX_LINE];
    int n = 0;
    
    while ( (n = read(0, buffer, 1024)) > 1) 
    {    
        buffer[n-1] = '\0';   
        if (fork()) {
            wait(0);
        } else {
            execlp(buffer, buffer, NULL);
            perror(buffer);
            exit(1);
        }
    }
    return 0;
}
```

```zsh
(kali@kali)-[~/Documents/SNH/myUnix-2.1.3]$ shell/sh1
ls
lib  README.md  root  shell  src  util
pwd
/home/kali/Documents/SNH/myUnix-2.1.3
```

Facciamo un passo avanti e proviamo ad impartire, oltre al comando, alcuni argomenti.

```zsh
(kali@kali)-[~/Documents/SNH/myUnix-2.1.3]$ shell/sh1
ls -l
ls -l: No such file or directory
```

Analizziamo il problema:

1. `read()` scrive in `buffer` i caratteri inseriti dall'utente. Quando `read()` termina, `buffer` contiene la stringa "`ls -l\n`".
2. Lo statement `buffer[n-1] = '\0';` sostituisce il simbolo newline con il terminatore di stringa '`\0`'.
3. Il processo figlio chiama la funzione `execlp()` e passa come primo argomento il contenuto di `buffer`, cioè, "`ls -l\0`". Quest'ultimo viene interpretato come il nome di un file eseguibile (e non come un path visto che non contiene alcun `/`), quindi, la funzione `execlp()` cerca un file chiamato "`ls -l`". Sfortunatamente per noi non esiste alcun file "`ls -l`" nelle directory presenti in PATH o nella working directory corrente.

Ne la funzione `execlp()`, ne tantomento la syscall `execv()` compiono alcun tipo di parsing degli argomenti. Dobbiamo essere noi ad implementare un meccanismo che ci permetta di ricoscere se e quali sono gli argomenti da passare al comando. In Linux, oltre a ciò precedentemente detto, la shell si occupa di eseguire il parsing degli argomenti.

Proviamo ad implementare un semplice parser (`lsh.c`):

```C
#include <stdio.h>    /* for perror() */
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

#define MAX_LINE 1024
#define MAX_ARGS 10

/* shell that splits lines into words */

int main()
{
	char buf[MAX_LINE];
	int i, n;
	int c_argc;
	char *c_argv[MAX_ARGS + 1];

	while ( (n = read(0, buf, MAX_LINE)) > 1 ) {

		buf[n - 1] = '\0';

		c_argv[0] = buf;
		c_argc = 1;
		for (i = 0; i < n && c_argc < MAX_ARGS; i++) {
	        if (buf[i] == ' ') {
                buf[i] = '\0';
                c_argv[c_argc++] = &buf[i + 1];
	        }
		}
		c_argv[c_argc] = NULL;

		if (fork()) {
		    wait(0);
		} else {
	        execvp(buf, c_argv);
	        perror(buf);
	        exit(1);
		}
	}
	return 0;
}
```

