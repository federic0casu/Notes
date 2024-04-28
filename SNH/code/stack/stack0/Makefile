DISABLE=-z execstack -no-pie -fcf-protection=none -z norelro -Wno-format-security

stack0: stack0.c printflag.c
	gcc -g $(DISABLE) -fno-stack-protector stack0.c printflag.c -o stack0

install: stack0
	echo 'SNH{You_doing_great_stack0}' > flag.txt
	sudo chown root flag.txt
	sudo chgrp root flag.txt
	sudo chmod go-wr flag.txt
	sudo chown root stack0
	sudo chgrp root stack0
	sudo chmod u+s stack0

.PHONY: clean
clean:
	rm -f stack0 flag.txt
