run: dns.xq clean run.sh
	mkdir $@
	cp control.xml run
	cp dns.xqi run
	cp resolver.xq run
	cp -r queries $@
	./run.sh

start:
	exec galaxd -s ./run -language xquerybang &
	sleep 3
	exec webgui -s ./run &

stop:
	killall galaxd webgui

clean:
	$(RM) -r run

