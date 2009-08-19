all: cluster run

run: clean narada.xq run.sh 
	$(RM) -r run
	mkdir $@
	cp narada.xqi $@
	cp -r queries $@
	./$@.sh	
	(cd $@; ln -s ../run-latencies.xml latencies.xml)

cluster: clean narada.xq cluster.sh 
	mkdir $@
	cp narada.xqi $@
	./$@.sh	
	(cd $@; ln -s ../cluster-latencies.xml latencies.xml)

go:
	~/galax/galaxd -s run -t latencies.xml

clean:
	$(RM) -r run
	$(RM) -r cluster
