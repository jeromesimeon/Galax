FROM ocaml/opam2:centos
WORKDIR /opt/src
RUN set -ex; \
 sudo chown -R opam /opt; \
 sudo yum install -y pcre-devel; \
 git clone https://github.com/tenpercent/Galax.git; \
 opam install pxp camomile camlidl num pcre;
WORKDIR Galax
RUN set -ex; \
 eval $(opam env); \
 ./configure -local /opt; \
 make -d 2>&1 | tee make.out; \
 make install; \
 make realclean
ENV PATH="/opt/galax/bin:${PATH}"
