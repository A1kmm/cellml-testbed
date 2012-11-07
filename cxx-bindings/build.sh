#!/bin/bash
GHCDIR=/usr/lib/ghc
GHCVER=ghc-7.6.1
CABALDIR=~/.cabal
ROOTDIR=$(dirname $0)/..

ghc -fllvm -fPIC -dynamic -hide-package mtl ./CellMLMarshal.hs

LIST=""
for L in $(ls -f $ROOTDIR/cellml12/dist/build/libHScellml12-*.so $ROOTDIR/cmathml3/dist/build/libHScmathml3-*.so $ROOTDIR/arrowapply-utils/dist/build/libHSarrowapply-utils-*.so $CABALDIR/lib/hxt-9.3.1.1/$GHCVER/libHShxt-*.so $CABALDIR/lib/hxt-charproperties-9.1.1/$GHCVER/libHShxt-charproperties-*.so $CABALDIR/lib/hxt-unicode-9.0.2/$GHCVER/libHShxt-*.so $CABALDIR/lib/parsec-3.1.3/$GHCVER/libHSparsec-*.so $CABALDIR/lib/mtl-2.1.2/$GHCVER/libHSmtl-*.so $CABALDIR/lib/uniplate-1.6.7/$GHCVER/libHSuniplate-*.so $CABALDIR/lib/unordered-containers-0.2.2.1/$GHCVER/libHSunordered-containers-*.so $CABALDIR/lib/hashable-1.1.2.5/$GHCVER/libHShashable-*.so $CABALDIR/lib/syb-0.3.7/$GHCVER/libHSsyb*.so $CABALDIR/lib/monadio-unwrappable-0.3/$GHCVER/libHSmonadio-unwrappable-*.so $CABALDIR/lib/curl-1.3.7/$GHCVER/libHScurl-*.so $CABALDIR/lib/cereal-0.3.5.2/$GHCVER/libHScereal-*.so $CABALDIR/lib/monads-tf-0.1.0.1/$GHCVER/libHSmonads-tf-*.so $CABALDIR/lib/temporary-1.1.2.4/$GHCVER/libHStemporary-*.so $CABALDIR/lib/network-2.4.0.1/$GHCVER/libHSnetwork-*.so $GHCDIR/process-*/libHSprocess-*.so $GHCDIR/directory-*/libHSdirectory-*.so $GHCDIR/filepath-*/libHSfilepath-*.so $CABALDIR/lib/transformers-0.3.0.0/$GHCVER/libHStransformers-*.so $GHCDIR/unix-*/libHSunix-*.so $GHCDIR/binary-*/libHSbinary-*.so $GHCDIR/containers-*/libHScontainers-*.so $GHCDIR/bytestring-*/libHSbytestring-*.so $GHCDIR/deepseq-*/libHSdeepseq-*.so $GHCDIR/array-*/libHSarray-*.so $GHCDIR/libHSrts_thr-ghc7.6.1.so $GHCDIR/base-4.*/libHSbase-4.*.so $GHCDIR/integer-gmp-0.*/libHSinteger-gmp-0.*.so $GHCDIR/ghc-prim-0.*/libHSghc-prim-0.*.so); do LIST="$LIST $(readlink -f $L)"; done

mkdir cellml-testbed-bindings/bin
echo "#!/bin/bash" >cellml-testbed-bindings/bin/cellml-testbed-config
echo echo \"-I\$\(dirname \$0\)/../include -L\$\(dirname \$0\)/../lib $(ls cellml-testbed-bindings/lib/*.so | perl -pe "s#cellml-testbed-bindings/lib/lib([^ ]+)\.so#\-l\1#g")\" >>cellml-testbed-bindings/bin/cellml-testbed-config
chmod +x cellml-testbed-bindings/bin/cellml-testbed-config


g++ -fPIC -shared -O3 CellMLMarshal.o -Icellml-testbed-bindings/include -I$GHCDIR/include ./cellml-testbed-bindings.cpp $LIST -o libcellml-testbed.so
rm -fr cellml-testbed-bindings/lib
mkdir cellml-testbed-bindings/lib
cp $LIST libcellml-testbed.so cellml-testbed-bindings/lib

tar -cjf cellml-testbed-x86_64.tar.bz2 cellml-testbed-bindings
