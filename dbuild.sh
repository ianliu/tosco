if [ $# -lt 2 ]; then
	echo $0 version description
	exit 1;
fi

if [ ! -e export ]; then
	mkdir export && sudo mount -t tmpfs tmpfs export
	if [ -e .hg ]; then
       		hg archive export && rm export/.hg*
	fi
	if [ -e .svn ]; then
		svn export --force . export
                cp tosco-su-install export/ 
	fi
	cd export
	dch -v $*
else
	echo Recicled export
fi

for ARCH in amd64 i386; do
    # for DIST in jaunty lenny karmic lucid; do
    for DIST in jaunty ; do
	sudo rm -f /export_local/pbuilder/$DIST-$ARCH/result/*
	sudo mount -t tmpfs tmpfs /var/cache/pbuilder/build

	# cd su
	# perl -i -pe "s/\) .*; urgency/) $DIST; urgency/" */debian/changelog
	# ARCH=$ARCH pdebuild && \
	# sudo dput -u local /export_local/pbuilder/$DIST-$ARCH/result/*changes && \
	# sudo DIST=$DIST ARCH=$ARCH pbuilder --update || exit 22
        # cd ..

	perl -i -pe "s/\) .*; urgency/) $DIST; urgency/" */debian/changelog
	ARCH=$ARCH pdebuild && \
	sudo dput -u local /export_local/pbuilder/$DIST-$ARCH/result/*changes && \
	sudo umount /var/cache/pbuilder/build || exit 55
    done
done
cd ..
sudo umount export
rmdir export


	# debsign -kA594D681 /export_local/pbuilder/$DIST-$ARCH/result/*changes && \
	# sudo dput local /export_local/pbuilder/$DIST-$ARCH/result/*changes && \
