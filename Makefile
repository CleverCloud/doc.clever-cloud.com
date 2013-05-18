preview: site
	./site preview

site: site.hs
	ghc --make site.hs
	./site clean

clean: site
	./site clean

build: site
	./site build

debug: site
	./site clean
	./site build -v

#publish: build
#	git stash save
#	git checkout publish
#	mkdir _source
#	find . -maxdepth 1 ! -name _source ! -name . ! -name .git -exec mv '{}' _source/ \;
#	cp -r _source/_site/. ./
#	rm -fr _source
#	git add -A . && git commit -m "Publish" || true
#	rm -fr ./*
#	git push cc publish:master
#	git checkout master
#	git checkout -- .
#	git stash pop || true