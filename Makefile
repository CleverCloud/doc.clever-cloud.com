all: build

mrproper: clean
	cabal clean

publish: build
	git stash save
	git checkout publish || git checkout --orphan publish
	mkdir _source
	find . -maxdepth 1 ! -name _source ! -name . ! -name .git -exec mv '{}' _source/ \;
	cp -r _source/_site/. ./
	rm -fr _source
	git add -A . && git commit -m "Publish" || true
	rm -fr ./*
	git push -f git+ssh://git@push.clever-cloud.com/app_1c28a3c0-6e4c-4064-8c41-edb2b4a582c3.git publish:master
	git checkout master
	git checkout -- .
	git stash pop || true

preprodpublish: build
	git stash save
	git checkout publish || git checkout --orphan publish
	mkdir _source
	find . -maxdepth 1 ! -name _source ! -name . ! -name .git -exec mv '{}' _source/ \;
	cp -r _source/_site/. ./
	rm -fr _source
	git add -A . && git commit -m "Publish" || true
	rm -fr ./*
	git push -f git+ssh://git@push.clever-cloud.com/app_c9396b59-7f1b-42c1-9855-90b7cc135dc9.git publish:master
	git checkout master
	git checkout -- .
	git stash pop || true

cabal.sandbox.config:
	cabal sandbox init --sandbox=../hakyll-cabal-sandbox

build: dist/build/doc-clevercloud/doc-clevercloud
	./dist/build/doc-clevercloud/doc-clevercloud build

dist/build/doc-clevercloud/doc-clevercloud: Main.hs cabal.sandbox.config
	cabal build
	./dist/build/doc-clevercloud/doc-clevercloud clean


preview: dist/build/doc-clevercloud/doc-clevercloud
	./dist/build/doc-clevercloud/doc-clevercloud preview -p 9000

clean: dist/build/doc-clevercloud/doc-clevercloud
	./dist/build/doc-clevercloud/doc-clevercloud clean
