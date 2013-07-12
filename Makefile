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

publish: build
	git stash save
	git checkout publish || git checkout --orphan publish
	mkdir _source
	find . -maxdepth 1 ! -name _source ! -name . ! -name .git -exec mv '{}' _source/ \;
	cp -r _source/_site/. ./
	rm -fr _source
	git add -A . && git commit -m "Publish" || true
	rm -fr ./*
	git push -f git+ssh://git@push.clever-cloud.com/app_90ba6f3c-065a-471b-9ca1-f2e98fcdddb3.git publish:master
	git checkout master
	git checkout -- .
	git stash pop || true

testpublish: build
	git stash save
	git checkout publish || git checkout --orphan publish
	mkdir _source
	find . -maxdepth 1 ! -name _source ! -name . ! -name .git -exec mv '{}' _source/ \;
	cp -r _source/_site/. ./
	rm -fr _source
	git add -A . && git commit -m "Publish" || true
	rm -fr ./*
	git push -f git+ssh://git@push.clever-cloud.com/app_da73f1cf-915c-4fb1-af1f-a21ed1cfaf1c.git publish:master
	git checkout master
	git checkout -- .
	git stash pop || true