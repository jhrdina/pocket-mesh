build:
	dune build

run: build
	./_build/install/default/bin/signal-server

test:
	dune runtest

# some boilerplate to publish a new version to GitHub
release:
	git add package.json opam
	git commit -m "Version $(version)"
	git tag -a $(version) -m "Version $(version)."
	git push "git@github.com:jhrdina/pocket-mesh-signal-server.git"
	git push "git@github.com:jhrdina/pocket-mesh-signal-server.git" tag $(version)

clean:
	rm -rf _build *.install

.PHONY: build release test
