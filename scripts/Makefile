DOCS = ../docs
TAG = effect-racket-artifact

docker: artifact.scm
	guix pack -r base.tar.gz -f docker -S /bin=bin -m $<
	docker load < base.tar.gz
	docker build .. -t $(TAG) -f Dockerfile
	docker save $(TAG) | gzip > $(TAG).tar.gz
	docker rmi $(TAG)
	rm base.tar.gz

docs: $(DOCS)/*.scrbl ../effect-racket-doc/scribblings/*.scrbl
	raco scribble ++xref-in setup/xref load-collections-xref \
	              --redirect-main http://docs.racket-lang.org/ \
	              --dest $(DOCS) \
	              --html $^
	cp $(DOCS)/artifact.html $(DOCS)/index.html

.PHONY: docker docs
