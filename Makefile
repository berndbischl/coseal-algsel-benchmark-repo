R	:= R --no-save --no-restore
RSCRIPT	:= Rscript
DELETE	:= rm -fR

.SILENT:

usage:
	echo "Available targets:"
	echo " clean                   - Clean up"
	echo " render-scenario-pages   - Render HTML pages for scenario"
	echo " push-scenario-pages     - Render pages and push to gh-pages"

clean:
	echo  "Cleaning up."
	${DELETE} html
	mkdir html

render-scenario-pages: clean
	echo "Creating scenario html pages"
	cd server_scripts; ${RSCRIPT} renderKnitRPages.R; cd .. 

push-scenario-pages: render-scenario-pages
	rm -rf /tmp/html
	cp -r html /tmp/
	git checkout gh-pages
	git pull
	${DELETE} scenario-pages
	cp -r /tmp/html scenario-pages 
	git add scenario-pages 
	git commit -am "new scenario pages"
	git push origin gh-pages
	git checkout master


