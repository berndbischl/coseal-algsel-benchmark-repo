R	:= R --no-save --no-restore
RSCRIPT	:= Rscript
DELETE	:= rm -fR

.SILENT:

usage:
	echo "Available targets:"
	echo " clean               - Clean up"
	echo " render-task-pages   - Render HTML pages for task"
	echo " push-task-pages     - Render pages and push to gh-pages"

clean:
	echo  "Cleaning up."
	${DELETE} html
	mkdir html

render-task-pages: clean
	echo "Creating task html pages"
	${RSCRIPT} renderKnitRPages.R

push-task-pages: render-task-pages
	git checkout gh-pages
	# ${DELETE} man
	# mv /tmp/pkgdocs man
	# git add man
	# git commit -am "new html help"
	# git push origin gh-pages
	# git checkout master


