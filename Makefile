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
	cd server_scripts; ${RSCRIPT} renderKnitRPages.R; cd .. 

push-task-pages: render-task-pages
	rm -rf /tmp/html
	cp -r html /tmp/
	git checkout gh-pages
	git pull
	${DELETE} task-pages
	cp -r /tmp/html task-pages 
	git add task-pages 
	git commit -am "new task pages"
	git push origin gh-pages
	git checkout master


