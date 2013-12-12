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
	${RSCRIPT} algselbench/renderKnitRPages.R
	# sed $(SED_OPTION) s/\`\`\`r/\`\`\`splus/ doc/knitted/*.md
	# sed $(SED_OPTION) s/\`\`\`r/\`\`\`splus/ doc/knitted/tutorial/*.md
	# mv doc/figure doc/knitted/tutorial/figur

	# printf "\nGenerating html docs...\n"
	# mkdir staticdocs
	# ${DELETE} /tmp/pkgdocs
	# mkdir /tmp/pkgdocs
	# mv README.md README.xxx
	# ${RSCRIPT} ./makeR/generate-html-docs
	# mv README.xxx README.md
	# ${DELETE} Rplots*.pdf
	# git checkout gh-pages
	# ${DELETE} man
	# mv /tmp/pkgdocs man
	# git add man
	# git commit -am "new html help"
	# git push origin gh-pages
	# git checkout master


