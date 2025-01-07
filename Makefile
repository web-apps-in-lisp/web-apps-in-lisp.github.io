
run:
	./hugo serve

build:
	./hugo build --gc --minify && cp -r public/ docs/ && echo "done"
