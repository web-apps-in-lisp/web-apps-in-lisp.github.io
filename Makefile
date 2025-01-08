
run:
	./hugo serve

build:
	./hugo build --gc --minify && cp -r public/* docs && echo "copied to docs/. Done."

publish:
	git commit -m "publish" && git push
