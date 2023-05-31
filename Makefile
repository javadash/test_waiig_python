default: mypy pylint test repl

repl:
	python main.py

test:
	python -m unittest *test.py

mypy:
	# Or use --strict, a superset of --disallow-untyped-defs
	# Use tools such as MonkeyType or pyannotate to generate initial hints
	mypy --disallow-untyped-defs *.py

pylint:
	pylint *.py

clean:
	rm -fr __pycache__
	rm -fr .mypy_cache
	rm -fr venv