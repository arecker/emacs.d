VENV_PYTHON := $(shell which python)

.PHONY: all
all: prod

venv/bin/python:
	@echo "==> building $@"
	$(VENV_PYTHON) -m venv venv

.PHONY: prod dev
prod dev: venv/bin/python
	@echo "==> switching to $@ configs"
	./scripts/bootstrap.py --mode=$@ --verbose

.PHONY: clean
clean:
	@echo "==> cleaning project"
	rm -rf ./venv
