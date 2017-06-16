RACKET=racket
PYTHON=python2.7
ID=src

.PHONY: teste-unitario teste-funcional teste zip limpar

teste: teste-unitario teste-funcional

teste-unitario:
	@echo Executando testes unitários
	$(RACKET) src/infinity-testes.rkt

teste-funcional:
	@echo Executando testes funcionais
	@$(PYTHON) testador.py testes racket src/infinity-main.rkt

zip: src.zip

src.zip: $(shell find src/ -type f ! -wholename 'src/compiled*' ! -name '*~')
	@echo Criando arquivo $(ID).zip.
	@zip --quiet $(ID).zip -r $?
	@echo Arquivo $(ID).zip criado.

limpar:
	@echo Removendo src/compiled
	@rm -rf src/compiled/
