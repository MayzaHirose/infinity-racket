#!/usr/bin/env python2.7

def ler_casos(dir_testes):
    import os, os.path
    casos = []
    dir_casos = os.path.join(dir_testes, 'casos')
    dir_esperados = os.path.join(dir_testes, 'esperados')
    arqs_casos = map(lambda x: os.path.join(dir_casos, x),
                    filter(lambda x: x.endswith('.txt'),
                        os.listdir(dir_casos)))
    for arq_caso in arqs_casos:
        nome_caso = os.path.splitext(os.path.basename(arq_caso))[0]
        arq_esperado = os.path.join(dir_esperados, nome_caso + '_resolvido.txt')

        with open(arq_esperado) as esperado:
            casos.append(([arq_caso], esperado.read()))

    return casos

def main(dir_testes, prog):
    testar(prog, ler_casos(dir_testes))

def testar(prog, casos):
    total = 0
    falhas = 0
    for caso in casos:
        params, esperado = caso
        nome = " ".join(prog + params)
        print nome,
        ok, obtido = executar_programa(prog, params)
        total += 1
        if not ok or obtido != esperado:
            falhas += 1
            print "Falhou!"
        else:
            print "OK!"
    print "Passou em", (total - falhas), "teste(s) do total de", total

def executar_programa(prog, params):
    import subprocess
    try:
        return True, subprocess.check_output(prog + params)
    except:
        r = "Erro ao executar o programa: %s\nExecute o programa no terminal para ver o erro.\n" % " ".join(prog + params)
        return False, r

if __name__ == '__main__':
    import sys
    if len(sys.argv) < 3:
        print "Modo de usar: $python %s dir_testes nome-do-programa [parametros]" % sys.argv[0]
        sys.exit(1)
    main(sys.argv[1], sys.argv[2:])
