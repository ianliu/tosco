## A quem se destina ##

Este guia é destinado a interessados em ter a versão mais nova do S88Modeling instalada em seu sistema Ubuntu e não querem esperar pelo lançamento de um pacote binário.

A partir da versão 1.0.0, S88Modeling incorporou as funcionalidades do programa Rays2, também parte do projeto ToSCo. Sendo assim, a necessidade da instalação do programa Rays2 fica muito reduzida.

## Preparação ##

O programa S88Modeling depende de algumas bibliotecas que devem ser instaladas no sistema. Além disto, algumas ferramentas de compilação também são necessárias. Só assim a versão mais nova poderá ser compilada.

Para instalar essas dependências execute em um terminal os dois comandos abaixo:
```
sudo apt-get install mercurial gcc automake autoconf
sudo apt-get install libglib2.0 libglib2.0-dev tosco
```

## Obtendo a versão mais recente ##

O programa S88Modeling é distribuído através do projeto ToSCo. Para baixar a versão mais recente do ToSCo, execute em um terminal o comando a seguir:
```
hg clone https://code.google.com/p/tosco/
```

Caso já tenha feito isto antes e precise apenas se certificar que a versão local que você tem realmente correspode à versão mais recente no repositório, execute o comando a seguir, dentro da pasta `tosco`:
```
hg pull -u
```

## Compilando ##

Para compilar o S88Modeling, em um terminal, dirija-se até o diretório onde baixou o ToSCo e rode os comandos:
```
cd s88modeling
aclocal
autoheader
automake --add-missing
autoconf
./configure
```
Se todos os comando acima executarem sem problemas, você estará pronto para a compilação. Basta rodar
```
make
```
Na pasta `src` deve ter sido criado o arquivo executável `s88modeling`.

## Instalação ##

A instalação do programa S88Modeling é bem simples. Siga os passos:
```
if [ ! -d $HOME/bin ]; then mkdir $HOME/bin; fi
cp src/s88modeling $HOME/bin
cp gebr/s88modeling.mnu $HOME/GeBR-Menus
```

Para conferir se a versão recém instalada ficou de fato disponível, rode `which s88modeling` e observe se o caminho informado é `$HOME/bin` (onde `$HOME` será substituído pelo diretório raiz de sua área -- algo como `/home/seu-usuario/`). Se nenhum caminho for exibido ou se for exibido o caminho para a versão instalada globalmente no sistema (em `/usr/bin/s88modeling`) será necessário encerrar sua seção e logar novamente para que as alterações tenham efeito.

Outra verificação que pode ser feita é rodando `s88modeling --version` (essa opção só é reconhecida a partir da versão 1.0.0).

