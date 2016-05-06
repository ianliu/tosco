## A quem se destina ##

Este guia é destinado a interessados em ter a versão mais nova do Rays2 instalada em seu sistema Ubuntu e não querem esperar pelo lançamento de um pacote binário.

## Preparação ##

O programa Rays2 depende de algumas bibliotecas que devem ser instaladas
no sistema. Além disto, algumas ferramentas de compilação também são necessárias. Só assim a versão mais nova poderá ser compilada.

Para instalar essas dependências execute em um terminal os dois comandos abaixo:
```
sudo apt-get install mercurial gcc automake autoconf
sudo apt-get install libglib2.0 libglib2.0-dev
```

## Obtendo a versão mais recente ##

O programa Rays2 é distribuído através do projeto ToSCo. Para baixar a versão mais recente do ToSCo, execute em um terminal o comando a seguir:
```
hg clone https://code.google.com/p/tosco/
```

Caso já tenha feito isto antes e precise apenas se certificar que a versão local que você tem realmente correspode à versão mais recente no repositório, execute o comando a seguir, dentro da pasta `tosco`:
```
hg pull -u
```

## Compilando ##

Para compilar o Rays2, em um terminal, dirija-se até o diretório onde baixou o ToSCo e rode os comandos:
```
cd rays2
aclocal
automake --add-missing
autoconf
./configure
```
Se todos os comando acima executarem sem problemas, você estará pronto para a compilação. Basta rodar
```
make
```
Na pasta `src` deve ter sido criado o arquivo executável `rays2`.

## Instalação ##

A instalação do programa Rays2 é bem simples. Siga os passos:
```
if [ ! -d $HOME/bin ]; then mkdir $HOME/bin; fi
cp src/rays2 $HOME/bin
cp gebr/rays2.mnu $HOME/GeBR-Menus
```

Para conferir se a versão recém instalada ficou de fato disponível, rode `which rays2` e observe se o caminho informado é `$HOME/bin` (onde `$HOME` será substituído pelo diretório raiz de sua área -- algo como `/home/seu-usuario/`). Se nenhum caminho for exibido ou se for exibido o caminho para a versão instalada globalmente no sistema (em `/usr/bin/rays2`) será necessário encerrar sua seção e logar novamente para que as alterações tenham efeito.

