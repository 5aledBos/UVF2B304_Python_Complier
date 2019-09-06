# UVF2B304_Python_Complier

Ce projet consiste à construire un interpréteur python en utilisant Ocaml

## Membre du groupe :

- Bousrih khaled
- Sahri Hamza

## Instruction de build :


Un fichier makefile est mis en place pour le build du fichier main.
Executez la commande "make" pour créer le fichier main.native


La liste des tests :
- stmts_test.py
- objects.py
- objects2.py
- fibonacci.py
- len&range.py
- arith&conditional.py


Pour lancer un test, executez la commande "./main.native tests/nom_du_test.py"

## Description de l'implémentation 



les fichiers python pris en considération pour ce projet se compose d'une liste optionelle de déclaration de classe, et une liste d'instructions

liste d'instructions implémentées:

- instruction for
- instruction If
- Definition de Class/Fonction
- Appel de Classe/Fonction
- Instruction print & return
- len : longueur d'une liste ou un string
- range(n) : renvoie la liste [0, 1, 2, ..., n-1]

les classes doivent impérativement avoir un constructeur en utilisant la fonction "__init__". 
Les variables de classes ne sont pas implémentées (variables statiques)
