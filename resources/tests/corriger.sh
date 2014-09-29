#!/bin/bash
# UQAM - Département d'informatique
# INF4230 - Intelligence artificielle
# TP1 - A*
# Script d'évaluation du TP1
#
# Instructions:
# 1. Déposer ce script avec les fichiers problèmes dans un répertoire distinct (ex: tests).
# 2. Se placer dans le répertoire contenant votre programme ou contenant la liste des soumissions Oto (*.tp_oto).
# 3. Lancer ce script (ex: ../tests/evaluer.sh).

# Répertoire contenant les fichier tests de vérification et de performance
repertoire_tests="`dirname $0`"
date=`date +%Y%m%d_%H%M%S`

pushd ${repertoire_tests}
tests="`ls sokoban*.txt`"
if [ valideur-sokoban.cpp -nt valideur-sokoban ]; then
   echo "Recompiler valideur-sokoban.cpp ..."
   g++ valideur-sokoban.cpp -o valideur-sokoban
fi
popd

## La fonction qui évalue un TP.
function EvaluerTP 
{
    unzip tp1.zip
    rm -f *.class tp1[ab] *.jar

    logfile="log-${date}.txt"
    
    # Extraction des codes permanents dans les fichiers sources
    echo "Rapport de correction automatique" > ${logfile}
    echo "#################################" >> ${logfile}
    echo "Date : ${date}" >> ${logfile}
    echo "Code(s) MS : " >> ${logfile}
    ( (pwd | grep -oh [a-zA-Z]{4}[0-9]{8}) && grep -oh  [a-zA-Z]{4}[0-9]{8} *.{h*,c*,java} ) | sort -u >> ${logfile}
    echo -e "\n\n" >> ${logfile}

    echo -e "\nCompilation ..."
    echo -e "\nCompilation ..." >> ${logfile}   
    
    ## Detection du langage de programmation
    ### JAVA
    if [ -e TP1a.java ]; then
        javac TP1[ab].java 2>&1 >> ${logfile}
        
        if [ ! -e TP1b.class ]; then
            echo "ERREUR : la classe TP1b.class n'a pas été produite!"
            echo "  ERREUR : la classe TP1b.class n'a pas été produite!" >> ${logfile}
            return
        fi
        (
            #ulimit -t 30 -f 1024
            for test in $tests;
            do
            	echo "Test : $test"
                time java -Xmx12g TP1b ${repertoire_tests}/${test} > ${test}.resultat
            done
        )        
    fi
    
    ### Makefile (C/C++)
    if [ -e Makefile ]; then
        make clean
        echo -ne
        make 2>&1 >> ${logfile}
        echo -ne
        
        if [ ! -x tp1b ]; then
            echo "ERREUR : l'exécutable tp1a n'a pas été produit!"
            echo "  ERREUR : l'exécutable tp1a n'a pas été produit!" >> ${logfile}
            return
        fi
        (
            ulimit -t 30 -v 2097152 -f 1024
            for test in $tests;
            do
            	echo "Test : $test"
                time ./tp1b ${repertoire_tests}/${test} > ${test}.resultat
            done            
        )
    fi
    
    
    ## Vérification des résultats ##
    if [ -x ${repertoire_tests}/valideur-sokoban ]; then
        ## Valideur Sokoban disponile
        for test in $tests;
        do
            v=`${repertoire_tests}/valideur-sokoban ${repertoire_tests}/${test} ${test}.resultat ${repertoire_tests}/${test}.solution`
            echo "${test} $v"
            echo "${test} $v" >> ${logfile}
            sommaire="${sommaire}\t$v"
        done
    else
        ## Valideur Sokoban non disponile : utilisation de diff
        for test in $tests;
        do
            diff -tibw ${test}.resultat ${repertoire_tests}/${test}.solution 2>&1 > /dev/null
        	if [ $? -eq 0 ];
        	then
        	    v="Optimal"
         	else
         	    v="NonOptimal"
        	fi
        	echo "${test} ===> $v"
        	echo "${test} ===> $v" >> ${logfile}
        	sommaire="${sommaire}\t$v"
        done    
    fi
}

# Lister les soumissions Oto (répertoires terminant par .tp_oto)
tps=`ls -1 | grep .tp_oto`

if [ -n "$tps" ]; then
    #Mode correction en batch
    echo "#Rapport de correction" > "rapport-${date}.txt"
    echo "#Date: ${date}" >> "rapport-${date}.txt"
    echo -e -n "#Soumission\t" >> "rapport-${date}.txt"
    for test in $tests; do
       echo -e -n $test >> "rapport-${date}.txt"
    done
    echo >> "rapport-${date}.txt"
    
    for tp in $tps; do
       sommaire="$tp "
       echo "## CORRECTION : $tp"
       pushd $tp
       EvaluerTP
       popd
       echo ">> $sommaire"
       echo "$sommaire" >> "rapport-${date}.txt"
    done
else
    #Mode correction dans le répertoire courant
    EvaluerTP
fi

