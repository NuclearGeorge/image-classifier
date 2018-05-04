#!/bin/bash
# Nombre del trabajo
#PBS -N Jarvis
#
# Archivos de salida
# -o Jarvis.out
# -e Jarvis.err
#
# Cola de ejecucion (-q cola)
#
#PBS -q mpi
#PBS -l mem=128Gb,nodes=1:ppn=4
. /etc/profile.d/modules.sh
module load Mathematica
cd $PBS_O_WORKDIR
echo ==============================
echo SÚPER COMPUTO
echo Por: Dr. Jorge Chávez Carlos
echo ==============================
cat $PBS_NODEFILE > $HOME/nodos
math -noprompt -run < IC_JCHC.m
rm *.m
echo ==============================
