#!/bin/bash
# Lancement du système avec interface web

cd /home/idaho/UTC/A25/IA01/tp3-systeme-expert

echo "========================================="
echo "  Démarrage du système expert culinaire"
echo "========================================="
echo ""
echo "Lancement en cours..."
echo ""

sbcl --load main.lisp
