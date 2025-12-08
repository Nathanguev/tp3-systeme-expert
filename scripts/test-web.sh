#!/bin/bash
# Test rapide de l'interface web

echo "Test de l'interface web..."
echo ""

cd /home/idaho/UTC/A25/IA01/tp3-systeme-expert

# Test du chargement
sbcl --non-interactive \
  --eval "(require :asdf)" \
  --eval "(load \"src/base-faits.lisp\")" \
  --eval "(load \"src/base-regles.lisp\")" \
  --eval "(load \"src/gestion-regles.lisp\")" \
  --eval "(load \"src/moteur.lisp\")" \
  --eval "(load \"donnees/recettes.lisp\")" \
  --eval "(handler-case
            (progn
              (load \"src/web-server.lisp\")
              (load \"src/web-api.lisp\")
              (format t \"Interface web: OK~%\")
              (quit))
            (error (c)
              (format t \"Interface web: ERREUR - ~A~%\" c)
              (quit)))" \
  2>&1 | grep -E "(Interface web|Error|error)" | head -5

echo ""
echo "Test terminé"
