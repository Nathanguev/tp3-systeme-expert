#!/bin/bash

echo "Test des dépendances..."

# Test packages APT
if ! dpkg -l | grep -q "^ii  cl-hunchentoot"; then
    echo "Erreur: cl-hunchentoot manquant"
    exit 1
fi

if ! dpkg -l | grep -q "^ii  cl-yason"; then
    echo "Erreur: cl-yason manquant"
    exit 1
fi

echo "Packages: OK"

# Test chargement SBCL
if sbcl --non-interactive --eval "(require :hunchentoot)" 2>&1 | grep -q "error"; then
    echo "Erreur: Hunchentoot ne charge pas"
    exit 1
fi

if sbcl --non-interactive --eval "(require :yason)" 2>&1 | grep -q "error"; then
    echo "Erreur: Yason ne charge pas"
    exit 1
fi

echo "Chargement: OK"

# Test JSON
if sbcl --non-interactive --eval "(require :yason)" --eval "(with-output-to-string (s) (yason:encode-alist '((\"test\" . t)) s))" --quit >/dev/null 2>&1; then
    echo "JSON: OK"
else
    echo "Erreur: JSON ne fonctionne pas"
    exit 1
fi

echo "Tous les tests: OK"
