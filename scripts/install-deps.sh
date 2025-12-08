#!/bin/bash

echo "Installation des dépendances..."

# Vérifier Ubuntu/Debian
if [ ! -f /etc/debian_version ]; then
    echo "Erreur: Ce script nécessite Ubuntu/Debian"
    exit 1
fi

# Vérifier si les paquets sont installés
PACKAGES="cl-hunchentoot cl-yason"
NEED_INSTALL=false

for pkg in $PACKAGES; do
    if ! dpkg -l | grep -q "^ii  $pkg"; then
        NEED_INSTALL=true
        break
    fi
done

# Installer si nécessaire
if [ "$NEED_INSTALL" = true ]; then
    echo "Installation: cl-hunchentoot, cl-yason"
    sudo apt update -qq
    
    if sudo apt install -y $PACKAGES > /dev/null 2>&1; then
        echo "Installation réussie"
    else
        echo "Erreur lors de l'installation"
        exit 1
    fi
else
    echo "Paquets déjà installés"
fi

echo "Terminé. Lancez: sbcl --load main.lisp"
