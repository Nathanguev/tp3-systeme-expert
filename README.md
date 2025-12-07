# Système Expert Culinaire - IA01 TP3

Système expert d'ordre 0+ pour la recommandation de recettes culinaires basé sur les ingrédients et le matériel disponibles.

## Description

Ce système expert aide les utilisateurs à identifier les recettes réalisables en fonction de :

- **Ingrédients disponibles** (avec quantités)
- **Matériel de cuisine** disponible
- **Filtres optionnels** : végétarien, saisons, type de plat (entrée, plat, dessert)

## Structure du projet

```txt
tp3-systeme-expert/
├── main.lisp              # Point d'entrée principal
├── src/
│   ├── base-faits.lisp    # Base de faits (ingrédients, matériel)
│   ├── base-regles.lisp   # Base de règles (recettes)
│   ├── moteur.lisp        # Moteurs d'inférence
│   └── interface.lisp     # Interface utilisateur
├── donnees/
│   └── recettes.lisp      # Données des recettes
├── tests/
│   └── scenario-1.lisp    # Scénarios de test
└── docs/                  # Documentation et rapports
```

## Fonctionnalités

- **Moteur d'inférence** : Chaînage avant/arrière pour déduire les recettes réalisables
- **Vérification stricte** : Contrôle des quantités d'ingrédients et du matériel requis
- **Filtrage intelligent** : Application de critères optionnels (saison, type, régime)
- **Traçabilité** : Explication des règles déclenchées et des exclusions

## Installation

Prérequis : Common Lisp (SBCL)

```bash
# Charger le système
sbcl --load main.lisp

# Ou
sbcl --script main.lisp
```

## Utilisation

```lisp
; Lancer le système expert
(load "main.lisp")

; Exemple d'utilisation (à adapter selon l'implémentation)
; Le système demande les ingrédients disponibles et propose les recettes
```

## Exemples de recettes

Le système inclut plusieurs recettes comme :

- Riz pilaf (végétarien, toutes saisons)
- Gratin dauphinois (végétarien, automne/hiver)
- Quiche lorraine (printemps/été/automne/hiver)
- Tarte tatin (végétarien, dessert, automne/hiver)

## Auteurs

Projet réalisé dans le cadre du cours IA01 - UTC semestre A25

## Licence

Projet académique - UTC

## Équipe

- Nathan GUEVARA
- Loïc ZHOU
