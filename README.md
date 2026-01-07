# Système Expert Culinaire - IA01 TP3

Système expert d'ordre 0+ pour la recommandation de recettes culinaires basé sur les ingrédients et le matériel disponibles.

## Architecture

```txt
tp3-systeme-expert/
├── main.lisp                  # Point d'entrée principal
├── src/
│   ├── base-faits.lisp        # Gestion de la base de faits
│   ├── base-regles.lisp       # Données brutes des recettes
│   ├── gestion-regles.lisp    # Gestion de la base de règles
│   ├── moteur.lisp            # Moteurs d'inférence
│   └── interface.lisp         # Interface utilisateur
├── donnees/
│   └── recettes.lisp          # Parsing et chargement des recettes
├── tests/
│   └── scenario-1.lisp        # Scénarios de test
└── docs/                      # Documentation et rapports
```

### Composants principaux

- **Base de faits** : Stocke les ingrédients disponibles, matériel, filtres actifs
- **Base de règles** : Contient les règles de composition des recettes (ordre 0+)
- **Moteur d'inférence** : Chaînage avant / arrière pour déduire les recettes possibles
- **Interface** : Interaction en ligne de commande avec l'utilisateur

## Utilisation

```bash
# Lancer le système avec SBCL
sbcl --script main.lisp
```

Le système guide l'utilisateur à travers :

1. Saisie des ingrédients
2. Choix des filtres (saison, type, végétarien)
3. Affichage des recettes réalisables
4. Explication du raisonnement

## Équipe

Projet réalisé dans le cadre du cours IA01 - UTC A25

- Nathan GUEVARA
- Loïc ZHOU
