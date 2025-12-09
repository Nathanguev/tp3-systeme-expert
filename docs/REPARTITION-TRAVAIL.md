# Répartition du Travail - TP3 Système Expert

**Projet** : Système Expert Culinaire d'ordre 0+  
**Équipe** : Nathan GUEVARA & Loïc ZHOU  
**Date limite** : 7 janvier 2026 (code) + Soutenance 11 décembre 2025

---

## Objectifs obligatoires du projet

D'après le sujet, voici les éléments **obligatoires** à implémenter :

1. **Deux moteurs d'inférence** : chaînage avant + chaînage arrière
2. **Interaction utilisateur** : saisie des faits et visualisation
3. **Base de connaissances** : faits + règles (déjà formalisés dans TP2)
4. **Intégration des composants** : liaison moteur + interface + base
5. **Tests sur plusieurs scénarios** : minimum 3 scénarios différents
6. **Explicabilité** : trace du raisonnement et justification des conclusions

---

## Répartition proposée

### **Loïc ZHOU** - Infrastructure et Moteur

#### Module 1 : Base de faits (`src/base-faits.lisp`)

**Durée estimée** : 2-3h  
**Complexité** : ★★

**Tâches** :

- [ ] Implémenter `initialiser-base-faits()`
- [ ] Implémenter `ajouter-fait(cle, valeur)`
- [ ] Implémenter `obtenir-fait(cle)`
- [ ] Implémenter `modifier-fait(cle, nouvelle-valeur)`
- [ ] Implémenter `comparer-fait(cle, operateur, valeur)` → **IMPORTANT pour les règles**
- [ ] Implémenter `decremente-fait(cle, quantite)` → **IMPORTANT pour ordre 0+**
- [ ] Implémenter `afficher-base-faits()`
- [ ] Tester avec des faits simples

**Dépendances** : Aucune  
**Fichiers touchés** : `src/base-faits.lisp`

#### Module 2 : Gestion des règles (`src/gestion-regles.lisp`)

**Durée estimée** : 3-4h  
**Complexité** : ★★★

**Tâches** :

- [x] Implémenter `ajouter-regle(...)` → **DÉJÀ FAIT**
- [ ] Implémenter `evaluer-condition(condition)` → utilise `comparer-fait`
- [ ] Implémenter `evaluer-conditions(conditions)` → conjonction ET
- [ ] Implémenter `regles-candidates()` → pour chaînage avant
- [ ] Implémenter `regles-pour-but(but)` → pour chaînage arrière
- [ ] Implémenter `appliquer-regle(regle)` → exécute actions + conclusion
- [ ] Implémenter `regle-respecte-filtres-p(regle)`
- [ ] Tester l'évaluation de conditions simples

**Dépendances** : `base-faits.lisp` (doit être terminé)  
**Fichiers touchés** : `src/gestion-regles.lisp`

#### Module 3 : Moteur d'inférence - Chaînage avant (`src/moteur.lisp`)

**Durée estimée** : 3-4h  
**Complexité** : ★★★★

**Tâches** :

- [ ] Implémenter `chainage-avant()` → stratégie en largeur
  - Récupérer règles candidates
  - Trier par profondeur (compositions d'abord)
  - Appliquer règles applicables
  - Enregistrer dans `*regles-declenchees*`
  - Répéter jusqu'à saturation
- [ ] Implémenter `obtenir-trace-inference()`
- [ ] Implémenter `afficher-trace-inference()`
- [ ] Implémenter `reinitialiser-moteur()`
- [ ] Tester avec un scénario simple

**Dépendances** : `gestion-regles.lisp` (doit être terminé)  
**Fichiers touchés** : `src/moteur.lisp`

---

### **Nathan GUEVARA** - Données et Interface

#### Module 4 : Données des recettes (`donnees/recettes.lisp`)

**Durée estimée** : 2-3h
**Complexité** : ★★

**Tâches** :

- [x] Implémenter `charger-recettes()`
- [x] Implémenter `charger-compositions-intermediaires()`
  - Parser `*donnees-compositions*` (7 compositions)
  - Créer les règles avec `ajouter-regle`
  - Format : nom, conditions (ingrédients + matériel), conclusion, profondeur=1
- [x] Implémenter `charger-recettes-finales()`
  - Parser `*donnees-recettes*` (14 recettes)
  - Créer les règles avec conditions + filtres
  - Format : incluant saisons, type, végétarien, profondeur=0
- [x] Implémenter `lister-tous-ingredients()`
- [x] Implémenter `lister-tout-materiel()`
- [x] Tester le chargement (vérifier 21 règles créées)

**Dépendances** : `gestion-regles.lisp` (pour `ajouter-regle`)
**Fichiers touchés** : `donnees/recettes.lisp`, `src/base-regles.lisp` (données brutes)

#### Module 5 : Interface utilisateur (`src/interface.lisp`)

**Durée estimée** : 3-4h
**Complexité** : ★★★

**Tâches** :

- [x] `interface-principale()` → **DÉJÀ FAIT**
- [x] Implémenter `saisir-ingredients()`
  - Utiliser `lister-tous-ingredients()` pour proposer liste
  - Demander quantité pour chaque ingrédient
  - Appeler `ajouter-fait(ingredient, quantite)`
- [x] Implémenter `saisir-materiel()`
  - Utiliser `lister-tout-materiel()` pour proposer liste
  - Sélection multiple
  - Appeler `ajouter-fait(materiel, t)`
- [x] Implémenter `configurer-filtres()`
  - Demander végétarien (oui/non)
  - Demander saisons (sélection multiple)
  - Demander types (sélection multiple)
  - Ajouter faits filtres
- [x] Implémenter `lancer-recherche-recettes()`
  - Appeler `chainage-avant()`
  - Afficher recettes réalisables
  - Proposer d'afficher la trace
- [x] Implémenter `afficher-recettes-realisables(recettes)`
  - Grouper par type (entrée/plat/dessert)
  - Afficher propriétés
- [x] Implémenter utilitaires : `lire-entier()`, `lire-oui-non()`, `lire-choix-multiple()`

**Dépendances** : `moteur.lisp` (pour chaînage-avant), `donnees/recettes.lisp` (pour listes)  
**Fichiers touchés** : `src/interface.lisp`

#### Module 6 : Moteur d'inférence - Chaînage arrière (`src/moteur.lisp`)

**Durée estimée** : 3-4h  
**Complexité** : ★★★★

**Tâches** :

- [x] Implémenter `chainage-arriere(but)` → stratégie goal-driven
  - Vérifier si but déjà dans base de faits
  - Chercher règles concluant ce but
  - Prouver récursivement les conditions
  - Gérer profondeur maximale
- [x] Implémenter `prouver-but-recursif(but, profondeur)`
- [ ] Implémenter `expliquer-conclusion(fait)` → arbre de déduction
- [ ] Implémenter `verifier-recette-specifique()` dans interface
  - Proposer liste de recettes
  - Lancer chaînage-arriere
  - Expliquer succès ou échec
- [ ] Tester avec une recette spécifique

**Dépendances** : `gestion-regles.lisp`, chaînage-avant terminé  
**Fichiers touchés** : `src/moteur.lisp`, `src/interface.lisp`

---

### **Commun** - Tests et Intégration

#### Module 7 : Scénarios de test (`tests/scenario-1.lisp`)

**Durée estimée** : 2h  
**Complexité** : ★★
**Responsables** : Nathan + Loïc ensemble

**Tâches** :

- [ ] Implémenter `executer-scenario-1()` → Quiche lorraine
  - Ingrédients : beurre, oeuf, creme, lard, gruyere, farine, eau
  - Matériel : bol, rouleau, moule_tarte, fouet
  - Vérifier que pâte_brisée puis quiche_lorraine sont déduites
- [ ] Implémenter `executer-scenario-2()` → Utilisateur végétarien
  - Ingrédients limités
  - Filtre végétarien actif
  - Vérifier que seules recettes végétariennes sont proposées
- [ ] Implémenter `executer-scenario-3()` → Chaînage arrière
  - But : tarte_tatin
  - Vérifier preuve récursive (pâte_brisée nécessaire)
- [ ] Documenter les résultats attendus
- [ ] Valider avec exécution réelle

**Dépendances** : TOUT (dernière étape)  
**Fichiers touchés** : `tests/scenario-1.lisp`

---

## Dépendances et ordre de développement

```txt
┌─────────────────┐
│  base-faits     │ ← Loïc démarre ici (aucune dépendance)
│  (Module 1)     │
└────────┬────────┘
         │
         ▼
┌─────────────────┐        ┌──────────────────┐
│ gestion-regles  │        │ donnees/recettes │ ← Nathan démarre ici (parallèle)
│  (Module 2)     │◄───────┤   (Module 4)     │
└────────┬────────┘        └──────────────────┘
         │                           │
         ▼                           ▼
┌─────────────────┐        ┌──────────────────┐
│ chainage-avant  │        │   interface      │
│  (Module 3)     │◄───────┤   (Module 5)     │
└────────┬────────┘        └──────────────────┘
         │                           │
         └──────────┬────────────────┘
                    ▼ 
         ┌─────────────────────┐
         │  chainage-arriere   │ ← Nathan termine ici
         │    (Module 6)       │
         └──────────┬──────────┘
                    ▼
         ┌─────────────────────┐
         │   Tests (Module 7)  │ ← Nathan + Loïc ensemble
         │     INTÉGRATION     │
         └─────────────────────┘
```

---

## Checklist de livraison

### Code (7 janvier 23h30)

- [ ] Tous les modules implémentés et fonctionnels
- [ ] 3 scénarios de test exécutables et documentés
- [ ] Code commenté en français
- [ ] Pas d'erreurs au chargement (`sbcl --load main.lisp`)
- [ ] README.md à jour avec instructions d'utilisation

### Rapport PDF (8-15 pages)

- [ ] Partie 1 : Rappel TP2 (Nathan)
- [ ] Partie 2 : Moteurs d'inférence (Nathan)
- [ ] Partie 3 : Interface (Loïc)
- [ ] Partie 4 : Extensions (Loïc - si applicable)
- [ ] Partie 5 : Usage IA documenté (Nathan)
- [ ] Partie 6 : Discussion (Loïc)
- [ ] Captures d'écran des exécutions
- [ ] Références et bibliographie

### ZIP final

- [ ] `code/` : tous les fichiers .lisp
- [ ] `docs/` : rapport PDF + documentation
- [ ] `README.md` : instructions d'exécution
- [ ] `.gitignore` : fichiers exclus

---

## Usage de l'IA (à documenter)

**Important** : Pour chaque usage de GitHub Copilot ou autre IA :

- Noter le **prompt utilisé**
- Noter ce qui a été **généré automatiquement**
- Noter ce qui a été **modifié/corrigé manuellement**
- Expliquer **pourquoi** la correction était nécessaire

→ Ces notes serviront pour la **Partie 5 du rapport** (2 points sur 18)

---

**Version** : 1.0
