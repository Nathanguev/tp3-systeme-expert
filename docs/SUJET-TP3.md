# IA01 - Sujet de TP3 - Système Expert - A25

## Implémentation d'un Système Expert d'ordre 0+

  >
    Soutenance et démonstration (6 min tout compris) :

    - Groupe 1 : 16 décembre 2025
    - Groupes 2 et 3 : 11 décembre 2025
    - Codes et Rapport : 7 janvier 2026 à 23.30 sur Moodle (un rendu par binôme)

    L'utilisation d'IA générative (ChatGPT, Copilot, Gemini, Mistral, etc.) est autorisée. Dans ce cas, vous devrez toutefois documenter précisément son usage : prompts utilisés, types d'aide apportée, outils ou extensions employés, etc.

---

### Objectif

À partir de votre de la base de règles de votre **projet de TP2**, vous devez désormais **implémenter le système expert d'ordre 0+**

Le TP3 consiste à :

- Programmer deux moteurs d'inférence (combinaisons possibles : chaînage avant ou arrière ou mixte, profondeur d'abord ou largeur d'abord) ;
- Prévoir une interaction avec l'utilisateur ;
- Intégrer l'ensemble des composants dans votre SE : moteurs, interface et base de connaissances (faits et règles) ;
- Tester le système sur plusieurs scénarios d'entrée ;
- (Optionnel) Etendre votre SE pour facilter son usage / exploitation (base de données externes pour alimenter le SE, visualisation des résultats, …).

---

## Partie 1 - Rappel et préparation

Résumez brièvement le travail du **TP2** :

- Domaine d'expertise choisi
- Objectif du système expert
- Logique générale des règles (base de faits, moteur d'inférence)

---

## Partie 2 - Implémentation du moteur d'inférence

1. **Implémentez et décrivez les moteurs d'inférence** correspondants à votre modèle d’ordre 0+ :
   - Chaînage **avant** (à partir des faits pour déduire de nouveaux faits)
   - OU chaînage **arrière** (à partir d'un but à vérifier)
   - OU une version **mixte** combinant les deux

2. Décrivez les structures de données utilisées :
   - Représentation des faits
   - Représentation des règles

3. Décrivez les fonctions de service proposées associées aux structures de données :
   - Vérification d'un fait
   - Termes conditions, conclusion
   - Règles candidates
   - Exécution des règles
   - etc.

4. Testez le fonctionnement du moteur avec des **exemples concrets issus de votre TP2**.

---

## Partie 3 - Interaction et interface

1. Créez une interaction utilisateur permettant d’entrer ou de visualiser des faits :

2. Affichez les résultats et éventuellement le raisonnement suivi (explicabilité) :
   - Faits initiaux, règles déclenchées, faits déduits, conclusion.
   - Possibilité de visualiser l’arbre de raisonnement ou la séquence d’inférences.

---

## Partie 4 - Extensions possibles (facultatif)

Vous pouvez enrichir votre projet en intégrant une ou plusieurs extensions :

- **Couplage IA** : utilisation d’un modèle génératif pour suggérer de nouvelles règles, améliorer l’interface, expliquer les résultats, etc.
- **Connexion à une base de données externe** : récupération de faits depuis un fichier CSV, une API, une base SQLite, etc.
- **Interface visuelle avancée** : visualisation dynamique du graphe de raisonnement, génération d’arbres explicatifs.
- **Apprentissage adaptatif** : ajout ou modification automatique de règles à partir d’exemples utilisateurs.
- **Application web** : implémentation du SE sous forme de mini-site interactif.

---

## Partie 5 - Usage de l’IA générative

L’usage de l’IA est **autorisé et évalué positivement** à condition d’être **documenté et critique.**

Vous devez inclure dans votre rapport :

- Les **prompts exacts** utilisés pour générer ou améliorer du code, des interfaces ou des textes explicatifs ;
- Les **captures ou extraits de conversations** pertinentes (si possible, dans une annexe) ;
- Une **analyse critique** : ce que l’IA a correctement apporté, ce que vous avez corrigé, compris ou modifié.
- La **proportion estimée du travail généré** par vous et par l’IA.

*L’IA doit être considérée comme un **assistant technique et réflexif**, pas comme un substitut de compréhension.*

---

## Partie 6 - Discussion et validation

- Analysez les performances et limites de votre implémentation.
- Discutez la robustesse du moteur d’inférence et les cas non prévus.
- Proposez des pistes d’évolution :
  - Passage à un moteur d’ordre 1 ;
  - Intégration à une application plus large.

---

## Format du rendu

Votre rendu doit comporter :

1. Un rapport PDF (8 à 15 pages) contenant :
   - Résumé du TP2 et objectifs d’implémentation ;
   - Description du moteur d’inférence et des choix techniques ;
   - Tests et résultats sur plusieurs scénarios ;
   - Usage de l’IA documenté ;
   - Discussion et conclusion.

2. Un dossier compressé (.zip) contenant :
   - Le code source complet (commenté et exécutable) ;
   - Les fichiers de données éventuels (base de faits, règles, CSV, etc.) ;
   - Les fichiers d’interface ou de visualisation (facultatif).

---

### Conseils

- Vous pouvez vous inspirer de la structure du TP2 pour guider votre rédaction.
- Le code n’a pas besoin d’être long : il doit être clair, cohérent et fonctionnel.
- Les exemples doivent démontrer la logique du raisonnement (pas seulement l’exécution).
- L’usage réfléchi de l’IA est valorisé, pas la simple génération automatique.

---

### Grille d’évaluation (indicative)

| Critère | Description | Points |
| :-- | :-- | :-- |
| Fonctionnalité du système expert | Moteurs d’inférence fonctionnel (avant/arrière) | 6 pts |
| Qualité de l’implémentation | Structure, lisibilité, modularité du code | 4 pts |
| Pertinence et richesse des règles | Cohérence avec le TP2, complexité logique | 3 pts |
| Interaction et présentation des résultats | Interface, visualisation, scénarios de test | 3 pts |
| Usage documenté de l’IA | Clarté, transparence, analyse critique | 2 pts |
| Total |   | 18 pts |

---

  > Ce TP3 évalue la mise en œuvre concrète de votre raisonnement symbolique et votre
    capacité à combiner logique, programmation et usage réfléchi de l’IA.
