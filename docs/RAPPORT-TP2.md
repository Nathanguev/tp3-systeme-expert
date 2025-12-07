# IA01 - TP2

  > Système expert culinaire

---

## Introduction et rappel des notions clés

### Définition et composants

Un système expert (SE) est un programme qui imite le raisonnement d'un expert humain dans un domaine précis pour aider à décider, diagnostiquer ou recommander. Il utilise des faits (ce qui est vrai dans le cas présent) et des règles (connaissances) pour déduire des conclusions. Il sert aussi à conserver l'expertise.

Les trois composants principaux :

1. **Base de connaissances**
   - Base de faits : informations du cas courant (souvent fournies par l'utilisateur).
   - Base de règles : règles générales du domaine (SI conditions ALORS conclusion).
   - Métarègles : gestion des priorités, conflits, stratégie de parcours.
2. **Moteur d'inférence** : applique les règles aux faits (chaînage avant / arrière) et ajoute ou modifie des faits jusqu'à une conclusion.
3. **Interface utilisateur** : collecte les faits, affiche les conclusions et explique les règles déclenchées.

Acteurs impliqués :

1. Expert du domaine : fournit les connaissances et exemples.
2. Ingénieur de la connaissance : formalise faits, règles, métarègles.
3. Informaticien : implémente représentation, moteur, interface.
4. Utilisateur : saisit son cas et consulte les résultats.

### Quand utiliser un système expert

Cas typiques :

- Absence d'algorithme connu ou faisable : le raisonnement dépend de l'expertise humaine (ex. diagnostic médical, configuration technique complexe).
- Complexité combinatoire ou heuristique : trop de combinaisons possibles pour une recherche exhaustive (ex. planification de production, ordonnancement).
- Nécessité d'utiliser des connaissances symboliques : utilisation de symboles et catégories plutôt que de calculs numériques (ex. classification, taxonomie).
- Aide à la décision, au diagnostic ou à la classification : assister l'utilisateur dans un choix ou identifier un problème (ex. dépannage informatique, conseil juridique).
- Autre (besoin d'explications sur le raisonnement, capitalisation de l'expertise, aide sur des cas courants mais techniques).

### Ordres

#### Ordre 0

- Faits : propositions simples (ex. pomme = vrai).
- Règles : déclenchées une fois (pas de modification de valeurs).
- Exemple : SI pomme ALORS pomme_au_four (on ajoute pomme_au_four dans la base de faits).

#### Ordre 0+

- Faits : propositions + relations simples <relation, entité, valeur> (ex. pomme = 8).
- Peut modifier les valeurs de la base de faits et tester une règle tant que la condition reste vraie.
- Exemple : SI pomme >= 4 ALORS tarte_aux_pommes (on soustrait 4 à pomme).

#### Ordre 1

- Faits : prédicats avec variables ingredient(tarte, pomme).
- Raisonnement avec unification (logique du premier ordre).
- Exemple : SI ingredient(x, y) ET allergie(y) ALORS interdire(x).

---

## Formalisation de la problématique

### Domaine d’expertise

Recommandation culinaire simple basée sur la disponibilité.

### Problématique

Objectif : fournir la liste des recettes réalisables par un utilisateur à partir des ingrédients et du matériel dont il dispose.

Le système vérifie strictement deux aspects :

- disponibilité des ingrédients en quantité suffisante
- disponibilité du matériel requis

Des filtres optionnels affinent la sélection :

- végétarien (booléen) ;
- saisons sélectionnées (printemps, été, automne, hiver) ;
- type de plat (entrée, plat, dessert).

### Pourquoi un simple algorithme déterministe ne suffit pas

Un algorithme procédural peut faire un filtrage direct. Le format « règles 0+ » reste néanmoins adapté car il est lisible, modulaire et explicable. On sépare les connaissances (règles) des données (faits). On peut tracer précisément pourquoi une recette est incluse ou exclue (ingrédients manquants, saison non sélectionnée, type non demandé). Cette traçabilité est la valeur ajoutée.

### Ce que le système expert doit déterminer / recommander / identifier

Le système doit :

- vérifier la disponibilité stricte des ingrédients et du matériel ;
- appliquer les filtres optionnels (végétarien, saisons, type) ;
- produire la liste complète des recettes disponibles ;
- expliquer les exclusions (manque d’ingrédients, manque de matériel, filtre).

### Pourquoi l’ordre 0+ est adapté

Ordre 0 insuffisant : il ne gère pas les quantités numériques.

Ordre 0+ adapté : il permet les comparaisons numériques nécessaires pour vérifier les quantités d'ingrédients. Il gère aussi des faits booléens pour le matériel et les filtres.

Conclusion : l’ordre 0+ couvre précisément le besoin (comparateurs simples + filtres booléens). L’ordre 1 est inutile ici.

---

## Définition des connaissances (faits + règles)

### Sources d’expertise

- Recettes de cuisine de base (proportions simples, pratiques courantes).
- Conventions de saisonnalité culinaire grand public.

### Types de valeurs

- Booléens : `vegetarien`, `saison`, `type`.
- Numériques simples (entiers) : quantités d'ingrédients disponibles et requis.

### Vocabulaire et faits

- Faits utilisateur (base de faits initiale) :
  - Une quantité N pour chaque ingrédient disponible.
  - La liste du matériel disponible (présence booléenne).
  - Filtres : `recette_vegetarienne` (vrai/faux), `recette_printemps|ete|automne|hiver` (vrai/faux), `type_entree|plat|dessert` (vrai/faux). Si aucun n'est sélectionné pour une catégorie, le filtre est inactif pour cette catégorie.

- Faits catalogue (connaissances générales) : pour chaque recette, on encode des besoins et des méta‑propriétés sous forme de propositions spécifiques à la recette (ordre 0+ sans variables) :
  - Besoins en ingrédients et matériel regroupés dans des règles dédiées.
  - Propriétés : `vegetarien`, `entree|plat|dessert`, `ete|printemps|automne|hiver`.

Dans ce rapport, on illustre avec 5 recettes :

### Base de faits (exemple)

Ingrédients disponibles :

- `beurre` = 200
- `oeuf` = 12
- `creme` = 70
- `lard` = 300
- `gruyere` = 70
- `farine` = 250
- `eau` = 2000

Matériel disponible :

- `bol`
- `rouleau`
- `moule_tarte`
- `fouet`

Filtres activés :

- `recette_vegetarienne` = faux
- `type_plat` = vrai
- `type_entree` = faux
- `type_dessert` = faux
- `recette_printemps` = faux
- `recette_ete` = faux
- `recette_automne` = faux
- `recette_hiver` = faux

#### Connaissances des recettes

Pour chaque recette, on définit :

- Ses besoins en ingrédients (avec quantités minimales requises)
- Ses besoins en matériel
- Ses propriétés : saisons, type de plat, végétarien

### Base de règles

Nous présentons ici l'ensemble des recettes de notre base de connaissances. Chaque recette est définie par les ingrédients requis, le matériel nécessaire, les saisons et son type.

#### Recettes complètes (19 recettes)

##### 1. Riz pilaf

- ***Ingrédients*** : oignon (1), riz_long (300g), beurre (30g), eau (60cl)
- ***Matériel*** : casserole
- ***Saisons*** : printemps, été, automne, hiver
- ***Type*** : plat
- ***Végétarien*** : oui

##### 2. Céleri rôti

- ***Ingrédients*** : céleri (1), ail (2 gousses), huile (2 cl), beurre (20g)
- ***Matériel*** : four
- ***Saisons*** : automne, hiver
- ***Type*** : plat
- ***Végétarien*** : oui

##### 3. Gratin dauphinois

- ***Ingrédients*** : beurre (10g), pomme_de_terre (1200g), lait (40cl), ail (5 gousses), crème (40cl)
- ***Matériel*** : four
- ***Saisons*** : automne, hiver
- ***Type*** : plat
- ***Végétarien*** : oui

##### 4. Fond blanc de volaille

- ***Ingrédients*** : carotte (2), poireau (0.5), oignon (1), ail (2 gousses), volaille (1), eau (200cl)
- ***Matériel*** : casserole, écumoire
- ***Saisons*** : printemps, été, automne, hiver
- ***Type*** : plat
- ***Végétarien*** : non

##### 5. Quiche lorraine

- ***Ingrédients*** : beurre (20g), oeuf (8), crème (40cl), lard (180g), gruyère (70g), pâte_brisée (1)
- ***Matériel*** : moule_tarte, fouet, bol
- ***Saisons*** : printemps, été, automne, hiver
- ***Type*** : plat, entrée
- ***Végétarien*** : non
- ***Profondeur*** : 1 (utilise pâte_brisée)

##### 6. Tarte tatin

- ***Ingrédients*** : pomme (15), sucre (100g), eau (5cl), beurre (50g), pâte_brisée (1)
- ***Matériel*** : moule_tarte, casserole
- ***Saisons*** : automne, hiver
- ***Type*** : dessert
- ***Végétarien*** : oui
- ***Profondeur*** : 1 (utilise pâte_brisée)

##### 7. Crêpe Suzette

- ***Ingrédients*** : huile (4cl), sucre (30g), jus_orange (10cl), beurre (50g), pâte_crêpe (1)
- ***Matériel*** : poêle, spatule, assiette
- ***Saisons*** : printemps, été, automne, hiver
- ***Type*** : dessert
- ***Végétarien*** : oui
- ***Profondeur*** : 1 (utilise pâte_crêpe)

##### 8. Salade composée

- ***Ingrédients*** : lard (80g), tomate (3), emmental (80g), pomme_de_terre (500g), surimi (10 bâtonnets), oeuf (4), cornichon (15)
- ***Matériel*** : saladier, casserole
- ***Saisons*** : printemps, été
- ***Type*** : entrée
- ***Végétarien*** : non

##### 9. Boeuf bourguignon

- ***Ingrédients*** : beurre (60g), oignon (1), farine (30g), boeuf (600g), champignon (30g)
- ***Matériel*** : cocotte
- ***Saisons*** : automne
- ***Type*** : plat
- ***Végétarien*** : non

##### 10. Lasagne

- ***Ingrédients*** : pâte_lasagne (1), béchamel (1), farce_viande (1)
- ***Matériel*** : four
- ***Saisons*** : printemps, été, automne, hiver
- ***Type*** : plat
- ***Végétarien*** : non
- ***Profondeur*** : 1 (utilise pâte_lasagne, béchamel, farce_viande)

##### 11. Omelette aux champignons

- ***Ingrédients*** : oeuf (3), champignon (150g), beurre (1g)
- ***Matériel*** : poêle, spatule, bol
- ***Saisons*** : printemps, été, automne, hiver
- ***Type*** : plat
- ***Végétarien*** : oui

##### 12. Pâtes carbonara

- ***Ingrédients*** : pâtes (200g), lardons (100g), oeuf (2)
- ***Matériel*** : casserole, poêle
- ***Saisons*** : printemps, été, automne, hiver
- ***Type*** : plat
- ***Végétarien*** : non

##### 13. Soupe au potiron

- ***Ingrédients*** : potiron (400g), oignon (1), bouillon_légumes (1 cube)
- ***Matériel*** : casserole, mixeur
- ***Saisons*** : automne, hiver
- ***Type*** : plat
- ***Végétarien*** : oui

##### 14. Cannellonis farcis

- ***Ingrédients*** : pâte_lasagne (1), sauce_bolognaise (1), béchamel (1), parmesan (50g)
- ***Matériel*** : four
- ***Saisons*** : printemps, été, automne, hiver
- ***Type*** : plat
- ***Végétarien*** : non
- ***Profondeur*** : 2 (utilise pâte_lasagne, béchamel, sauce_bolognaise qui utilise concassé, farce_viande)

#### Compositions intermédiaires (profondeur 1)

Ces préparations sont utilisées comme ingrédients dans d'autres recettes :

##### Pâte brisée

- ***Ingrédients*** : farine (200g), oeuf (1), eau (4cl), beurre (100g)
- ***Matériel*** : bol, rouleau

##### Pâte à crêpe

- ***Ingrédients*** : beurre (40g), oeuf (4), farine (250g), lait (60cl), sucre (40g)
- ***Matériel*** : bol, fouet

##### Pâte à lasagne

- ***Ingrédients*** : farine (175g), oeuf (6)
- ***Matériel*** : bol, rouleau

##### Béchamel

- ***Ingrédients*** : beurre (30g), farine (30g), lait (45cl)
- ***Matériel*** : casserole, fouet

##### Farce de viande

- ***Ingrédients*** : oignon (1), ail (2 gousses), huile (2cl), boeuf (600g)
- ***Matériel*** : poêle, spatule

##### Concassé de tomates

- ***Ingrédients*** : tomate (5), oignon (1), ail (3 gousses), huile (2cl)
- ***Matériel*** : poêle

##### Sauce bolognaise

- ***Ingrédients*** : concassé (1), farce_viande (1)
- ***Matériel*** : casserole

#### Règles pour les recettes complètes

```sql
'R1' : SI oignon >= 1 ET riz_long >= 300 ET beurre >= 30 ET eau >= 60
ET casserole ET (recette_printemps OU recette_ete OU recette_automne
OU recette_hiver) ET type_plat ET recette_vegetarienne ALORS riz_pilaf

'R2' : SI celeri >= 1 ET ail >= 2 ET huile >= 2 ET beurre >= 20 ET four
ET (recette_automne OU recette_hiver) ET type_plat ET recette_vegetarienne
ALORS celeri_roti

'R3' : SI beurre >= 10 ET pomme_de_terre >= 1200 ET lait >= 40 ET ail >= 5
ET creme >= 40 ET four ET (recette_automne OU recette_hiver) ET type_plat
ET recette_vegetarienne ALORS gratin_dauphinois

'R4' : SI carotte >= 2 ET poireau >= 0.5 ET oignon >= 1 ET ail >= 2 ET
volaille >= 1 ET eau >= 200 ET casserole ET ecumoire ET (recette_printemps OU recette_ete OU recette_automne OU recette_hiver) ET type_plat ALORS fond_blanc_volaille

'R5' : SI beurre >= 20 ET oeuf >= 8 ET creme >= 40 ET lard >= 180 ET gruyere >= 70 ET 'pate_brisee' ET moule_tarte ET fouet ET bol ET (recette_printemps OU recette_ete OU recette_automne OU recette_hiver) ET (type_plat OU type_entree) ALORS quiche_lorraine

'R6' : SI pomme >= 15 ET sucre >= 100 ET eau >= 5 ET beurre >= 50 ET 'pate_brisee' ET moule_tarte ET casserole ET (recette_automne OU recette_hiver) ET type_dessert ET recette_vegetarienne ALORS tarte_tatin

'R7' : SI huile >= 4 ET sucre >= 30 ET jus_orange >= 10 ET beurre >= 50 ET 'pate_crepe' ET poele ET spatule ET assiette ET (recette_printemps OU recette_ete OU recette_automne OU recette_hiver) ET type_dessert ET recette_vegetarienne ALORS crepe_suzette

'R8' : SI lard >= 80 ET tomate >= 3 ET emmental >= 80 ET pomme_de_terre >= 500 ET surimi >= 10 ET oeuf >= 4 ET cornichon >= 15 ET saladier ET casserole ET (recette_printemps OU recette_ete) ET type_entree ALORS salade_composee

'R9' : SI beurre >= 60 ET oignon >= 1 ET farine >= 30 ET boeuf >= 600 ET champignon >= 30 ET cocotte ET recette_automne ET type_plat ALORS boeuf_bourguignon

'R10' : SI 'pate_lasagne' ET 'bechamel' ET 'farce_viande' ET four ET (recette_printemps OU recette_ete OU recette_automne OU recette_hiver) ET type_plat ALORS lasagne

'R11' : SI oeuf >= 3 ET champignon >= 150 ET beurre >= 1 ET poele ET spatule ET bol ET (recette_printemps OU recette_ete OU recette_automne OU recette_hiver) ET type_plat ET recette_vegetarienne ALORS omelette_champignons

'R12' : SI pates >= 200 ET lardons >= 100 ET oeuf >= 2 ET casserole ET poele ET (recette_printemps OU recette_ete OU recette_automne OU recette_hiver) ET type_plat ALORS pates_carbonara

'R13' : SI potiron >= 400 ET oignon >= 1 ET bouillon_legumes >= 1 ET casserole ET mixeur ET (recette_automne OU recette_hiver) ET type_plat ET recette_vegetarienne ALORS soupe_potiron

'R14' : SI 'pate_lasagne' ET 'sauce_bolognaise' ET 'bechamel' ET parmesan >= 50 ET four ET (recette_printemps OU recette_ete OU recette_automne OU recette_hiver) ET type_plat ALORS cannellonis

#### Règles pour les compositions intermédiaires
```

#### Règles pour les compositions intermédiaires

```sql
'R15' : SI farine >= 200 ET oeuf >= 1 ET eau >= 4 ET beurre >= 100 ET bol ET rouleau ALORS pate_brisee

'R16' : SI beurre >= 40 ET oeuf >= 4 ET farine >= 250 ET lait >= 60 ET sucre >= 40 ET bol ET fouet ALORS pate_crepe

'R17' : SI farine >= 175 ET oeuf >= 6 ET bol ET rouleau ALORS pate_lasagne

'R18' : SI beurre >= 30 ET farine >= 30 ET lait >= 45 ET casserole ET fouet ALORS bechamel

'R19' : SI oignon >= 1 ET ail >= 2 ET huile >= 2 ET boeuf >= 600 ET poele ET spatule ALORS farce_viande

'R20' : SI tomate >= 5 ET oignon >= 1 ET ail >= 3 ET huile >= 2 ET poele ALORS concasse

'R21' : SI 'concasse' ET 'farce_viande' ET casserole ALORS sauce_bolognaise
```

#### Justification

Nous avons défini **21 règles** (14 recettes finales + 7 compositions intermédiaires). Chaque règle condense la vérification complète en une seule ligne : ingrédients (quantités), matériel (booléen), saisons (disjonction), type de plat et caractère végétarien. Les compositions intermédiaires ne nécessitent pas de filtres car elles sont des étapes préparatoires.

---

## Raisonnement (chaînage avant / arrière)

### Exemple de chaînage avant

//

### Exemple de chaînage arrière

//

### Arbre de déduction

//

---

## Discussion et conclusion

Le système expert fournit la liste des recettes réalisables avec les ressources de l’utilisateur. Il applique des règles 0+ simples, lisibles et traçables. Chaque inclusion est justifiée par des règles techniques (ingrédients et matériel) et des filtres (végétarien, saisons, type).

Limites :

- Unités et quantités sont simplifiées (entiers). Les grammes et volumes précis ne sont pas gérés.
- Les substitutions d’ingrédients ne sont pas traitées.
- Pas de préférences gustatives ni de contraintes nutritionnelles.

Améliorations possibles :

- Ajouter des métarègles de priorité (préférer entrée puis plat, etc.).
- Gérer des unités hétérogènes et des conversions simples.
- Expliquer automatiquement les raisons d’exclusion détaillées (liste des manques).
- Étendre vers un ordre 1 pour factoriser les recettes avec des schémas génériques.

Conclusion : l’ordre 0+ est adapté à ce besoin. Il permet de vérifier des quantités et d’appliquer des filtres booléens tout en restant explicable. Le résultat final est la liste complète des recettes disponibles pour l’utilisateur.
