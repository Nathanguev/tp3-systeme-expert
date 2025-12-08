# Interface Web - Guide de développement

## Architecture

```txt
web/
├── index.html      # Structure HTML (squelette créé)
├── style.css       # Styles CSS (squelette créé)
└── app.js          # Logique JavaScript (squelette créé)

src/
├── web-server.lisp # Serveur Hunchentoot (squelette créé)
└── web-api.lisp    # Endpoints REST/JSON (squelette créé)
```

## Dépendances à installer

Avant de lancer l'interface web, installer les bibliothèques Lisp :

```lisp
;; Dans un REPL SBCL
(ql:quickload :hunchentoot)
(ql:quickload :cl-json)
```

## Ordre de développement recommandé

### 1. Serveur web (src/web-server.lisp)

- [ ] `demarrer-serveur-web()` - Créer l'instance Hunchentoot
- [ ] `arreter-serveur-web()` - Stopper le serveur
- [ ] `configurer-fichiers-statiques()` - Servir HTML/CSS/JS
- [ ] Handlers de pages (/, /404)

### 2. API REST (src/web-api.lisp)

- [ ] `reponse-json()` - Utilitaire JSON
- [ ] `api-liste-ingredients` - GET /api/ingredients
- [ ] `api-liste-materiel` - GET /api/materiel
- [ ] `api-ajouter-ingredient` - POST /api/ingredients/add
- [ ] `api-ajouter-materiel` - POST /api/materiel/add
- [ ] `api-configurer-filtres` - POST /api/filtres
- [ ] `api-lancer-recherche` - POST /api/recherche
- [ ] `api-trace` - GET /api/trace

### 3. Frontend JavaScript (web/app.js)

- [ ] `afficherOnglet()` - Navigation entre onglets
- [ ] `apiGet()` et `apiPost()` - Appels HTTP
- [ ] `chargerIngredients()` - Charger depuis API
- [ ] `afficherIngredients()` - Afficher dans DOM
- [ ] `validerIngredients()` - Envoyer à l'API
- [ ] Même chose pour matériel
- [ ] `validerFiltres()` - Configuration filtres
- [ ] `lancerRecherche()` - Lancer chaînage avant
- [ ] `afficherRecettes()` - Afficher résultats
- [ ] `afficherTrace()` - Visualiser trace
- [ ] `afficherNotification()` - Notifications utilisateur

## Test minimal

### Test 1 : Serveur web seul

```lisp
;; Charger uniquement le serveur
(load "src/web-server.lisp")

;; Démarrer
(demarrer-serveur-web)

;; Tester dans le navigateur : http://localhost:8080
;; Devrait afficher index.html

;; Arrêter
(arreter-serveur-web)
```

### Test 2 : API endpoints

```lisp
;; Charger serveur + API
(load "src/web-server.lisp")
(load "src/web-api.lisp")

;; Démarrer
(demarrer-serveur-web)

;; Tester dans le navigateur :
;; http://localhost:8080/api/ingredients
;; Devrait retourner JSON avec liste d'ingrédients
```

### Test 3 : Interface complète

```bash
# Lancer le système
sbcl --load main.lisp

# Choisir : 2 (Interface web)
# Ouvrir : http://localhost:8080
```

## Points d'attention

### Encodage JSON

- Utiliser `cl-json:encode-json-to-string` pour convertir structures Lisp → JSON
- Symboles Lisp → strings : `(symbol-name symbole)`
- Listes associatives → objets JSON

### CORS (si nécessaire)

Si vous voulez accéder depuis un autre domaine :

```lisp
(setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
```

### Fichiers statiques

Le serveur doit servir HTML/CSS/JS depuis le dossier `web/` :

```lisp
(push (hunchentoot:create-folder-dispatcher-and-handler
       "/" "web/")
      hunchentoot:*dispatch-table*)
```

## Exemples de réponses JSON

### GET /api/ingredients

```json
{
  "success": true,
  "data": ["tomate", "oeuf", "farine", "beurre", "sucre"]
}
```

### POST /api/recherche

```json
{
  "success": true,
  "data": {
    "recettes": [
      {
        "nom": "quiche_lorraine",
        "description": "Tarte salée aux œufs et lard",
        "type": "plat",
        "vegetarien": false,
        "saisons": ["automne", "hiver"]
      }
    ],
    "trace": [
      {"etape": 1, "regle": "regle_pate_brisee", "fait": "pate_brisee"},
      {"etape": 2, "regle": "regle_quiche_lorraine", "fait": "quiche_lorraine"}
    ]
  }
}
```

## Debugging

### Logs serveur

Hunchentoot affiche automatiquement les requêtes dans la console :

```bash
[INFO] 127.0.0.1 - [08/Dec/2025:14:23:45] "GET /api/ingredients HTTP/1.1" 200
```

### JavaScript console

Utiliser `console.log()` pour debugger :

```javascript
console.log('État actuel:', state);
console.log('Réponse API:', response);
```

### Erreurs communes

1. **Serveur ne démarre pas** → Vérifier que le port 8080 est libre
2. **404 sur /api/*** → Vérifier que les handlers sont définis
3. **Erreur JSON** → Vérifier l'encodage des symboles Lisp
4. **CORS error** → Ajouter headers CORS

## Bonus : Features avancées (optionnel)

- [ ] Graphique de dépendances des règles (D3.js)
- [ ] Animation de la trace en temps réel
- [ ] Sauvegarde de sessions (localStorage)
- [ ] Mode sombre (dark mode)
- [ ] Export PDF du résultat
- [ ] Recherche en temps réel (debounce)

---

**Auteurs** : Nathan GUEVARA & Loïc ZHOU  
**Projet** : TP3 IA01 - Système Expert Culinaire  
**Date** : Décembre 2025
