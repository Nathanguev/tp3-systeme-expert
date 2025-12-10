;;;; ============================================================================
;;;; SYSTÈME EXPERT CULINAIRE - POINT D'ENTRÉE PRINCIPAL
;;;; ============================================================================
;;;; Projet : TP3 IA01 - Système Expert d'ordre 0+
;;;; Auteurs : Nathan GUEVARA, Loïc ZHOU
;;;; Description : Point d'entrée principal intégrant tous les composants
;;;;               du système expert de recommandation culinaire
;;;; ============================================================================

;;; Chargement des modules du système expert
(load "src/base-faits.lisp")
(load "src/base-regles.lisp")       ; Données brutes des recettes
(load "src/gestion-regles.lisp")    ; Gestion de la base de règles
(load "src/moteur.lisp")
(load "src/interface.lisp")
(load "donnees/recettes.lisp")      ; Parsing et chargement des règles

;;; ----------------------------------------------------------------------------
;;; FONCTION PRINCIPALE
;;; ----------------------------------------------------------------------------



(defun demarrer-systeme-expert ()
  "Point d'entrée principal du système expert culinaire.
   Lance l'interface utilisateur et coordonne l'exécution complète."
  (format t "~%")
  (format t "=======================================~%")
  (format t "  SYSTÈME EXPERT CULINAIRE - IA01 TP3  ~%")
  (format t "=======================================~%")
  (format t "~%")

  ;; Initialisation de la base de faits
  (initialiser-base-faits)
  ;; Chargement des règles depuis les données
  (charger-recettes)
  ;; Lancement de l'interface utilisateur
  (ajouter-fait 'filtres 'hiver t)      ; Les cannellonis se mangent en hiver
(ajouter-fait 'filtres 'plat t)      ; On cherche un plat principal

;; 2. AJOUT DU MATÉRIEL (Tout le matériel nécessaire)
;; ----------------------------------------------------------------------------
;; Pour la pâte à lasagne
(ajouter-fait 'materiel 'bol t)
(ajouter-fait 'materiel 'rouleau t)
;; Pour la béchamel et bolognaise
(ajouter-fait 'materiel 'casserole t)
(ajouter-fait 'materiel 'fouet t)
;; Pour la farce et concassé
(ajouter-fait 'materiel 'poele t)
(ajouter-fait 'materiel 'spatule t)
;; Pour la cuisson finale
(ajouter-fait 'materiel 'four t)

;; 3. AJOUT DES INGRÉDIENTS BRUTS (Quantités suffisantes pour toutes les sous-recettes)
;; ----------------------------------------------------------------------------

;; -- Pour la Pâte à Lasagne (Besoin: Farine 175, Oeuf 6) --
(ajouter-fait 'ingredients 'farine 500) ; Large stock de farine
(ajouter-fait 'ingredients 'oeuf 12)     ; Large stock d'oeufs

;; -- Pour la Béchamel (Besoin: Beurre 30, Farine 30, Lait 45) --
(ajouter-fait 'ingredients 'beurre 200)  ; Assez pour béchamel + cuisson
(ajouter-fait 'ingredients 'lait 100)

;; -- Pour la Farce Viande (Besoin: Oignon 1, Ail 2, Huile 2, Boeuf 600) --
(ajouter-fait 'ingredients 'boeuf 1000)

;; -- Pour le Concassé (Besoin: Tomate 5, Oignon 1, Ail 3, Huile 2) --
(ajouter-fait 'ingredients 'tomate 10)

;; -- Ingrédients communs (Aromates / Huile) --
;; Note : Il faut assez pour la Farce ET le Concassé cumulés
(ajouter-fait 'ingredients 'oignon 5)    ; Besoin total : 1 + 1 = 2
(ajouter-fait 'ingredients 'ail 10)      ; Besoin total : 2 + 3 = 5
(ajouter-fait 'ingredients 'huile 10)    ; Besoin total : 2 + 2 = 4

;; -- Pour la finition (Cannellonis) --
(ajouter-fait 'ingredients 'parmesan 100)
(chainage-avant)
(print *solutions-trouvees*)
  (interface-principale)
  )



;;; ----------------------------------------------------------------------------
;;; LANCEMENT AUTOMATIQUE
;;; ----------------------------------------------------------------------------

;; Lancer le système expert au chargement du fichier
(demarrer-systeme-expert)

