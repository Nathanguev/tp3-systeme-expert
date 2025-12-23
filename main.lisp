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
(ajouter-fait 'ingredients 'farine 350)
  (ajouter-fait 'ingredients 'oeuf 13)
  (ajouter-fait 'ingredients 'huile 15)
  
  ;; 2. Pour la Béchamel
  (ajouter-fait 'ingredients 'beurre 30)
  (ajouter-fait 'ingredients 'farine 30) ;; Attention: cumul possible selon ton implémentation
  (ajouter-fait 'ingredients 'lait 300)

  ;; 3. Pour la Sauce Bolognaise (via Concasse + Farce)
  ;; -> Concasse de tomates
  (ajouter-fait 'ingredients 'tomate 5)
  (ajouter-fait 'ingredients 'oignon 1)
  (ajouter-fait 'ingredients 'ail 3)
  (ajouter-fait 'ingredients 'huile 2)
  ;; -> Farce viande
  (ajouter-fait 'ingredients 'oignon 1) ;; 2ème oignon
  (ajouter-fait 'ingredients 'ail 2)    ;; +2 ails
  (ajouter-fait 'ingredients 'huile 30) ;; +30 huile
  (ajouter-fait 'ingredients 'boeuf 600)
  
  ;; 4. Pour le montage final Lasagnes
  (ajouter-fait 'ingredients 'gruyere 100)
  
  ;; Matériel total nécessaire
  (ajouter-fait 'materiel 'four t)
  (ajouter-fait 'materiel 'bol t)
  (ajouter-fait 'materiel 'rouleau t)
  (ajouter-fait 'materiel 'casserole t)
  (ajouter-fait 'materiel 'fouet t)
  (ajouter-fait 'materiel 'poele t)
  (ajouter-fait 'materiel 'spatule t)
  (interface-principale))



;;; ----------------------------------------------------------------------------
;;; LANCEMENT AUTOMATIQUE
;;; ----------------------------------------------------------------------------

;; Lancer le système expert au chargement du fichier
(demarrer-systeme-expert)

