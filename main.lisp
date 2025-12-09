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
  (format t "========================================~%")
  (format t " SYSTÈME EXPERT CULINAIRE - IA01 TP3~%")
  (format t "========================================~%")
  (format t "~%")

  ;; Initialisation de la base de faits
  (initialiser-base-faits)

  ;; Chargement des règles depuis les données
  (charger-recettes)

  ;; Lancement de l'interface utilisateur
  (interface-principale))

;;; ----------------------------------------------------------------------------
;;; LANCEMENT AUTOMATIQUE
;;; ----------------------------------------------------------------------------

;; Lancer le système expert au chargement du fichier
(demarrer-systeme-expert)
