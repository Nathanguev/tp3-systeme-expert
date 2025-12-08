;;;; ============================================================================
;;;; SYSTÈME EXPERT CULINAIRE - POINT D'ENTRÉE PRINCIPAL
;;;; ============================================================================
;;;; Projet : TP3 IA01 - Système Expert d'ordre 0+
;;;; Auteurs : Nathan GUEVARA, Loïc ZHOU
;;;; Description : Point d'entrée principal intégrant tous les composants
;;;;               du système expert de recommandation culinaire
;;;; ============================================================================

;;; Configuration des chemins pour les bibliothèques Common Lisp (apt)
(require :asdf)

;;; Chargement des modules du système expert
(load "src/base-faits.lisp")
(load "src/base-regles.lisp")       ; Données brutes des recettes
(load "src/gestion-regles.lisp")    ; Gestion de la base de règles
(load "src/moteur.lisp")
(load "src/interface.lisp")
(load "donnees/recettes.lisp")      ; Parsing et chargement des règles

;;; Modules optionnels pour l'interface web
(defvar *interface-web-disponible* nil
  "Indique si l'interface web est disponible")

;; Tentative de chargement de l'interface web
(handler-case
    (progn
      (load "src/web-server.lisp")
      (load "src/web-api.lisp")
      (setf *interface-web-disponible* t)
      (format t "Interface web disponible~%"))
  (error (c)
    (format t "Interface web non disponible (dépendances manquantes)~%")
    (format t "Erreur : ~A~%" c)))

;;; ----------------------------------------------------------------------------
;;; FONCTION PRINCIPALE
;;; ----------------------------------------------------------------------------

(defun demarrer-systeme-expert ()
  "Point d'entrée principal du système expert culinaire.
   Lance l'interface utilisateur et coordonne l'exécution complète."
  (format t "~%")
  (format t "=======================================~%")
  (format t "  SYSTÈME EXPERT CULINAIRE - IA01 TP3~%")
  (format t "=======================================~%")
  (format t "~%")
  
  ;; Initialisation de la base de faits
  (initialiser-base-faits)
  
  ;; Chargement des règles depuis les données
  (charger-recettes)
  
  ;; Choix de l'interface
  (if *interface-web-disponible*
      (progn
        (format t "Choisissez l'interface :~%")
        (format t "  1. Interface console (CLI)~%")
        (format t "  2. Interface web (navigateur)~%")
        (format t "~%Votre choix : ")
        (finish-output)
        
        (let* ((input (read-line))
               (choix (parse-integer input :junk-allowed t)))
          
          (cond
            ;; Interface console
            ((eql choix 1)
             (format t "~%Lancement de l'interface console...~%~%")
             (interface-principale))
            
            ;; Interface web
            ((eql choix 2)
             (format t "~%Démarrage du serveur web...~%")
             (demarrer-serveur-web)
             (format t "~%Interface web accessible sur : ~A~%" (obtenir-url-serveur))
             (format t "~%Appuyez sur Entrée pour arrêter le serveur...~%")
             (read-line)
             (arreter-serveur-web)
             (format t "~%Serveur arrêté. Au revoir !~%"))
            
            ;; Choix invalide
            (t
             (format t "~%Choix invalide. Lancement de l'interface console par défaut...~%~%")
             (interface-principale)))))
      
      ;; Si interface web non disponible, utiliser CLI
      (progn
        (format t "Interface web non disponible.~%")
        (format t "Lancement de l'interface console...~%~%")
        (interface-principale))))

;;; ----------------------------------------------------------------------------
;;; LANCEMENT AUTOMATIQUE
;;; ----------------------------------------------------------------------------

;; Lancer le système expert au chargement du fichier
;; Commenté pour permettre le chargement sans lancement automatique
;; Décommentez ou appelez (demarrer-systeme-expert) pour lancer
(demarrer-systeme-expert)
