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

    ;   (pate_brisee
    ;  ((farine 200) (oeuf 1) (eau 4) (beurre 100))
    ;  (bol rouleau)
    ;  nil nil vegetarien)

    ; (pate_crepe
    ;  ((beurre 40) (oeuf 4) (farine 250) (lait 60) (sucre 40))
    ;  (bol fouet)
    ;  nil nil vegetarien)
    ; (quiche_lorraine
    ;  ((beurre 20) (oeuf 8) (creme 40) (lard 180) (gruyere 70) (pate_brisee 1))
    ;  (moule_tarte fouet bol)
    ;  (printemps ete automne hiver)
    ;  (plat entree)
    ;  nil)
    ;     (tarte_tatin
    ;  ((pomme 15) (sucre 100) (eau 5) (beurre 50) (pate_brisee 1))
    ;  (moule_tarte casserole)
    ;  (automne hiver)
    ;  dessert
    ;  vegetarien)
    (format t "~%~%~%~%~%~%~%~%~%~%")
    (ajouter-fait 'ingredients 'pomme 150)
    (ajouter-fait 'ingredients 'creme 100)
    (ajouter-fait 'ingredients 'lard 200)
    (ajouter-fait 'ingredients 'gruyere 100)
    (ajouter-fait 'ingredients 'farine 500)
    (ajouter-fait 'ingredients 'oeuf 60)
    (ajouter-fait 'ingredients 'eau 1000)
    (ajouter-fait 'ingredients 'beurre 500)
    (ajouter-fait 'ingredients 'lait 200)
    (ajouter-fait 'ingredients 'sucre 300)
    (ajouter-fait 'materiel 'bol t)
    (ajouter-fait 'materiel 'rouleau t)
    (ajouter-fait 'materiel 'fouet t)
    (ajouter-fait 'materiel 'moule_tarte t)
    (ajouter-fait 'materiel 'casserole t)
    (afficher-base-faits)
    (chainage-avant)
    (print *solutions-trouvees*)
    (format t "~%")
  ;; Lancement de l'interface utilisateur
  ;(interface-principale)
  )



;;; ----------------------------------------------------------------------------
;;; LANCEMENT AUTOMATIQUE
;;; ----------------------------------------------------------------------------

;; Lancer le système expert au chargement du fichier
(demarrer-systeme-expert)
