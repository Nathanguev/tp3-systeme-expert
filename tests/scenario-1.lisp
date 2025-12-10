;;;; ============================================================================
;;;; SCÉNARIO DE TEST 1 - BASE DE FAITS EXEMPLE
;;;; ============================================================================
;;;; Description : Premier scénario de test avec des ingrédients et matériel
;;;;               prédéfinis pour tester le système expert
;;;; ============================================================================

(load "src/base-faits.lisp")
;;; Charger le système
(load "main.lisp")

;;; ----------------------------------------------------------------------------
;;; CONFIGURATION DU SCÉNARIO 1
;;; ----------------------------------------------------------------------------

(defun executer-scenario-1 ()
  "Scénario 1 : Utilisateur avec ingrédients pour quiche lorraine."

  (format t "~%")
  (format t "=======================================~%")
  (format t "          SCÉNARIO DE TEST 1           ~%")
  (format t "      Objectif : Quiche lorraine       ~%")
  (format t "=======================================~%")
  (format t "~%")

  ;; Réinitialiser le système
  (initialiser-base-faits)
  (reinitialiser-moteur)

  ;; TODO: Configurer les ingrédients disponibles
  ;; Exemple basé sur le TP2 :
  (ajouter-fait 'ingredients 'beurre 200)
  (ajouter-fait 'ingredients 'oeuf 12)
  (ajouter-fait 'ingredients 'creme 70)
  (ajouter-fait 'ingredients 'lard 300)
  (ajouter-fait 'ingredients 'gruyere 70)
  (ajouter-fait 'ingredients 'farine 250)
  (ajouter-fait 'ingredients 'eau 2000)

  ;; TODO: Configurer le matériel disponible
  (ajouter-fait 'materiel 'bol t)
  (ajouter-fait 'materiel 'rouleau t)
  (ajouter-fait 'materiel 'moule_tarte t)
  (ajouter-fait 'materiel 'fouet t)

  ;; TODO: Configurer les filtres
  (ajouter-fait 'filtres 'recette_vegetarienne nil)
  (ajouter-fait 'filtres 'type_plat t)
  (ajouter-fait 'filtres 'type_entree nil)
  (ajouter-fait 'filtres 'type_dessert nil)
  (ajouter-fait 'filtres 'recette_printemps nil)
  (ajouter-fait 'filtres 'recette_ete nil)
  (ajouter-fait 'filtres 'recette_automne nil)
  (ajouter-fait 'filtres 'recette_hiver nil)

  (format t "Faits initiaux configurés :~%")
  (afficher-base-faits)

  ;; Lancer le chaînage avant
  (format t "~%Lancement du chaînage avant...~%")
  (let ((resultats (chainage-avant)))
    (format t "~%Résultats obtenus :~%")
    (if resultats
        (progn
          (format t "Recettes réalisables : ~A~%" resultats)
          (afficher-trace-inference))
        (format t "Aucune recette réalisable.~%")))

  ;; Afficher les statistiques
  (format t "~%Statistiques :~%")
  (afficher-statistiques))

;;; ----------------------------------------------------------------------------
;;; AUTRES SCÉNARIOS À IMPLÉMENTER
;;; ----------------------------------------------------------------------------

(defun executer-scenario-2 ()
  "Scénario 2 : Utilisateur végétarien avec peu d'ingrédients."
  ;; TODO: Implémenter un deuxième scénario
  ;; - Ingrédients limités
  ;; - Filtre végétarien actif
  ;; - Tester différentes saisons
  )

(defun executer-scenario-3 ()
  "Scénario 3 : Test du chaînage arrière pour une recette spécifique."
  
  (format t "~%")
  (format t "=======================================~%")
  (format t "          SCÉNARIO DE TEST 3           ~%")
  (format t "  Objectif : Chaînage arrière simple   ~%")
  (format t "=======================================~%")
  (format t "~%")

  ;; Réinitialiser le système
  (initialiser-base-faits)
  (reinitialiser-moteur)

  ;; Configurer quelques ingrédients de base
  (ajouter-fait 'ingredients 'beurre 300)
  (ajouter-fait 'ingredients 'farine 200)
  (ajouter-fait 'ingredients 'oeuf 6)
  (ajouter-fait 'ingredients 'sucre 150)
  (ajouter-fait 'ingredients 'pomme 20)
  (ajouter-fait 'ingredients 'eau 1000)

  ;; Configurer le matériel
  (ajouter-fait 'materiel 'bol t)
  (ajouter-fait 'materiel 'rouleau t)
  (ajouter-fait 'materiel 'moule_tarte t)
  (ajouter-fait 'materiel 'four t)
  (ajouter-fait 'materiel 'casserole t)

  (format t "Faits initiaux configurés :~%")
  (afficher-base-faits)

  ;; Tester le chaînage arrière sur une recette spécifique
  (format t "~%Test du chaînage arrière sur 'tarte_tatin'...~%")
  (let ((resultat (chainage-arriere 'tarte_tatin)))
    (format t "~%Résultat :~%")
    (if resultat
        (progn
          (format t "La recette 'tarte_tatin' PEUT être réalisée !~%")
          (format t "~%Trace du raisonnement :~%")
          (dolist (entree (reverse *regles-declenchees*))
            (format t "  - Règle ~A -> ~A~%" (car entree) (cdr entree))))
        (format t "La recette 'tarte_tatin' NE PEUT PAS être réalisée.~%")))

  (format t "~%Base de faits après chaînage arrière :~%")
  (afficher-base-faits))

(defun executer-tous-scenarios ()
  "Execute tous les scénarios de test."
  (executer-scenario-1)
  (format t "~%~%")
  (executer-scenario-2)
  (format t "~%~%")
  (executer-scenario-3))

;;; ----------------------------------------------------------------------------
;;; LANCEMENT
;;; ----------------------------------------------------------------------------

;; Exécuter le scénario 1 au chargement
(executer-scenario-3)
