;;;; ============================================================================
;;;; SCÉNARIO DE TEST 1 - BASE DE FAITS EXEMPLE
;;;; ============================================================================
;;;; Description : Premier scénario de test avec des ingrédients et matériel
;;;;               prédéfinis pour tester le système expert
;;;; ============================================================================

;;; Charger le système
(load "../main.lisp")

;;; ----------------------------------------------------------------------------
;;; CONFIGURATION DU SCÉNARIO 1
;;; ----------------------------------------------------------------------------

(defun executer-scenario-1 ()
  "Scénario 1 : Utilisateur avec ingrédients pour quiche lorraine."
  
  (format t "~%")
  (format t "========================================~%")
  (format t " SCÉNARIO DE TEST 1~%")
  (format t " Objectif : Quiche lorraine~%")
  (format t "========================================~%")
  (format t "~%")
  
  ;; Réinitialiser le système
  (initialiser-base-faits)
  (reinitialiser-moteur)
  
  ;; TODO: Configurer les ingrédients disponibles
  ;; Exemple basé sur le TP2 :
  ;; (ajouter-fait 'beurre 200)
  ;; (ajouter-fait 'oeuf 12)
  ;; (ajouter-fait 'creme 70)
  ;; (ajouter-fait 'lard 300)
  ;; (ajouter-fait 'gruyere 70)
  ;; (ajouter-fait 'farine 250)
  ;; (ajouter-fait 'eau 2000)
  
  ;; TODO: Configurer le matériel disponible
  ;; (ajouter-fait 'bol t)
  ;; (ajouter-fait 'rouleau t)
  ;; (ajouter-fait 'moule_tarte t)
  ;; (ajouter-fait 'fouet t)
  
  ;; TODO: Configurer les filtres
  ;; (ajouter-fait 'recette_vegetarienne nil)
  ;; (ajouter-fait 'type_plat t)
  ;; (ajouter-fait 'type_entree nil)
  ;; (ajouter-fait 'type_dessert nil)
  ;; (ajouter-fait 'recette_printemps nil)
  ;; (ajouter-fait 'recette_ete nil)
  ;; (ajouter-fait 'recette_automne nil)
  ;; (ajouter-fait 'recette_hiver nil)
  
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
  ;; TODO: Implémenter un troisième scénario
  ;; - Définir un but précis (ex: tarte_tatin)
  ;; - Lancer chaînage arrière
  ;; - Vérifier les compositions intermédiaires
  )

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
(executer-scenario-1)
