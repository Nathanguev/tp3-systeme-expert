;;;; ============================================================================
;;;; DONNÉES DES RECETTES - BASE DE CONNAISSANCES
;;;; ============================================================================
;;;; Description : Définition de toutes les recettes du système expert
;;;;               Chargement dans la base de règles
;;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; CHARGEMENT DES RECETTES
;;; ----------------------------------------------------------------------------

(defun charger-recettes ()
  "Charge toutes les recettes dans la base de règles.
   Crée les règles pour les compositions intermédiaires et les recettes finales."
  
  (format t "Chargement des recettes...~%")
  
  ;; Initialiser la base de règles
  (initialiser-base-regles)
  
  ;; Charger les compositions intermédiaires (profondeur 1+)
  (charger-compositions-intermediaires)
  
  ;; Charger les recettes finales (profondeur 0)
  (charger-recettes-finales)
  
  (format t "~D règles chargées avec succès.~%" (length *base-regles*)))

;;; ----------------------------------------------------------------------------
;;; COMPOSITIONS INTERMÉDIAIRES (Profondeur 1+)
;;; ----------------------------------------------------------------------------

(defun charger-compositions-intermediaires ()
  "Charge les compositions intermédiaires (pâtes, sauces, etc.)."
  
  ;; TODO: Implémenter le chargement des compositions
  ;; Pour chaque composition, créer une règle avec ajouter-regle
  
  ;; Exemple de structure (à adapter) :
  ;; Pâte brisée
  ;; (ajouter-regle 
  ;;   'R15
  ;;   "Pâte brisée"
  ;;   '((farine >= 200) (oeuf >= 1) (eau >= 4) (beurre >= 100) 
  ;;     (bol = t) (rouleau = t))
  ;;   'pate_brisee
  ;;   :actions '((decremente farine 200) (decremente oeuf 1) 
  ;;              (decremente eau 4) (decremente beurre 100))
  ;;   :profondeur 1
  ;;   :metadata '(:vegetarien t))
  
  ;; Pâte à crêpe
  ;; (ajouter-regle ...)
  
  ;; Pâte à lasagne
  ;; (ajouter-regle ...)
  
  ;; Béchamel
  ;; (ajouter-regle ...)
  
  ;; Farce de viande
  ;; (ajouter-regle ...)
  
  ;; Concassé de tomates
  ;; (ajouter-regle ...)
  
  ;; Sauce bolognaise
  ;; (ajouter-regle ...)
  )

;;; ----------------------------------------------------------------------------
;;; RECETTES FINALES (Profondeur 0)
;;; ----------------------------------------------------------------------------

(defun charger-recettes-finales ()
  "Charge les recettes finales complètes."
  
  ;; TODO: Implémenter le chargement des recettes finales
  ;; Pour chaque recette, créer une règle avec ajouter-regle
  
  ;; Exemple de structure (à adapter) :
  ;; Riz pilaf
  ;; (ajouter-regle
  ;;   'R1
  ;;   "Riz pilaf"
  ;;   '((oignon >= 1) (riz_long >= 300) (beurre >= 30) (eau >= 60)
  ;;     (casserole = t)
  ;;     (ou (recette_printemps = t) (recette_ete = t) 
  ;;         (recette_automne = t) (recette_hiver = t))
  ;;     (type_plat = t)
  ;;     (recette_vegetarienne = t))
  ;;   'riz_pilaf
  ;;   :actions '((decremente oignon 1) (decremente riz_long 300)
  ;;              (decremente beurre 30) (decremente eau 60))
  ;;   :profondeur 0
  ;;   :metadata '(:saisons (printemps ete automne hiver)
  ;;               :type plat
  ;;               :vegetarien t))
  
  ;; Céleri rôti
  ;; (ajouter-regle ...)
  
  ;; Gratin dauphinois
  ;; (ajouter-regle ...)
  
  ;; Fond blanc de volaille
  ;; (ajouter-regle ...)
  
  ;; Quiche lorraine
  ;; (ajouter-regle ...)
  
  ;; Tarte tatin
  ;; (ajouter-regle ...)
  
  ;; Crêpe Suzette
  ;; (ajouter-regle ...)
  
  ;; Salade composée
  ;; (ajouter-regle ...)
  
  ;; Boeuf bourguignon
  ;; (ajouter-regle ...)
  
  ;; Lasagne
  ;; (ajouter-regle ...)
  
  ;; Omelette aux champignons
  ;; (ajouter-regle ...)
  
  ;; Pâtes carbonara
  ;; (ajouter-regle ...)
  
  ;; Soupe au potiron
  ;; (ajouter-regle ...)
  
  ;; Cannellonis farcis
  ;; (ajouter-regle ...)
  )

;;; ----------------------------------------------------------------------------
;;; UTILITAIRES DE DONNÉES
;;; ----------------------------------------------------------------------------

(defun lister-toutes-recettes ()
  "Retourne la liste de toutes les recettes disponibles.
   Retour : liste de symboles"
  ;; TODO: Implémenter le listage
  ;; - Parser *base-regles*
  ;; - Extraire les conclusions de profondeur 0
  )

(defun lister-tous-ingredients ()
  "Retourne la liste de tous les ingrédients référencés.
   Retour : liste de symboles"
  ;; TODO: Implémenter le listage
  ;; - Parser toutes les règles
  ;; - Extraire ingrédients des conditions
  ;; - Dédoublonner et trier
  )

(defun lister-tout-materiel ()
  "Retourne la liste de tout le matériel référencé.
   Retour : liste de symboles"
  ;; TODO: Implémenter le listage
  ;; - Parser toutes les règles
  ;; - Extraire matériel des conditions
  ;; - Dédoublonner et trier
  )
