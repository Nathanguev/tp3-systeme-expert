;;;; ============================================================================
;;;; UTILITAIRES WEB - FONCTIONS HELPERS
;;;; ============================================================================
;;;; Description : Fonctions utilitaires manquantes pour l'interface web
;;;;               Ces fonctions extraient les informations des règles
;;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; EXTRACTION D'INGRÉDIENTS ET MATÉRIEL
;;; ----------------------------------------------------------------------------

(defun lister-tous-ingredients ()
  "Retourne la liste de tous les ingrédients référencés dans les recettes.
   Retour : liste de symboles d'ingrédients"
  (let ((ingredients nil))
    (dolist (regle *base-regles*)
      (dolist (condition (regle-conditions regle))
        ;; Format des conditions : (ingredient operateur quantite)
        (let ((ingredient (car condition)))
          ;; Filtrer les symboles qui ne sont pas du matériel
          (unless (or (member ingredient '(four bol rouleau fouet casserole poele plaque))
                      (member ingredient ingredients))
            (push ingredient ingredients)))))
    (sort ingredients #'string< :key #'symbol-name)))

(defun lister-tout-materiel ()
  "Retourne la liste de tout le matériel référencé dans les recettes.
   Retour : liste de symboles de matériel"
  (let ((materiel '(four bol rouleau fouet casserole poele plaque)))
    (sort materiel #'string< :key #'symbol-name)))

;;; ----------------------------------------------------------------------------
;;; TRACE DU RAISONNEMENT
;;; ----------------------------------------------------------------------------

(defun obtenir-trace ()
  "Retourne la trace du raisonnement sous forme structurée.
   Retour : liste de (regle . fait) pour chaque déduction"
  (if (null *regles-declenchees*)
      nil
      (mapcar (lambda (regle-nom)
                (let ((regle (obtenir-regle regle-nom)))
                  (if regle
                      (cons regle-nom (regle-conclusion regle))
                      (cons regle-nom 'inconnu))))
              *regles-declenchees*)))
