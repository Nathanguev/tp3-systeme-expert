;;;; ============================================================================
;;;; MOTEUR D'INFÉRENCE - CHAÎNAGE AVANT ET ARRIÈRE
;;;; ============================================================================
;;;; Description : Implémentation des moteurs d'inférence d'ordre 0+
;;;;               - Chaînage avant (forward chaining)
;;;;               - Chaînage arrière (backward chaining)
;;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; VARIABLES GLOBALES DE TRAÇABILITÉ
;;; ----------------------------------------------------------------------------

(defvar *regles-declenchees* nil
  "Liste des règles déclenchées pendant l'inférence pour traçabilité.")

(defvar *solutions-trouvees* nil
  "Liste pour stocker toutes les combinaisons de menus valides trouvées")

(defvar *trace-inference-avant* nil
  "Stocke la copie de *regles-declenchees* au moment où une solution est trouvée.")

(defvar *trace-echecs* nil
  "Liste des tentatives échouées pendant le chaînage arrière.")

(defvar *profondeur-max* 10
  "Profondeur maximale pour éviter les boucles infinies.")

;;; ----------------------------------------------------------------------------
;;; CHAÎNAGE AVANT (Forward Chaining)
;;; ----------------------------------------------------------------------------

(defun chainage-avant ()
"Moteur d'inférence à chaînage avant (stratégie en PROFONDEUR).
   Principe :
     1. Trouver les règles applicables.
     2. En choisir une, l'appliquer (descendre).
     3. Revenir en arrière (backtrack) pour essayer les autres.
   Retour : liste des faits déduits (conclusions)"

  (let ((candidates (regles-candidates)))
    (setf candidates (remove-if (lambda (r) 
      (member (regle-nom r) *regles-declenchees*)) candidates))

    (when (null candidates)
      (return-from chainage-avant))

    (setf candidates (sort candidates (lambda (r1 r2)
      (< (regle-profondeur r1) (regle-profondeur r2)))))

    (dolist (regle candidates)
      (appliquer-regle regle)
      (push (regle-nom regle) *regles-declenchees*)
      (pushnew regle *solutions-trouvees*)
      (chainage-avant)
      (pop *regles-declenchees*)
      (desappliquer-regle regle))))

;;; ----------------------------------------------------------------------------
;;; CHAÎNAGE ARRIÈRE (Backward Chaining)
;;; ----------------------------------------------------------------------------

(defun chainage-arriere (but)
  "Moteur d'inférence à chaînage arrière (goal-driven).
   Principe :
     1. Partir d'un but à prouver
     2. Chercher les règles concluant ce but
     3. Vérifier récursivement les conditions de ces règles
     4. Retourner vrai si le but peut être prouvé
   Paramètres :
     - but : symbole du fait à prouver
   Retour : t si le but est prouvé, nil sinon"
  (setf *regles-declenchees* nil)
  (setf *trace-echecs* nil)
  (prouver-but-recursif but 0))

(defun prouver-but-recursif (but profondeur)
  "Fonction auxiliaire récursive pour le chaînage arrière.
   Paramètres :
     - but : fait à prouver
     - profondeur : profondeur courante de récursion
   Retour : t si prouvé, nil sinon"

  ;; Vérifier la profondeur maximale et si le but est déjà prouvé
  (when (or (>= profondeur *profondeur-max*) (obtenir-fait but))
    (return-from prouver-but-recursif (obtenir-fait but)))

  ;; Chercher les règles qui concluent ce but
  (let ((regles-but (regles-pour-but but)))
    (when (and (null regles-but) (zerop profondeur))
      (push (list :but but :raison "Aucune règle ne conclut ce fait") *trace-echecs*)
      (return-from prouver-but-recursif nil))

    ;; Essayer chaque règle
    (dolist (regle regles-but)
      (let ((conditions-manquantes nil))
        ;; Vérifier toutes les conditions
        (dolist (condition (regle-conditions regle))
          (unless (or (obtenir-fait (first condition))
                     (prouver-but-recursif (first condition) (1+ profondeur)))
            (push (list (first condition) (third condition)) conditions-manquantes)))

        ;; Si toutes les conditions sont prouvées, appliquer la règle
        (if (null conditions-manquantes)
            (progn
              (appliquer-regle regle)
              (push (cons (regle-nom regle) but) *regles-declenchees*)
              (return-from prouver-but-recursif t))
            ;; Sinon, enregistrer l'échec (seulement pour profondeur 0)
            (when (zerop profondeur)
              (push (list :but but
                         :regle (regle-nom regle)
                         :conditions-manquantes (nreverse conditions-manquantes))
                    *trace-echecs*))))))
  nil)

;;; ----------------------------------------------------------------------------
;;; TRAÇABILITÉ ET EXPLICATION
;;; ----------------------------------------------------------------------------

(defun obtenir-trace-inference ()
  "Retourne la trace complète du raisonnement effectué.
   Retour : liste structurée de l'inférence"
  (setf *trace-inference-avant* nil)
  (dolist (solution *solutions-trouvees*)
    (let ((nom (regle-nom solution))
          (conclusion (regle-conclusion solution))
          (conditions (regle-conditions solution)))
      (push (list nom conditions conclusion) *trace-inference-avant*)))
  *trace-inference-avant*)

(defun format-liste-ingredients (conditions)
  (let (conditions_lisible '())
    (dolist (condition conditions)
      (unless (eq (third condition) 'T)
        (push (list (first condition) (third condition)) conditions_lisible)))
  conditions_lisible))

(defun afficher-trace-inference ()
  "Affiche la trace du raisonnement de manière formatée."
  (let ((trace (obtenir-trace-inference))
        (etape 1))
    
    (format t "~%=== TRACE DU RAISONNEMENT ===~%~%")
    
    (if (null trace)
        (format t "  Aucune trace disponible. ~%")

        (dolist (ligne trace)
          (let ((nom (first ligne))
                (conditions (second ligne))
                (conclusion (third ligne)))
            
            ;; LIGNE 1 : Le résumé de l'action
            (format t "  ~A Étape ~D : Règle ~A -> Fait ~A~%" 
                    (format-ok) etape nom conclusion)
            
            ;; LIGNE 2 : Le détail des ingrédients utilisés
            (format t "       Used : [ ~A ]~%~%" 
                    (format-liste-ingredients conditions))
            (incf etape))))))

;;; ----------------------------------------------------------------------------
;;; UTILITAIRES
;;; ----------------------------------------------------------------------------

(defun reinitialiser-moteur ()
  "Réinitialise le moteur d'inférence (nettoie la traçabilité)."
  (setf *regles-declenchees* nil)
  (setf *solutions-trouvees* nil)
  (setf *trace-inference-avant* nil)
  (setf *trace-echecs* nil))
