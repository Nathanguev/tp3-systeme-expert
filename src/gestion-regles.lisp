;;;; ============================================================================
;;;; BASE DE RÈGLES - GESTION DES CONNAISSANCES GÉNÉRALES
;;;; ============================================================================
;;;; Description : Gestion de la base de règles du système expert
;;;;               Représentation des règles d'ordre 0+
;;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; STRUCTURES DE DONNÉES
;;; ----------------------------------------------------------------------------

(defvar *base-regles* nil
  "Base de règles globale contenant toutes les règles du système.
   Structure : liste de structures REGLE
   Chaque règle contient :
     - nom : identifiant unique de la règle
     - conditions : liste de conditions à vérifier
     - conclusion : fait à ajouter/modifier si conditions vraies
     - profondeur : niveau de composition (0 = recette finale)
     - metadata : informations supplémentaires (saisons, type, etc.)")

(defstruct regle
  "Structure représentant une règle d'ordre 0+."
  nom              ; Symbole identifiant la règle (ex: 'R1)
  description      ; Description textuelle de la règle
  conditions       ; Liste de conditions (triplets: cle operateur valeur)
  conclusion       ; Conclusion de la règle (symbole du fait déduit)
  actions          ; Actions à effectuer (decremente-fait, incremente-fait)
  profondeur       ; Profondeur de composition
  metadata)        ; Métadonnées (saisons, type de plat, végétarien)

;;; ----------------------------------------------------------------------------
;;; INITIALISATION
;;; ----------------------------------------------------------------------------

(defun initialiser-base-regles ()
  "Initialise ou réinitialise la base de règles."
  (setf *base-regles* nil))

;;; ----------------------------------------------------------------------------
;;; GESTION DES RÈGLES
;;; ----------------------------------------------------------------------------

(defun ajouter-regle (nom description conditions conclusion &key actions (profondeur 0) metadata)
  "Ajoute une règle à la base de règles.
   Paramètres :
     - nom : symbole identifiant unique
     - description : texte descriptif
     - conditions : liste de conditions
     - conclusion : symbole du fait déduit
     - actions : actions à effectuer (optionnel)
     - profondeur : niveau de composition (défaut: 0)
     - metadata : liste de propriétés supplémentaires
   Retour : la règle créée"

  ;; Vérifier si une règle avec ce nom existe déjà
  (let ((regle-existante (obtenir-regle nom)))
    (when regle-existante
      (warn "La règle ~A existe déjà et sera remplacée." nom)
      (supprimer-regle nom)))

  ;; Créer la nouvelle règle
  (let ((nouvelle-regle (make-regle
                          :nom nom
                          :description description
                          :conditions conditions
                          :conclusion conclusion
                          :actions actions
                          :profondeur profondeur
                          :metadata metadata)))

    ;; Ajouter à la base de règles
    (push nouvelle-regle *base-regles*)
    nouvelle-regle))

(defun obtenir-regle (nom)
  "Récupère une règle par son nom.
   Paramètres :
     - nom : symbole identifiant la règle
   Retour : structure REGLE ou nil"
  (find nom *base-regles* :key #'regle-nom :test #'eq))

(defun supprimer-regle (nom)
  "Supprime une règle de la base de règles.
   Paramètres :
     - nom : symbole identifiant la règle
   Retour : t si supprimée, nil sinon"
  (let ((ancienne-taille (length *base-regles*)))
    (setf *base-regles* (remove nom *base-regles* :key #'regle-nom :test #'eq))
    (/= ancienne-taille (length *base-regles*))))

;;; ----------------------------------------------------------------------------
;;; ÉVALUATION DES CONDITIONS
;;; ----------------------------------------------------------------------------

(defun evaluer-condition (condition)
  "Évalue une condition simple.
   Paramètres :
     - condition : triplet (cle operateur valeur)
   Retour : t si la condition est vraie, nil sinon"
  (let ((cle (first condition))
        (operateur (second condition))
        (valeur (third condition)))
    (comparer-fait cle operateur valeur)))

(defun evaluer-conditions (conditions)
  "Évalue une liste de conditions (conjonction ET).
   Paramètres :
     - conditions : liste de conditions
   Retour : t si toutes les conditions sont vraies, nil sinon"
  (dolist (condition conditions)
    (unless (evaluer-condition condition)
      (return-from evaluer-conditions nil)))
  t)

;;; ----------------------------------------------------------------------------
;;; SÉLECTION DES RÈGLES
;;; ----------------------------------------------------------------------------

(defun regles-candidates ()
  "Retourne la liste des règles dont les conditions sont satisfaites.
   Retour : liste de structures REGLE"
  (let ((candidates '()))
    (dolist (regle *base-regles*)
      (when (evaluer-conditions (regle-conditions regle))
        (push regle candidates)))
    candidates))

(defun regles-pour-but (but)
  "Retourne les règles permettant de déduire un but donné.
   Paramètres :
     - but : symbole du fait à déduire
   Retour : liste de structures REGLE"
  (let ((resultat '()))
    (dolist (regle *base-regles*)
      (when (eq (regle-conclusion regle) but)
        (push regle resultat)))
    resultat))

;;; ----------------------------------------------------------------------------
;;; APPLICATION DES RÈGLES
;;; ----------------------------------------------------------------------------

(defun appliquer-regle (regle)
  "Applique une règle : exécute les actions et ajoute la conclusion.
   Paramètres :
     - regle : structure REGLE à appliquer
   Retour : t si appliquée avec succès, nil sinon"
  (let ((conditions (regle-conditions regle))
        (conclusion (regle-conclusion regle))
        (actions (regle-actions regle)))
    (when (evaluer-conditions conditions)
      (dolist (action actions)
        (apply (car action) (cdr action)))
        (ajouter-fait 'ingredients conclusion 1)
      t)))

(defun desappliquer-regle (regle)
  "Désapplique une règle : exécute les actions et retire la conclusion.
   Paramètres :
     - regle : structure REGLE à désappliquer
   Retour : t si désappliquée avec succès, nil sinon"
  (let ((conditions (regle-conditions regle))
        (conclusion (regle-conclusion regle))
        (actions (regle-actions regle)))
    (when (> (cdr (assoc conclusion (cadr (assoc 'ingredients *base-faits*)))) 0)

      (dolist (action actions)
        (if (eq (car action) 'decremente-fait)
            (apply 'incremente-fait (cdr action))
            (apply 'decremente-fait (cdr action))))
      (decremente-fait conclusion 1)
      t)))

;;; ----------------------------------------------------------------------------
;;; MÉTADONNÉES ET FILTRAGE
;;; ----------------------------------------------------------------------------

(defun regle-respecte-filtres-p (regle)
  "Vérifie si une règle respecte les filtres actifs de la base de faits.
   Paramètres :
     - regle : structure REGLE
   Retour : t si les filtres sont respectés, nil sinon"
  (let ((metadata (regle-metadata regle)))
    (dolist (filtre metadata)
      (cond

  ((or (eq filtre ':saisons) (eq filtre ':type) (eq filtre t))
   t)

  ((eq filtre ':vegetarien)
   (unless (obtenir-fait 'vegetarien)
     (return-from regle-respecte-filtres-p nil))
   t)

  ((listp filtre)
   (dolist (sous-filtre filtre)
     (unless (obtenir-fait sous-filtre)
       (return-from regle-respecte-filtres-p nil)))
   t)

  (t (unless (obtenir-fait filtre)
     (return-from regle-respecte-filtres-p nil)))))
   t))
