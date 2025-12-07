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
  
  ;; Charger les recettes finales (profondeur 0) en premier
  (charger-recettes-finales)
  
  ;; Charger les compositions intermédiaires (profondeur 1+) ensuite
  (charger-compositions-intermediaires)
  
  (format t "~D règles chargées avec succès.~%" (length *base-regles*)))

;;; ----------------------------------------------------------------------------
;;; COMPOSITIONS INTERMÉDIAIRES (Profondeur 1+)
;;; ----------------------------------------------------------------------------

(defun charger-compositions-intermediaires ()
  "Charge les compositions intermédiaires (pâtes, sauces, etc.).
   Parse les données depuis *donnees-compositions* et *donnees-compositions-avancees*."
  
  (let ((compteur (+ (length *donnees-recettes*) 1)) ; Commence après les recettes finales
        (nb-compositions 0))
    
    ;; Charger les compositions simples
    (dolist (composition *donnees-compositions*)
      (let* ((nom (first composition))
             (ingredients (second composition))
             (materiel (third composition))
             (vegetarien (sixth composition))
             (conditions nil)
             (actions nil))
        
        ;; Convertir les ingrédients en conditions et actions
        (dolist (ing ingredients)
          (push (list (first ing) '>= (second ing)) conditions)
          (push (list 'decremente (first ing) (second ing)) actions))
        
        ;; Ajouter le matériel en conditions
        (dolist (mat materiel)
          (push (list mat '= t) conditions))
        
        ;; Créer la règle
        (ajouter-regle
          (intern (format nil "R~D" compteur))
          (string-capitalize (substitute #\Space #\_ (string nom)))
          (nreverse conditions)
          nom
          :actions (nreverse actions)
          :profondeur 1
          :metadata (when vegetarien '(:vegetarien t)))
        
        (incf compteur)
        (incf nb-compositions)))
    
    ;; Charger les compositions avancées
    (dolist (composition *donnees-compositions-avancees*)
      (let* ((nom (first composition))
             (ingredients (second composition))
             (materiel (third composition))
             (conditions nil)
             (actions nil)
             ;; Déterminer la profondeur selon les dépendances
             (profondeur (if (or (eq nom 'sauce_bolognaise)) 2 1)))
        
        ;; Convertir les ingrédients en conditions et actions
        (dolist (ing ingredients)
          (push (list (first ing) '>= (second ing)) conditions)
          (push (list 'decremente (first ing) (second ing)) actions))
        
        ;; Ajouter le matériel en conditions
        (dolist (mat materiel)
          (push (list mat '= t) conditions))
        
        ;; Créer la règle
        (ajouter-regle
          (intern (format nil "R~D" compteur))
          (string-capitalize (substitute #\Space #\_ (string nom)))
          (nreverse conditions)
          nom
          :actions (nreverse actions)
          :profondeur profondeur
          :metadata nil)
        
        (incf compteur)
        (incf nb-compositions)))
    
    (format t "~D compositions intermédiaires chargées.~%" nb-compositions)))

;;; ----------------------------------------------------------------------------
;;; RECETTES FINALES (Profondeur 0)
;;; ----------------------------------------------------------------------------

(defun charger-recettes-finales ()
  "Charge les recettes finales complètes.
   Parse les données depuis *donnees-recettes*."
  
  (let ((compteur 1) ; Commence à R1
        (nb-recettes 0))
    
    ;; Parcourir toutes les recettes
    (dolist (recette *donnees-recettes*)
      (let* ((nom (first recette))
             (ingredients (second recette))
             (materiel (third recette))
             (saisons (fourth recette))
             (type-plat (fifth recette))
             (vegetarien (sixth recette))
             (conditions nil)
             (actions nil)
             (metadata nil))
        
        ;; Convertir les ingrédients en conditions et actions
        (dolist (ing ingredients)
          (push (list (first ing) '>= (second ing)) conditions)
          (push (list 'decremente (first ing) (second ing)) actions))
        
        ;; Ajouter le matériel en conditions
        (dolist (mat materiel)
          (push (list mat '= t) conditions))
        
        ;; Construire les métadonnées
        (when saisons
          (setf metadata (append metadata (list :saisons saisons))))
        (when type-plat
          (setf metadata (append metadata (list :type type-plat))))
        (when vegetarien
          (setf metadata (append metadata (list :vegetarien t))))
        
        ;; Créer la règle
        (ajouter-regle
          (intern (format nil "R~D" compteur))
          (string-capitalize (substitute #\Space #\_ (string nom)))
          (nreverse conditions)
          nom
          :actions (nreverse actions)
          :profondeur 0
          :metadata metadata)
        
        (incf compteur)
        (incf nb-recettes)))
    
    (format t "~D recettes chargées.~%" nb-recettes)))

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
  (let ((ingredients nil))
    ;; Parcourir toutes les règles
    (dolist (regle *base-regles*)
      ;; Parcourir les conditions de chaque règle
      (dolist (condition (regle-conditions regle))
        ;; Vérifier si c'est une condition numérique (ingrédient)
        ;; Format: (ingredient >= quantite)
        (when (and (listp condition)
                   (= (length condition) 3)
                   (member (second condition) '(>= <= = > <)))
          ;; Ajouter l'ingrédient s'il n'est pas déjà dans la liste
          (pushnew (first condition) ingredients))))
    ;; Trier alphabétiquement et retourner
    (sort ingredients #'string< :key #'symbol-name)))

(defun lister-tout-materiel ()
  "Retourne la liste de tout le matériel référencé.
   Retour : liste de symboles"
  (let ((materiel nil))
    ;; Parcourir toutes les règles
    (dolist (regle *base-regles*)
      ;; Parcourir les conditions de chaque règle
      (dolist (condition (regle-conditions regle))
        ;; Vérifier si c'est une condition de matériel booléen
        ;; Format: (materiel = t)
        (when (and (listp condition)
                   (= (length condition) 3)
                   (eq (second condition) '=)
                   (eq (third condition) t))
          ;; Ajouter le matériel s'il n'est pas déjà dans la liste
          (pushnew (first condition) materiel))))
    ;; Trier alphabétiquement et retourner
    (sort materiel #'string< :key #'symbol-name)))
