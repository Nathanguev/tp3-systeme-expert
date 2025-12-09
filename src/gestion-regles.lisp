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
     - priorite : niveau de priorité (optionnel)
     - profondeur : niveau de composition (0 = recette finale)
     - metadata : informations supplémentaires (saisons, type, etc.)")

(defstruct regle
  "Structure représentant une règle d'ordre 0+."
  nom              ; Symbole identifiant la règle (ex: 'R1)
  description      ; Description textuelle de la règle
  conditions       ; Liste de conditions (triplets: cle operateur valeur)
  conclusion       ; Conclusion de la règle (symbole du fait déduit)
  actions          ; Actions à effectuer (ex: décrémenter ingrédients)
  priorite         ; Priorité d'exécution (optionnel)
  profondeur       ; Profondeur de composition (0+ = final, 1+ = intermédiaire)
  metadata)        ; Métadonnées (saisons, type, végétarien, etc.)

;;; ----------------------------------------------------------------------------
;;; INITIALISATION
;;; ----------------------------------------------------------------------------

(defun initialiser-base-regles ()
  "Initialise ou réinitialise la base de règles."
  (setf *base-regles* nil))

;;; ----------------------------------------------------------------------------
;;; GESTION DES RÈGLES
;;; ----------------------------------------------------------------------------

(defun ajouter-regle (nom description conditions conclusion &key actions (priorite 0) (profondeur 0) metadata)
  "Ajoute une règle à la base de règles.
   Paramètres :
     - nom : symbole identifiant unique
     - description : texte descriptif
     - conditions : liste de conditions
     - conclusion : symbole du fait déduit
     - actions : actions à effectuer (optionnel)
     - priorite : niveau de priorité (défaut: 0)
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
                          :priorite priorite
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
  ;; TODO: Implémenter l'évaluation d'une condition
  ;; - Extraire cle, operateur, valeur
  ;; - Utiliser comparer-fait de base-faits.lisp
  ;; - Gérer les cas spéciaux (OU, ET, NOT)
  (let ((cle (first condition))
        (operateur (second condition))
        (valeur (third condition)))
    (comparer-fait cle operateur valeur))
  )

(defun evaluer-conditions (conditions)
  "Évalue une liste de conditions (conjonction ET).
   Paramètres :
     - conditions : liste de conditions
   Retour : t si toutes les conditions sont vraies, nil sinon"
  ;; TODO: Implémenter l'évaluation multiple
  ;; - Parcourir la liste des conditions
  ;; - Retourner nil dès qu'une condition est fausse
  ;; - Retourner t si toutes sont vraies
  (dolist (condition conditions)
    (unless (evaluer-condition condition)
      (return nil)))
  t
  )

;;; ----------------------------------------------------------------------------
;;; SÉLECTION DES RÈGLES
;;; ----------------------------------------------------------------------------

(defun regles-candidates ()
  "Retourne la liste des règles dont les conditions sont satisfaites.
   Retour : liste de structures REGLE"
  ;; TODO: Implémenter la sélection
  ;; - Parcourir *base-regles*
  ;; - Évaluer les conditions de chaque règle
  ;; - Retourner celles dont conditions sont vraies
  )

(defun regles-pour-but (but)
  "Retourne les règles permettant de déduire un but donné.
   Paramètres :
     - but : symbole du fait à déduire
   Retour : liste de structures REGLE"
  ;; TODO: Implémenter la sélection par but
  ;; - Filtrer *base-regles* sur le champ conclusion
  )

(defun trier-regles-par-priorite (regles)
  "Trie une liste de règles par priorité décroissante.
   Paramètres :
     - regles : liste de structures REGLE
   Retour : liste triée"
  ;; TODO: Implémenter le tri
  ;; - Utiliser sort avec accesseur regle-priorite
  )

(defun trier-regles-par-profondeur (regles)
  "Trie une liste de règles par profondeur croissante.
   Paramètres :
     - regles : liste de structures REGLE
   Retour : liste triée"
  ;; TODO: Implémenter le tri
  ;; - Profondeur 1+ avant profondeur 0 (compositions avant finales)
  )

;;; ----------------------------------------------------------------------------
;;; APPLICATION DES RÈGLES
;;; ----------------------------------------------------------------------------

(defun appliquer-regle (regle)
  "Applique une règle : exécute les actions et ajoute la conclusion.
   Paramètres :
     - regle : structure REGLE à appliquer
   Retour : t si appliquée avec succès, nil sinon"
  ;; TODO: Implémenter l'application
  ;; - Vérifier que les conditions sont toujours vraies
  ;; - Exécuter les actions (décrémenter ingrédients)
  ;; - Ajouter la conclusion à la base de faits
  ;; - Enregistrer la règle déclenchée pour traçabilité
  )

(defun peut-appliquer-regle-p (regle)
  "Vérifie si une règle peut être appliquée (conditions satisfaites).
   Paramètres :
     - regle : structure REGLE
   Retour : t si applicable, nil sinon"
  ;; TODO: Implémenter la vérification
  ;; - Évaluer les conditions
  ;; - Vérifier que la conclusion n'est pas déjà dans la base de faits
  )

;;; ----------------------------------------------------------------------------
;;; AFFICHAGE ET TRAÇABILITÉ
;;; ----------------------------------------------------------------------------

(defun afficher-regle (regle)
  "Affiche une règle de manière formatée.
   Paramètres :
     - regle : structure REGLE à afficher"
  ;; TODO: Implémenter l'affichage
  ;; - Format : SI <conditions> ALORS <conclusion>
  ;; - Inclure les métadonnées pertinentes
  )

(defun afficher-base-regles (&optional (filtrer nil))
  "Affiche toutes les règles de la base.
   Paramètres :
     - filtrer : optionnel, critère de filtrage"
  ;; TODO: Implémenter l'affichage complet
  ;; - Grouper par profondeur ou type
  ;; - Utiliser afficher-regle pour chaque règle
  )

;;; ----------------------------------------------------------------------------
;;; MÉTADONNÉES ET FILTRAGE
;;; ----------------------------------------------------------------------------

(defun regle-respecte-filtres-p (regle)
  "Vérifie si une règle respecte les filtres actifs de la base de faits.
   Paramètres :
     - regle : structure REGLE
   Retour : t si les filtres sont respectés, nil sinon"
  ;; TODO: Implémenter la vérification des filtres
  ;; - Vérifier recette_vegetarienne si applicable
  ;; - Vérifier les saisons actives
  ;; - Vérifier les types de plats demandés
  )

(defun extraire-metadata (regle cle)
  "Extrait une métadonnée d'une règle.
   Paramètres :
     - regle : structure REGLE
     - cle : symbole de la métadonnée recherchée
   Retour : valeur de la métadonnée ou nil"
  ;; TODO: Implémenter l'extraction
  ;; - Chercher dans le champ metadata de la règle
  )
