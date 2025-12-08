;;;; ============================================================================
;;;; BASE DE FAITS - GESTION DES CONNAISSANCES COURANTES
;;;; ============================================================================
;;;; Description : Gestion de la base de faits (ingrédients, matériel, filtres)
;;;;               Structure de données pour l'ordre 0+
;;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; STRUCTURES DE DONNÉES
;;; ----------------------------------------------------------------------------

(defvar *base-faits* '((ingredients ())
                      (materiel ())
                      (filtres ()))
  "Base de faits globale contenant tous les faits courants du système.
   Structure : liste d'association de catégories, chaque catégorie contenant une liste d'association (clé . valeur)
   - Pour ingrédients : (nom-ingredient . quantite-disponible)
   - Pour matériel : (nom-materiel . t/nil)
   - Pour filtres : (nom-filtre . t/nil)")

(defvar *historique-faits* '()
  "Historique des modifications de la base de faits pour traçabilité.
   Structure : liste de triplets (action fait ancienne-valeur)")

(defvar *sauvegarde-faits* '()
  "Sauvegarde temporaire de la base de faits pour restauration ultérieure.")

(defvar *sauvegarde-historique* '()
  "Sauvegarde temporaire de l'historique des faits pour restauration ultérieure.")

;;; ----------------------------------------------------------------------------
;;; INITIALISATION
;;; ----------------------------------------------------------------------------

(defun initialiser-base-faits ()
  "Initialise ou réinitialise la base de faits à un état vide.
   Nettoie également l'historique."
  (setf *base-faits* nil)
  (setf *historique-faits* nil))

;;; ----------------------------------------------------------------------------
;;; OPÉRATIONS DE BASE SUR LES FAITS
;;; ----------------------------------------------------------------------------

(defun ajouter-fait (type cle valeur)
  "Ajoute un fait à la base de faits ou met à jour sa valeur.
   Paramètres :
     - type : type de fait ('ingredients, 'materiel, ou 'filtres)
     - cle : symbole identifiant le fait
     - valeur : valeur associée (nombre ou booléen)
   Retour : la valeur ajoutée
   Note de migration : la signature a changé, il faut désormais fournir le type en premier argument."

  (if (obtenir-fait cle)
      (modifier-fait cle valeur)
      (let ((categorie (assoc type *base-faits*)))
        (when categorie
            (progn
              (push (cons cle valeur) (cadr categorie))
              (format t "~A ajouté avec la valeur ~A.~%" cle valeur)
              (push (list 'ajouter-fait cle nil) *historique-faits*)
              valeur)
        )
      )
    )
)



(defun obtenir-fait (cle)
  "Récupère la valeur d'un fait dans la base de faits.
   Paramètres :
     - cle : symbole identifiant le fait
   Retour : valeur du fait ou nil si non trouvé"

  (some (lambda (type)
          (cdr (assoc cle (cadr (assoc type *base-faits*)))))
        '(ingredients materiel filtres))
)


(defun modifier-fait (cle nouvelle-valeur)
  "Modifie la valeur d'un fait existant (utilisé par les règles 0+).
   Paramètres :
     - cle : symbole identifiant le fait
     - nouvelle-valeur : nouvelle valeur à assigner
   Retour : la nouvelle valeur ou nil si le fait n'existe pas"

  (let ((fait (obtenir-fait cle)))
    (if fait
      (progn
        (push (list 'modifier-fait cle (cdr fait)) *historique-faits*)
        (setf (cdr fait) nouvelle-valeur)
        nouvelle-valeur)
    nil)
  )
)

(defun supprimer-fait (cle)
  "Supprime un fait de la base de faits.
   Paramètres :
     - cle : symbole identifiant le fait
   Retour : t si supprimé, nil si non trouvé"

  (let ((fait (obtenir-fait cle)))
    (if fait
      (progn
        (dolist (type '(ingredients materiel filtres))
          (let ((categorie (assoc type *base-faits*)))
            (when categorie
              (setf (cadr categorie)
                    (remove fait (cadr categorie))))))
        (push (list 'supprimer-fait cle (cdr fait)) *historique-faits*)
        t)
      nil
    )
  )
)

;;; ----------------------------------------------------------------------------
;;; OPÉRATIONS NUMÉRIQUES (pour ordre 0+)
;;; ----------------------------------------------------------------------------

(defun decremente-fait (cle quantite)
  "Décrémente la valeur numérique d'un fait (utile pour consommer ingrédients).
   Paramètres :
     - cle : symbole identifiant le fait
     - quantite : quantité à soustraire
   Retour : nouvelle valeur ou nil si impossible"

  (let ((fait (obtenir-fait cle)))
    (if (and fait (numberp (cdr fait)) (>= (cdr fait) quantite))
      (progn
        (push (list 'decremente-fait cle (cdr fait)) *historique-faits*)
        (setf (cdr fait) (- (cdr fait) quantite))
        (cdr fait))
      nil)
  )
)

(defun incremente-fait (cle quantite)
  "Incrémente la valeur numérique d'un fait.
   Paramètres :
     - cle : symbole identifiant le fait
     - quantite : quantité à ajouter
   Retour : nouvelle valeur ou nil si impossible"

  (let ((fait (obtenir-fait cle)))
    (if (and fait (numberp (cdr fait)))
      (progn
        (push (list 'incremente-fait cle (cdr fait)) *historique-faits*)
        (setf (cdr fait) (+ (cdr fait) quantite))
        (cdr fait))
      nil)
  )
)

;;; ----------------------------------------------------------------------------
;;; COMPARAISONS (pour conditions des règles)
;;; ----------------------------------------------------------------------------

(defun comparer-fait (cle operateur valeur)
  "Compare un fait avec une valeur selon un opérateur.
   Paramètres :
     - cle : symbole identifiant le fait
     - operateur : '>=, '<=, '=, '>, '<
     - valeur : valeur de comparaison
   Retour : t si la comparaison est vraie, nil sinon"

  (let ((fait (obtenir-fait cle)))
    (if fait
      (cond
        ((and (numberp (cdr fait)) (numberp valeur)) (funcall operateur (cdr fait) valeur))
        ((eq operateur '=) (equal (cdr fait) valeur))
      )
    )
  )
)
;;; ----------------------------------------------------------------------------
;;; AFFICHAGE ET TRAÇABILITÉ
;;; ----------------------------------------------------------------------------

(defun afficher-base-faits (&optional (filtrer nil))
  "Affiche la base de faits courante de manière formatée.
   Paramètres :
     - filtrer : optionnel, type de faits à afficher (:ingredients :materiel :filtres)"
  (let ((categories '((ingredients . "Ingrédients")
                      (materiel . "Matériel")
                      (filtres . "Filtres"))))
    (dolist (cat categories)
      (when (or (null filtrer) (eq filtrer (car cat)))
        (format t "~%--- ~A ---~%" (cdr cat))
        (dolist (fait (cadr (assoc (car cat) *base-faits*)))
          (format t "~A : ~A~%" (car fait) (cdr fait))))))
)

(defun afficher-historique ()
  "Affiche l'historique des modifications de la base de faits."

  (format t "~%Historique des modifications :~%")
  (dolist (modification *historique-faits*)
    (format t "Action : ~A, Fait : ~A, Ancienne valeur : ~A~%"
            (first modification) (second modification) (third modification)))
  (when (null *historique-faits*)
    (format t "Aucune modification enregistrée.~%")
  ))

;;; ----------------------------------------------------------------------------
;;; SAUVEGARDE ET RESTAURATION
;;; ----------------------------------------------------------------------------

(defun sauvegarder-etat ()
  "Sauvegarde l'état actuel de la base de faits.
   Retour : copie de la base de faits"

  (let ((nb-sauvegarde (+ (length *sauvegarde-faits*) 1)))
    (push (list nb-sauvegarde (copy-tree *base-faits*)) *sauvegarde-faits*)
    (push (list nb-sauvegarde (copy-tree *historique-faits*)) *sauvegarde-historique*)
    (format t "État de la base de faits sauvegardé avec l'index ~A.~%" nb-sauvegarde)
    (copy-tree *base-faits*)
  )
  )

(defun restaurer-etat (index-etat)
  "Restaure la base de faits à un état sauvegardé.
   Paramètres :
     - index-etat : index numérique de l'état sauvegardé à restaurer"

  (let ((etat-sauvegarde (assoc index-etat *sauvegarde-faits*))
        (etat-historique (assoc index-etat *sauvegarde-historique*)))
    (if (and etat-sauvegarde etat-historique)
      (progn
        (setf *base-faits* (copy-tree (cadr etat-sauvegarde)))
        (setf *historique-faits* (copy-tree (cadr etat-historique)))
        (format t "Base de faits restaurée à l'état ~A.~%" index-etat)
        (copy-tree (cadr etat-sauvegarde)))
    nil)
))

