;;;; ============================================================================
;;;; BASE DE FAITS - GESTION DES CONNAISSANCES COURANTES
;;;; ============================================================================
;;;; Description : Gestion de la base de faits (ingrédients, matériel, filtres)
;;;;               Structure de données pour l'ordre 0+
;;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; STRUCTURES DE DONNÉES
;;; ----------------------------------------------------------------------------

(defvar *base-faits* ((ingredients nil)
                      (materiel nil)
                      (filtres nil))
  "Base de faits globale contenant tous les faits courants du système.
   Structure : liste d'association (clé . valeur)
   - Pour ingrédients : (nom-ingredient . quantite-disponible)
   - Pour matériel : (nom-materiel . t/nil)
   - Pour filtres : (nom-filtre . t/nil)")

(defvar *historique-faits* nil
  "Historique des modifications de la base de faits pour traçabilité.
   Structure : liste de triplets (action fait ancienne-valeur)")

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
     - cle : symbole identifiant le fait
     - valeur : valeur associée (nombre ou booléen)
   Retour : la valeur ajoutée"
  ;; TODO: Implémenter l'ajout avec historique
  ;; - Vérifier si le fait existe déjà
  ;; - Enregistrer l'ancienne valeur dans l'historique
  ;; - Ajouter ou mettre à jour dans *base-faits*
  (if (obtenir-fait cle)
    (progn
      (push (list 'modifier-fait cle (obtenir-fait cle)) *historique-faits*)
      (modifier-fait cle valeur)
    )
    (cond
          ((eq type "ingredients") (push (cons cle valeur) (cdr (assoc 'ingredients *base-faits*))) (format t "Ingrédient ~A ajouté avec quantité ~A.~%" cle valeur))
          ((eq type "materiel") (push (cons cle valeur) (cdr (assoc 'materiel *base-faits*))) (format t "Matériel ~A ajouté avec état ~A.~%" cle valeur))
          ((eq type "filtres") (push (cons cle valeur) (cdr (assoc 'filtres *base-faits*))) (format t "Filtre ~A ajouté avec état ~A.~%" cle valeur))
    ))

)


(defun obtenir-fait (cle)
  "Récupère la valeur d'un fait dans la base de faits.
   Paramètres :
     - cle : symbole identifiant le fait
   Retour : valeur du fait ou nil si non trouvé"
  ;; TODO: Implémenter la récupération
  ;; - Chercher dans *base-faits* avec assoc
  ;; - Retourner la valeur (cdr) ou nil
  (let ((ingredient (assoc cle (cdr (assoc 'ingredients *base-faits*))))
        (materiel (assoc cle (cdr (assoc 'materiel *base-faits*))))
        (filtre (assoc cle (cdr (assoc 'filtres *base-faits*)))))
    (cond
      (ingredient (cdr ingredient))
      (materiel (cdr materiel))
      (filtre (cdr filtre))
      (t nil)))
)


(defun modifier-fait (cle nouvelle-valeur)
  "Modifie la valeur d'un fait existant (utilisé par les règles 0+).
   Paramètres :
     - cle : symbole identifiant le fait
     - nouvelle-valeur : nouvelle valeur à assigner
   Retour : la nouvelle valeur ou nil si le fait n'existe pas"
  ;; TODO: Implémenter la modification avec historique
  ;; - Vérifier l'existence du fait
  ;; - Sauvegarder l'ancienne valeur
  ;; - Mettre à jour la valeur
)


(defun supprimer-fait (cle)
  "Supprime un fait de la base de faits.
   Paramètres :
     - cle : symbole identifiant le fait
   Retour : t si supprimé, nil si non trouvé"
  ;; TODO: Implémenter la suppression avec historique
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
  ;; TODO: Implémenter la décrémentation
  ;; - Vérifier que le fait existe et est numérique
  ;; - Vérifier que la soustraction est possible (>= 0)
  ;; - Décrémenter et enregistrer dans l'historique
  )

(defun incremente-fait (cle quantite)
  "Incrémente la valeur numérique d'un fait.
   Paramètres :
     - cle : symbole identifiant le fait
     - quantite : quantité à ajouter
   Retour : nouvelle valeur ou nil si impossible"
  ;; TODO: Implémenter l'incrémentation
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
  ;; TODO: Implémenter la comparaison
  ;; - Récupérer la valeur du fait
  ;; - Appliquer l'opérateur de comparaison
  ;; - Gérer les cas nil et les types non numériques
  )

;;; ----------------------------------------------------------------------------
;;; AFFICHAGE ET TRAÇABILITÉ
;;; ----------------------------------------------------------------------------

(defun afficher-base-faits (&optional (filtrer nil))
  "Affiche la base de faits courante de manière formatée.
   Paramètres :
     - filtrer : optionnel, type de faits à afficher (:ingredients :materiel :filtres)"
  ;; TODO: Implémenter l'affichage organisé
  ;; - Grouper par catégorie (ingrédients, matériel, filtres)
  ;; - Formatter proprement pour l'utilisateur

  )

(defun afficher-historique ()
  "Affiche l'historique des modifications de la base de faits."
  ;; TODO: Implémenter l'affichage de l'historique
  ;; - Parcourir *historique-faits*
  ;; - Afficher chaque modification avec son contexte
  )

;;; ----------------------------------------------------------------------------
;;; SAUVEGARDE ET RESTAURATION
;;; ----------------------------------------------------------------------------

(defun sauvegarder-etat ()
  "Sauvegarde l'état actuel de la base de faits.
   Retour : copie de la base de faits"
  ;; TODO: Implémenter la sauvegarde
  ;; - Créer une copie profonde de *base-faits*
  )

(defun restaurer-etat (etat)
  "Restaure la base de faits à un état sauvegardé.
   Paramètres :
     - etat : état sauvegardé précédemment"
  ;; TODO: Implémenter la restauration
  ;; - Remplacer *base-faits* par l'état fourni
  )
