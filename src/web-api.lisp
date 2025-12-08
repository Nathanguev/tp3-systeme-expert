;;;; ============================================================================
;;;; API REST - ENDPOINTS JSON
;;;; ============================================================================
;;;; Description : Endpoints REST pour l'interface web
;;;;               Expose les fonctionnalités du système expert via JSON
;;;; ============================================================================

;;; Dépendance : yason pour la sérialisation JSON
;;; Note : Doit être installé avant : sudo apt install cl-yason
(asdf:load-system :yason)

;;; ----------------------------------------------------------------------------
;;; UTILITAIRES GÉNÉRAUX
;;; ----------------------------------------------------------------------------

(defun split-string-by-char (string char)
  "Divise une chaîne en liste de sous-chaînes selon un caractère séparateur.
   Paramètres :
     - string : chaîne à diviser
     - char : caractère séparateur
   Retour : liste de chaînes"
  (when (and string (> (length string) 0))
    (let ((result nil)
          (current ""))
      (loop for c across string do
        (if (char= c char)
            (when (> (length current) 0)
              (push current result)
              (setf current ""))
            (setf current (concatenate 'string current (string c)))))
      (when (> (length current) 0)
        (push current result))
      (nreverse result))))

;;; ----------------------------------------------------------------------------
;;; UTILITAIRES JSON
;;; ----------------------------------------------------------------------------

(defun reponse-json (data)
  "Envoie une réponse JSON avec le bon content-type.
   Paramètres :
     - data : structure de données à encoder en JSON
   Retour : chaîne JSON"
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (with-output-to-string (s)
    (yason:encode-alist data s)))

(defun reponse-erreur (message &optional (code 400))
  "Envoie une réponse d'erreur JSON.
   Paramètres :
     - message : message d'erreur
     - code : code HTTP (défaut : 400)
   Retour : chaîne JSON avec erreur"
  (setf (hunchentoot:return-code*) code)
  (reponse-json `(("error" . ,message) ("code" . ,code))))

(defun reponse-succes (data &optional (message "Succès"))
  "Envoie une réponse de succès JSON.
   Paramètres :
     - data : données à retourner
     - message : message optionnel
   Retour : chaîne JSON avec succès"
  (reponse-json `(("success" . t) ("message" . ,message) ("data" . ,data))))

;;; ----------------------------------------------------------------------------
;;; API - INGRÉDIENTS
;;; ----------------------------------------------------------------------------

(hunchentoot:define-easy-handler (api-liste-ingredients :uri "/api/ingredients") ()
  "GET /api/ingredients - Retourne la liste de tous les ingrédients disponibles.
   Réponse : {success: true, data: ['tomate', 'oeuf', ...]}"
  (let ((ingredients (lister-tous-ingredients)))
    (reponse-succes (mapcar #'string-downcase 
                            (mapcar #'symbol-name ingredients))
                    "Liste des ingrédients")))

(hunchentoot:define-easy-handler (api-ajouter-ingredient :uri "/api/ingredients/add") 
    (ingredient quantite)
  "POST /api/ingredients/add - Ajoute un ingrédient à la base de faits.
   Paramètres :
     - ingredient : nom de l'ingrédient (string)
     - quantite : quantité disponible (number)
   Réponse : {success: true, message: 'Ingrédient ajouté'}"
  (if (or (null ingredient) (null quantite))
      (reponse-erreur "Paramètres manquants")
      (handler-case
          (let* ((ing-symbole (intern (string-upcase ingredient)))
                 (qte (parse-integer quantite)))
            (ajouter-fait ing-symbole qte)
            (reponse-succes nil (format nil "Ingrédient ~A ajouté (~D)" ingredient qte)))
        (error (e)
          (reponse-erreur (format nil "Erreur: ~A" e))))))

;;; ----------------------------------------------------------------------------
;;; API - MATÉRIEL
;;; ----------------------------------------------------------------------------

(hunchentoot:define-easy-handler (api-liste-materiel :uri "/api/materiel") ()
  "GET /api/materiel - Retourne la liste de tout le matériel disponible.
   Réponse : {success: true, data: ['four', 'bol', ...]}"
  (let ((materiel (lister-tout-materiel)))
    (reponse-succes (mapcar #'string-downcase 
                            (mapcar #'symbol-name materiel))
                    "Liste du matériel")))

(hunchentoot:define-easy-handler (api-ajouter-materiel :uri "/api/materiel/add") 
    (materiel)
  "POST /api/materiel/add - Ajoute du matériel à la base de faits.
   Paramètres :
     - materiel : nom du matériel (string)
   Réponse : {success: true, message: 'Matériel ajouté'}"
  (if (null materiel)
      (reponse-erreur "Paramètre manquant")
      (handler-case
          (let ((mat-symbole (intern (string-upcase materiel))))
            (ajouter-fait mat-symbole t)
            (reponse-succes nil (format nil "Matériel ~A ajouté" materiel)))
        (error (e)
          (reponse-erreur (format nil "Erreur: ~A" e))))))

;;; ----------------------------------------------------------------------------
;;; API - FILTRES
;;; ----------------------------------------------------------------------------

(hunchentoot:define-easy-handler (api-configurer-filtres :uri "/api/filtres") 
    (vegetarien saisons types)
  "POST /api/filtres - Configure les filtres de recherche.
   Paramètres :
     - vegetarien : 'true' ou 'false'
     - saisons : liste séparée par virgules 'printemps,ete'
     - types : liste séparée par virgules 'entree,plat,dessert'
   Réponse : {success: true, message: 'Filtres configurés'}"
  (when (and vegetarien (string-equal vegetarien "true"))
    (ajouter-fait 'vegetarien t))
  
  (when (and saisons (> (length saisons) 0))
    (dolist (saison (split-string-by-char saisons #\,))
      (let ((s (intern (string-upcase (string-trim '(#\Space) saison)))))
        (ajouter-fait s t))))
  
  (when (and types (> (length types) 0))
    (dolist (type (split-string-by-char types #\,))
      (let ((tp (intern (string-upcase (string-trim '(#\Space) type)))))
        (ajouter-fait tp t))))
  
  (reponse-succes nil "Filtres configurés"))

;;; ----------------------------------------------------------------------------
;;; API - RECHERCHE DE RECETTES
;;; ----------------------------------------------------------------------------

(hunchentoot:define-easy-handler (api-lancer-recherche :uri "/api/recherche") ()
  "POST /api/recherche - Lance le chaînage avant et retourne les recettes.
   Réponse : {success: true, data: {recettes: [...], trace: [...]}}"
  (if (null *base-faits*)
      (reponse-erreur "Base de faits vide")
      (handler-case
          (progn
            (chainage-avant)
            (let* ((recettes-trouvees nil)
                   (trace (obtenir-trace)))
              (dolist (fait *base-faits*)
                (let ((regle (obtenir-regle (car fait))))
                  (when (and regle (= (regle-profondeur regle) 0))
                    (push (formater-recette-json regle) recettes-trouvees))))
              (reponse-succes `(("recettes" . ,(nreverse recettes-trouvees))
                               ("trace" . ,(formater-trace-json trace)))
                             (format nil "~D recette(s) trouvée(s)" 
                                    (length recettes-trouvees)))))
        (error (e)
          (reponse-erreur (format nil "Erreur: ~A" e))))))

(hunchentoot:define-easy-handler (api-details-recette :uri "/api/recettes/details") 
    (nom)
  "GET /api/recettes/details?nom=quiche_lorraine - Détails d'une recette.
   Paramètres :
     - nom : nom de la recette (string)
   Réponse : {success: true, data: {nom, description, ingredients, materiel, ...}}"
  (if (null nom)
      (reponse-erreur "Paramètre manquant")
      (let* ((symbole (intern (string-upcase nom)))
             (regle (obtenir-regle symbole)))
        (if regle
            (reponse-succes (formater-recette-json regle) "Détails de la recette")
            (reponse-erreur "Recette non trouvée" 404)))))

;;; ----------------------------------------------------------------------------
;;; API - BASE DE FAITS
;;; ----------------------------------------------------------------------------

(hunchentoot:define-easy-handler (api-base-faits :uri "/api/faits") ()
  "GET /api/faits - Retourne la base de faits actuelle.
   Réponse : {success: true, data: [{cle, valeur}, ...]}"
  (let ((faits (mapcar (lambda (fait)
                         `(("cle" . ,(string-downcase (symbol-name (car fait))))
                           ("valeur" . ,(cdr fait))))
                       *base-faits*)))
    (reponse-succes faits "Base de faits actuelle")))

(hunchentoot:define-easy-handler (api-reinitialiser :uri "/api/reinitialiser") ()
  "POST /api/reinitialiser - Réinitialise le système (faits + moteur).
   Réponse : {success: true, message: 'Système réinitialisé'}"
  (initialiser-base-faits)
  (reinitialiser-moteur)
  (reponse-succes nil "Système réinitialisé"))

;;; ----------------------------------------------------------------------------
;;; API - TRACE ET EXPLICABILITÉ
;;; ----------------------------------------------------------------------------

(hunchentoot:define-easy-handler (api-trace :uri "/api/trace") ()
  "GET /api/trace - Retourne la trace du raisonnement.
   Réponse : {success: true, data: [{etape, regle, fait}, ...]}"
  (let ((trace (obtenir-trace)))
    (reponse-succes (formater-trace-json trace) "Trace du raisonnement")))

(hunchentoot:define-easy-handler (api-statistiques :uri "/api/statistiques") ()
  "GET /api/statistiques - Retourne les statistiques du système.
   Réponse : {success: true, data: {nb_recettes, nb_ingredients, nb_regles_declenchees, ...}}"
  (let* ((nb-recettes (length (remove-if-not (lambda (r) (= (regle-profondeur r) 0))
                                             *base-regles*)))
         (nb-ingredients (length (lister-tous-ingredients)))
         (nb-faits (length *base-faits*))
         (nb-trace (length (obtenir-trace))))
    (reponse-succes `(("nb_recettes" . ,nb-recettes)
                     ("nb_ingredients" . ,nb-ingredients)
                     ("nb_faits" . ,nb-faits)
                     ("nb_regles_declenchees" . ,nb-trace))
                   "Statistiques du système")))

;;; ----------------------------------------------------------------------------
;;; UTILITAIRES DE FORMATAGE
;;; ----------------------------------------------------------------------------

(defun formater-recette-json (regle)
  "Formate une règle (recette) en structure JSON.
   Paramètres :
     - regle : structure regle
   Retour : alist pour cl-json"
  (let* ((metadata (regle-metadata regle))
         (vegetarien (cdr (assoc 'vegetarien metadata)))
         (type-plat (cdr (assoc 'type metadata)))
         (saisons (cdr (assoc 'saisons metadata))))
    `(("nom" . ,(string-downcase (symbol-name (regle-nom regle))))
      ("description" . ,(or (regle-description regle) ""))
      ("type" . ,(if type-plat (string-downcase (symbol-name type-plat)) ""))
      ("vegetarien" . ,(if vegetarien t :false))
      ("saisons" . ,(if saisons 
                        (mapcar (lambda (s) (string-downcase (symbol-name s))) saisons)
                        nil)))))

(defun formater-trace-json (trace)
  "Formate la trace d'inférence en structure JSON.
   Paramètres :
     - trace : liste de (regle . fait)
   Retour : liste d'alists pour cl-json"
  (let ((resultat nil)
        (etape 1))
    (dolist (entree trace)
      (push `(("etape" . ,etape)
              ("regle" . ,(string-downcase (symbol-name (car entree))))
              ("fait" . ,(string-downcase (symbol-name (cdr entree)))))
            resultat)
      (incf etape))
    (nreverse resultat)))
