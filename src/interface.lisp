;;;; ============================================================================
;;;; INTERFACE UTILISATEUR - INTERACTION ET VISUALISATION
;;;; ============================================================================
;;;; Description : Interface utilisateur pour l'interaction avec le système expert
;;;;               Collecte des faits, affichage des résultats, explicabilité
;;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; INTERFACE PRINCIPALE
;;; ----------------------------------------------------------------------------

(defun interface-principale ()
  "Point d'entrée de l'interface utilisateur.
   Menu principal permettant de naviguer entre les fonctionnalités."
  (loop
    (format t "~%")
    (format t "=== MENU PRINCIPAL ===~%")
    (format t "1. Saisir les ingrédients disponibles~%")
    (format t "2. Saisir le matériel disponible~%")
    (format t "3. Configurer les filtres (végétarien, saisons, type)~%")
    (format t "4. Lancer la recherche de recettes (chaînage avant)~%")
    (format t "5. Vérifier une recette spécifique (chaînage arrière)~%")
    (format t "6. Afficher la base de faits actuelle~%")
    (format t "7. Afficher la trace du raisonnement~%")
    (format t "8. Afficher les statistiques~%")
    (format t "9. Réinitialiser le système~%")
    (format t "0. Quitter~%")
    (format t "~%Votre choix : ")
    (finish-output)
    
    ;; Lire le choix utilisateur
    (let* ((input (read-line))
           (choix (parse-integer input :junk-allowed t)))
      
      (cond
        ;; Quitter
        ((eql choix 0)
         (format t "~%Au revoir !~%")
         (return))
        
        ;; Saisir ingrédients
        ((eql choix 1)
         (saisir-ingredients))
        
        ;; Saisir matériel
        ((eql choix 2)
         (saisir-materiel))
        
        ;; Configurer filtres
        ((eql choix 3)
         (configurer-filtres))
        
        ;; Lancer recherche
        ((eql choix 4)
         (lancer-recherche-recettes))
        
        ;; Vérifier recette spécifique
        ((eql choix 5)
         (verifier-recette-specifique))
        
        ;; Afficher base de faits
        ((eql choix 6)
         (afficher-base-faits)
         (pause))
        
        ;; Afficher trace
        ((eql choix 7)
         (afficher-trace-complete))
        
        ;; Afficher statistiques
        ((eql choix 8)
         (afficher-statistiques))
        
        ;; Réinitialiser
        ((eql choix 9)
         (format t "~%Réinitialisation du système...~%")
         (initialiser-base-faits)
         (reinitialiser-moteur)
         (format t "Système réinitialisé.~%"))
        
        ;; Choix invalide
        (t
         (format t "~%Choix invalide. Veuillez entrer un nombre entre 0 et 9.~%")
         (pause))))))

;;; ----------------------------------------------------------------------------
;;; SAISIE DES INGRÉDIENTS
;;; ----------------------------------------------------------------------------

(defun saisir-ingredients ()
  "Interface de saisie des ingrédients disponibles.
   Permet à l'utilisateur d'entrer les ingrédients et leurs quantités."
  
  (format t "~%=== SAISIE DES INGRÉDIENTS ===~%~%")
  
  (let ((ingredients (proposer-ingredients-connus)))
    ;; Vérifier si des ingrédients sont disponibles
    (when (null ingredients)
      (format t "Aucun ingrédient disponible. Chargez d'abord les recettes.~%")
      (pause)
      (return-from saisir-ingredients))
    ;; Afficher le nombre et la liste des ingrédients
    (format t "~D ingrédients disponibles dans le système.~%~%" (length ingredients))
    (format t "Liste des ingrédients :~%")
    (loop for ing in ingredients
          for compteur from 1
          do (format t "  ~2D. ~A~%" compteur (string-capitalize (substitute #\Space #\_ (string ing)))))

    (format t "~%Saisissez les ingrédients disponibles.~%")
    (format t "Pour chaque ingrédient, entrez le numéro et la quantité (0 pour terminer).~%~%")

    ;; Boucle de saisie
    (loop with nb-saisis = 0
          for choix = (lire-entier "Numéro de l'ingrédient (0 pour terminer) : " 0 (length ingredients))
          until (zerop choix)
          do (let* ((ingredient (nth (1- choix) ingredients))
                    (quantite (saisir-quantite-ingredient ingredient)))
               (ajouter-fait 'ingredients ingredient quantite)
               (format t "~A ~A : ~D ajouté~%~%" 
                       (format-ok)
                       (string-capitalize (substitute #\Space #\_ (string ingredient)))
                       quantite)
               (incf nb-saisis))
          finally (format t "~%~D ingrédient(s) saisi(s).~%" nb-saisis))
    (pause)))

(defun proposer-ingredients-connus ()
  "Affiche la liste des ingrédients référencés dans les recettes.
   Retour : liste de symboles d'ingrédients"
  (let ((ingredients nil))
    ;; Parcourir toutes les règles
    (dolist (regle *base-regles*)
      ;; Parcourir les conditions de chaque règle
      (dolist (condition (regle-conditions regle))
        (let ((cle (first condition))
              (op (second condition)))
          ;; Les ingrédients sont identifiés par des opérateurs numériques (>=, <=)
          (when (member op '(>= <=))
            (pushnew cle ingredients)))))
    ;; Retourner la liste triée alphabétiquement
    (sort ingredients #'string< :key #'string)))

(defun saisir-quantite-ingredient (ingredient)
  "Demande la quantité disponible pour un ingrédient.
   Paramètres :
     - ingredient : symbole de l'ingrédient
   Retour : quantité saisie (nombre)"
  (lire-entier 
   (format nil "Quantité de ~A : " 
           (string-capitalize (substitute #\Space #\_ (string ingredient))))
   1))

;;; ----------------------------------------------------------------------------
;;; SAISIE DU MATÉRIEL
;;; ----------------------------------------------------------------------------

(defun saisir-materiel ()
  "Interface de saisie du matériel de cuisine disponible.
   Permet à l'utilisateur de sélectionner le matériel possédé."
  
  (format t "~%=== SAISIE DU MATÉRIEL ===~%~%")
  
  (let ((materiel (proposer-materiel-connu)))
    ;; Vérifier si du matériel est disponible
    (when (null materiel)
      (format t "Aucun matériel disponible. Chargez d'abord les recettes.~%")
      (pause)
      (return-from saisir-materiel))
    ;; Afficher le nombre et la liste du matériel
    (format t "~D équipements disponibles dans le système.~%~%" (length materiel))
    (format t "Liste du matériel :~%")
    (loop for mat in materiel
          for compteur from 1
          do (format t "  ~2D. ~A~%" compteur (string-capitalize (substitute #\Space #\_ (string mat)))))

    (format t "~%Saisissez les numéros du matériel disponible, séparés par des espaces.~%")
    (format t "Exemple : 1 3 5 7~%")
    (format t "Ou appuyez sur Entrée pour terminer sans ajouter de matériel.~%~%")
    (format t "Votre sélection : ")
    (finish-output)

    ;; Lire et traiter l'entrée utilisateur
    (let* ((input (string-trim '(#\Space #\Tab) (read-line)))
           (choix-str (unless (string= input "") (diviser-chaine input))))
      (if (null choix-str)
          (format t "~%Aucun matériel saisi.~%")
          (loop with nb-saisis = 0
                with materiel-selectionne = nil
                for choix-input in choix-str
                for choix = (parse-integer choix-input :junk-allowed t)
                do (if (and choix (plusp choix) (<= choix (length materiel)))
                       (let ((mat (nth (1- choix) materiel)))
                         (unless (member mat materiel-selectionne)
                           (ajouter-fait 'materiel mat t)
                           (push mat materiel-selectionne)
                           (format t "~A ~A ajouté~%" 
                                   (format-ok)
                                   (string-capitalize (substitute #\Space #\_ (string mat))))
                           (incf nb-saisis)))
                       (format t "~A Choix invalide : ~A~%" (format-erreur) choix-input))
                finally (format t "~%~D équipement(s) saisi(s).~%" nb-saisis))))
    (pause)))

(defun diviser-chaine (chaine)
  "Divise une chaîne en liste de mots séparés par des espaces.
   Paramètres :
     - chaine : chaîne à diviser
   Retour : liste de chaînes"
  (let ((result nil)
        (word ""))
    (loop for char across chaine do
      (if (or (char= char #\Space) (char= char #\Tab))
          (when (> (length word) 0)
            (push word result)
            (setf word ""))
          (setf word (concatenate 'string word (string char)))))
    (when (> (length word) 0)
      (push word result))
    (nreverse result)))

(defun proposer-materiel-connu ()
  "Affiche la liste du matériel référencé dans les recettes.
   Retour : liste de symboles de matériel"
  (let ((materiel nil))
    ;; Parcourir toutes les règles
    (dolist (regle *base-regles*)
      ;; Parcourir les conditions de chaque règle
      (dolist (condition (regle-conditions regle))
        (let ((cle (first condition))
              (op (second condition)))
          ;; Le matériel est identifié par l'opérateur = avec valeur T
          (when (and (eq op '=) (eq (third condition) t))
            (pushnew cle materiel)))))
    ;; Retourner la liste triée alphabétiquement
    (sort materiel #'string< :key #'string)))

;;; ----------------------------------------------------------------------------
;;; CONFIGURATION DES FILTRES
;;; ----------------------------------------------------------------------------

(defun configurer-filtres ()
  "Interface de configuration des filtres optionnels.
   Permet de définir : végétarien, saisons, types de plats."
  
  (format t "~%=== CONFIGURATION DES FILTRES ===~%~%")
  
  ;; Filtre végétarien
  (let ((vegetarien (configurer-filtre-vegetarien)))
    (if vegetarien
        (progn
          (ajouter-fait 'filtres 'vegetarien t)
          (format t "~A Filtre végétarien activé~%" (format-ok)))
        (format t "~A Toutes les recettes (végétariennes et non-végétariennes) seront proposées~%" (format-ok))))
  (format t "~%")

  ;; Filtres saisons
  (let ((saisons (configurer-filtres-saisons)))
    (if saisons
        (dolist (saison saisons)
          (ajouter-fait 'filtres saison t)
          (format t "~A Saison ~A ajoutée~%" (format-ok) (string-capitalize (string saison))))
        (format t "~A Toutes les saisons acceptées~%" (format-ok))))
  (format t "~%")

  ;; Filtres types
  (let ((types (configurer-filtres-types)))
    (if types
        (dolist (type types)
          (ajouter-fait 'filtres type t)
          (format t "~A Type ~A ajouté~%" (format-ok) (string-capitalize (string type))))
        (format t "~A Tous les types de plats acceptés~%" (format-ok))))
  (format t "~%Configuration des filtres terminée.~%")
  (pause))

(defun configurer-filtre-vegetarien ()
  "Configure le filtre végétarien.
   Retour : t ou nil"
  (lire-oui-non "Souhaitez-vous uniquement des recettes végétariennes ? (o/n) : "))

(defun configurer-filtres-saisons ()
  "Configure les filtres de saisons.
   Retour : liste de saisons sélectionnées"
  (lire-choix-multiple "Sélectionnez les saisons (plusieurs choix possibles) :"
                       '(printemps ete automne hiver)))

(defun configurer-filtres-types ()
  "Configure les filtres de types de plats.
   Retour : liste de types sélectionnés"
  (lire-choix-multiple "Sélectionnez les types de plats (plusieurs choix possibles) :"
                       '(entree plat dessert)))

;;; ----------------------------------------------------------------------------
;;; LANCEMENT DU RAISONNEMENT
;;; ----------------------------------------------------------------------------

(defun lancer-recherche-recettes ()
  "Lance la recherche de recettes avec chaînage avant.
   Affiche les recettes réalisables et explique les exclusions."
  
  (format t "~%=== RECHERCHE DE RECETTES ===~%~%")
  
  ;; Vérifier que la base de faits n'est pas vide
  (when (null *base-faits*)
    (format t "~A Aucun ingrédient ou matériel saisi.~%" (format-erreur))
    (format t "Veuillez d'abord saisir vos ingrédients (option 1) et votre matériel (option 2).~%")
    (pause)
    (return-from lancer-recherche-recettes))
  (format t "Lancement du chaînage avant...~%~%")
  (chainage-avant)
  
  ;; Récupérer les recettes déduites (profondeur 0)
  (let ((recettes-trouvees 
          (loop for fait in *base-faits*
                for regle = (obtenir-regle (car fait))
                when (and regle (zerop (regle-profondeur regle)))
                collect (car fait))))
    
    ;; Afficher les résultats
    (if recettes-trouvees
        (progn
          (format t "~A ~D recette(s) réalisable(s) trouvée(s) !~%~%" 
                  (format-ok) 
                  (length recettes-trouvees))
          (afficher-recettes-realisables recettes-trouvees))
        (progn
          (format t "~A Aucune recette réalisable avec les ingrédients et le matériel disponibles.~%" 
                  (format-erreur))
          (format t "~%Suggestions :~%")
          (format t "  - Ajoutez plus d'ingrédients~%")
          (format t "  - Vérifiez que vous avez le matériel nécessaire~%")
          (format t "  - Modifiez vos filtres (végétarien, saisons, types)~%")))
    (format t "~%")
    
    ;; Proposer d'afficher la trace
    (when (lire-oui-non "Souhaitez-vous afficher la trace du raisonnement ? (o/n) : ")
      (afficher-trace-complete))
    (pause)))

(defun verifier-recette-specifique ()
  "Vérifie si une recette spécifique peut être réalisée (chaînage arrière).
   Demande à l'utilisateur quelle recette vérifier."
  ;; TODO: Implémenter la vérification
  ;; - Afficher liste des recettes disponibles
  ;; - Demander choix utilisateur
  ;; - Lancer chainage-arriere avec le but choisi
  ;; - Afficher le résultat (possible ou non)
  ;; - Expliquer pourquoi (manque d'ingrédients/matériel)
  )

;;; ----------------------------------------------------------------------------
;;; AFFICHAGE DES RÉSULTATS
;;; ----------------------------------------------------------------------------

(defun afficher-recettes-realisables (recettes)
  "Affiche la liste des recettes réalisables de manière formatée.
   Paramètres :
     - recettes : liste de symboles de recettes"
  
  ;; Grouper les recettes par type
  (let ((entrees nil)
        (plats nil)
        (desserts nil))
    
    (dolist (recette-nom recettes)
      (let ((regle (obtenir-regle recette-nom)))
        (when regle
          (let* ((metadata (regle-metadata regle))
                 (type-plat (cdr (assoc 'type metadata))))
            (cond
              ((eq type-plat 'entree)
               (push regle entrees))
              ((eq type-plat 'plat)
               (push regle plats))
              ((eq type-plat 'dessert)
               (push regle desserts))
              (t
               (push regle plats)))))))
    
    ;; Afficher les entrées
    (when entrees
      (format t "~%=== ENTRÉES ===~%")
      (dolist (regle (nreverse entrees))
        (afficher-recette-resume regle)))
    
    ;; Afficher les plats
    (when plats
      (format t "~%=== PLATS ===~%")
      (dolist (regle (nreverse plats))
        (afficher-recette-resume regle)))

    ;; Afficher les desserts
    (when desserts
      (format t "~%=== DESSERTS ===~%")
      (dolist (regle (nreverse desserts))
        (afficher-recette-resume regle)))))
          
(defun afficher-recette-resume (regle)
  "Affiche un résumé d'une recette.
   Paramètres :
     - regle : structure regle"
  
  (let* ((nom (regle-nom regle))
         (description (regle-description regle))
         (metadata (regle-metadata regle))
         (vegetarien (cdr (assoc 'vegetarien metadata)))
         (saisons (cdr (assoc 'saisons metadata))))
    
    (format t "~%  • ~A" (string-capitalize (substitute #\Space #\_ (string nom))))
    
    (when description
      (format t "~%    ~A" description))
    
    (when vegetarien
      (format t " ~C[32m[Végétarien]~C[0m" #\Escape #\Escape))
    
    (when saisons
      (format t "~%    Saisons : ~{~A~^, ~}" 
              (mapcar (lambda (s) (string-capitalize (string s))) saisons)))
    
    (format t "~%")))

(defun afficher-recette-detaillee (recette)
  "Affiche les détails d'une recette.
   Paramètres :
     - recette : symbole de la recette"
  ;; TODO: Implémenter l'affichage détaillé
  ;; - Récupérer la règle correspondante
  ;; - Afficher ingrédients requis avec quantités
  ;; - Afficher matériel nécessaire
  ;; - Afficher propriétés (saisons, type, végétarien)
  )

(defun expliquer-exclusion (recette)
  "Explique pourquoi une recette n'est pas réalisable.
   Paramètres :
     - recette : symbole de la recette"
  ;; TODO: Implémenter l'explication d'exclusion
  ;; - Identifier ingrédients manquants ou insuffisants
  ;; - Identifier matériel manquant
  ;; - Identifier filtres non respectés
  ;; - Afficher de manière claire et constructive
  )

;;; ----------------------------------------------------------------------------
;;; VISUALISATION DU RAISONNEMENT
;;; ----------------------------------------------------------------------------

(defun afficher-trace-complete ()
  "Affiche la trace complète du raisonnement effectué.
   Montre les règles déclenchées et les faits déduits."
  
  (format t "~%~%=== TRACE DU RAISONNEMENT ===~%~%")
  
  (let ((trace (obtenir-trace)))
    
    (if (null trace)
        (format t "Aucune trace disponible. Lancez d'abord une recherche de recettes.~%")
        (progn
          (format t "Règles déclenchées dans l'ordre :~%~%")
          (let ((etape 1))
            (dolist (entree trace)
              (let ((regle-nom (car entree))
                    (fait-deduit (cdr entree)))
                (format t "  Étape ~D : Règle ~A -> Fait ~A~%" 
                        etape 
                        (string-capitalize (substitute #\Space #\_ (string regle-nom)))
                        (string-capitalize (substitute #\Space #\_ (string fait-deduit))))
                (incf etape))))
          (format t "~%Total : ~D règle(s) déclenchée(s)~%" (length trace))))))

(defun visualiser-arbre-raisonnement (recette)
  "Visualise l'arbre de raisonnement pour une recette.
   Paramètres :
     - recette : symbole de la recette"
  ;; TODO: Implémenter la visualisation
  ;; - Générer l'arbre avec generer-arbre-raisonnement
  ;; - Afficher de manière hiérarchique (ASCII art)
  ;; - Montrer les dépendances entre règles
  )

;;; ----------------------------------------------------------------------------
;;; STATISTIQUES ET INFORMATIONS
;;; ----------------------------------------------------------------------------

(defun afficher-statistiques ()
  "Affiche les statistiques du système et de l'inférence."
  ;; TODO: Implémenter l'affichage des statistiques
  ;; - Nombre de recettes en base
  ;; - Nombre d'ingrédients référencés
  ;; - Nombre de règles déclenchées
  ;; - Nombre de recettes trouvées
  ;; - Temps d'exécution (optionnel)
  )

(defun afficher-etat-systeme ()
  "Affiche l'état complet du système (faits, règles, configuration)."
  ;; TODO: Implémenter l'affichage de l'état
  ;; - Base de faits courante
  ;; - Filtres actifs
  ;; - Nombre de règles chargées
  )

;;; ----------------------------------------------------------------------------
;;; UTILITAIRES D'INTERFACE
;;; ----------------------------------------------------------------------------

(defun format-ok ()
  "Retourne [OK] formaté en vert."
  (format nil "[~C[32mOK~C[0m]" #\Escape #\Escape))

(defun format-erreur ()
  "Retourne [ERREUR] formaté en rouge."
  (format nil "[~C[31mERREUR~C[0m]" #\Escape #\Escape))

(defun lire-entier (message &optional (min nil) (max nil))
  "Lit un entier depuis l'entrée utilisateur avec validation.
   Paramètres :
     - message : message à afficher
     - min : valeur minimale acceptée (optionnel)
     - max : valeur maximale acceptée (optionnel)
   Retour : entier saisi"
  (loop
    (format t "~A" message)
    (finish-output)
    
    (let* ((input (read-line))
           (valeur (parse-integer input :junk-allowed t)))
      (cond
        ;; Pas un nombre
        ((null valeur)
         (format t "~A Veuillez entrer un nombre valide.~%~%" (format-erreur)))
        ;; Vérifier minimum
        ((and min (< valeur min))
         (format t "~A La valeur doit être au moins ~D.~%~%" (format-erreur) min))
        ;; Vérifier maximum
        ((and max (> valeur max))
         (format t "~A La valeur doit être au plus ~D.~%~%" (format-erreur) max))
        ;; Valeur valide
        (t
         (return valeur))))))

(defun lire-oui-non (message)
  "Lit une réponse oui/non depuis l'entrée utilisateur.
   Paramètres :
     - message : message à afficher
   Retour : t pour oui, nil pour non"
  (loop
    (format t "~A" message)
    (finish-output)
    (let* ((input (string-trim '(#\Space #\Tab) (read-line)))
           (reponse (string-downcase input)))
      (cond
        ((or (string= reponse "o") (string= reponse "oui") 
             (string= reponse "y") (string= reponse "yes"))
         (return t))
        ((or (string= reponse "n") (string= reponse "non") 
             (string= reponse "no"))
         (return nil))
        (t
         (format t "~A Veuillez répondre par 'o' (oui) ou 'n' (non).~%" (format-erreur)))))))

(defun lire-choix-multiple (message options)
  "Lit un choix multiple depuis l'entrée utilisateur.
   Paramètres :
     - message : message à afficher
     - options : liste des options possibles
   Retour : liste des options sélectionnées"
  (loop
    (format t "~A~%" message)
    ;; Afficher les options numérotées
    (let ((compteur 1))
      (dolist (opt options)
        (format t "  ~D. ~A~%" compteur (string-capitalize (string opt)))
        (incf compteur)))
    
    (format t "~%Entrez les numéros séparés par des espaces (Entrée pour tous) : ")
    (finish-output)
    
    (let* ((input (string-trim '(#\Space #\Tab) (read-line)))
           (selections nil)
           (erreur nil))
      ;; Si entrée vide, retourner nil (tous acceptés)
      (when (string= input "")
        (return-from lire-choix-multiple nil))
      ;; Parser les choix
      (let ((choix-str (diviser-chaine input)))
        (dolist (choix-input choix-str)
          (let ((choix (parse-integer choix-input :junk-allowed t)))
            (if (and choix (>= choix 1) (<= choix (length options)))
                (let ((option (nth (1- choix) options)))
                  (unless (member option selections)
                    (push option selections)))
                (progn
                  (format t "~A Choix invalide : ~A~%" (format-erreur) choix-input)
                  (setf erreur t))))))
      ;; Si aucun choix valide, redemander
      (when (and erreur (null selections))
        (format t "~%"))
      ;; Si au moins un choix valide, retourner
      (when selections
        (return (nreverse selections))))))

(defun pause ()
  "Met en pause et attend que l'utilisateur appuie sur Entrée."
  (format t "~%Appuyez sur Entrée pour continuer...")
  (read-line))

;;; ----------------------------------------------------------------------------
;;; AIDE ET DOCUMENTATION
;;; ----------------------------------------------------------------------------

(defun afficher-aide ()
  "Affiche l'aide générale du système expert."
  ;; TODO: Implémenter l'affichage d'aide
  ;; - Expliquer le fonctionnement général
  ;; - Guider l'utilisateur pas à pas
  ;; - Donner des exemples d'utilisation
  )
