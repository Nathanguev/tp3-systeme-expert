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
         (afficher-base-faits))
        
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
         (format t "~%Choix invalide. Veuillez entrer un nombre entre 0 et 9.~%"))))))

;;; ----------------------------------------------------------------------------
;;; SAISIE DES INGRÉDIENTS
;;; ----------------------------------------------------------------------------

(defun saisir-ingredients ()
  "Interface de saisie des ingrédients disponibles.
   Permet à l'utilisateur d'entrer les ingrédients et leurs quantités."
  
  (format t "~%=== SAISIE DES INGRÉDIENTS ===~%~%")
  
  ;; Récupérer la liste des ingrédients connus
  (let ((ingredients (lister-tous-ingredients))
        (nb-saisis 0))
    
    (if (null ingredients)
        (progn
          (format t "Aucun ingrédient disponible. Chargez d'abord les recettes.~%")
          (return-from saisir-ingredients))
        (format t "~D ingrédients disponibles dans le système.~%~%" (length ingredients)))
    
    ;; Afficher la liste des ingrédients
    (format t "Liste des ingrédients :~%")
    (let ((compteur 1))
      (dolist (ing ingredients)
        (format t "  ~2D. ~A~%" compteur (string-capitalize (substitute #\Space #\_ (string ing))))
        (incf compteur)))
    
    (format t "~%Saisissez les ingrédients disponibles.~%")
    (format t "Pour chaque ingrédient, entrez le numéro et la quantité (0 pour terminer).~%~%")
    
    ;; Boucle de saisie
    (loop
      (format t "Numéro de l'ingrédient (0 pour terminer) : ")
      (finish-output)
      
      (let* ((input (read-line))
             (choix (parse-integer input :junk-allowed t)))
        
        ;; Terminer la saisie
        (when (or (null choix) (= choix 0))
          (format t "~%~D ingrédient(s) saisi(s).~%" nb-saisis)
          (return))
        
        ;; Vérifier que le choix est valide
        (if (and (> choix 0) (<= choix (length ingredients)))
            (let ((ingredient (nth (1- choix) ingredients)))
              
              ;; Demander la quantité
              (format t "Quantité de ~A : " 
                      (string-capitalize (substitute #\Space #\_ (string ingredient))))
              (finish-output)
              
              (let* ((qty-input (read-line))
                     (quantite (parse-integer qty-input :junk-allowed t)))
                
                (if (and quantite (> quantite 0))
                    (progn
                      ;; Ajouter le fait
                      (ajouter-fait ingredient quantite)
                      (format t "✓ ~A : ~D ajouté~%" 
                              (string-capitalize (substitute #\Space #\_ (string ingredient)))
                              quantite)
                      (incf nb-saisis))
                    (format t "✗ Quantité invalide. L'ingrédient n'a pas été ajouté.~%"))))
            
            (format t "✗ Choix invalide. Veuillez entrer un numéro entre 1 et ~D.~%" 
                    (length ingredients)))
        
        (format t "~%")))
    
    (pause)))

(defun proposer-ingredients-connus ()
  "Affiche la liste des ingrédients référencés dans les recettes.
   Retour : liste de symboles d'ingrédients"
  ;; TODO: Implémenter l'extraction des ingrédients
  ;; - Parser toutes les règles
  ;; - Extraire les ingrédients requis
  ;; - Retourner liste unique triée
  )

(defun saisir-quantite-ingredient (ingredient)
  "Demande la quantité disponible pour un ingrédient.
   Paramètres :
     - ingredient : symbole de l'ingrédient
   Retour : quantité saisie (nombre)"
  ;; TODO: Implémenter la saisie avec validation
  ;; - Afficher l'unité appropriée (g, cl, unités)
  ;; - Valider que c'est un nombre positif
  ;; - Gérer les erreurs de saisie
  )

;;; ----------------------------------------------------------------------------
;;; SAISIE DU MATÉRIEL
;;; ----------------------------------------------------------------------------

(defun saisir-materiel ()
  "Interface de saisie du matériel de cuisine disponible.
   Permet à l'utilisateur de sélectionner le matériel possédé."
  ;; TODO: Implémenter la saisie du matériel
  ;; - Afficher liste complète du matériel possible
  ;; - Permettre sélection multiple (cocher/décocher)
  ;; - Ajouter à la base de faits (t/nil pour chaque)
  )

(defun proposer-materiel-connu ()
  "Affiche la liste du matériel référencé dans les recettes.
   Retour : liste de symboles de matériel"
  ;; TODO: Implémenter l'extraction du matériel
  ;; - Parser toutes les règles
  ;; - Extraire le matériel requis
  ;; - Retourner liste unique triée
  )

;;; ----------------------------------------------------------------------------
;;; CONFIGURATION DES FILTRES
;;; ----------------------------------------------------------------------------

(defun configurer-filtres ()
  "Interface de configuration des filtres optionnels.
   Permet de définir : végétarien, saisons, types de plats."
  ;; TODO: Implémenter la configuration des filtres
  ;; - Menu pour chaque catégorie de filtre
  ;; - Végétarien : oui/non
  ;; - Saisons : sélection multiple (printemps/été/automne/hiver)
  ;; - Types : sélection multiple (entrée/plat/dessert)
  ;; - Ajouter les filtres à la base de faits
  )

(defun configurer-filtre-vegetarien ()
  "Configure le filtre végétarien.
   Retour : t ou nil"
  ;; TODO: Implémenter la configuration
  )

(defun configurer-filtres-saisons ()
  "Configure les filtres de saisons.
   Retour : liste de saisons sélectionnées"
  ;; TODO: Implémenter la configuration
  ;; - Permettre sélection multiple
  ;; - Si aucune sélectionnée, toutes acceptées
  )

(defun configurer-filtres-types ()
  "Configure les filtres de types de plats.
   Retour : liste de types sélectionnés"
  ;; TODO: Implémenter la configuration
  ;; - Permettre sélection multiple
  ;; - Si aucun sélectionné, tous acceptés
  )

;;; ----------------------------------------------------------------------------
;;; LANCEMENT DU RAISONNEMENT
;;; ----------------------------------------------------------------------------

(defun lancer-recherche-recettes ()
  "Lance la recherche de recettes avec chaînage avant.
   Affiche les recettes réalisables et explique les exclusions."
  ;; TODO: Implémenter le lancement
  ;; - Vérifier que des ingrédients et matériel sont saisis
  ;; - Lancer chainage-avant du moteur
  ;; - Récupérer les recettes déduites
  ;; - Afficher les résultats formatés
  ;; - Proposer d'afficher la trace
  )

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
  ;; TODO: Implémenter l'affichage
  ;; - Grouper par type (entrée/plat/dessert)
  ;; - Afficher nom, propriétés (végétarien, saisons)
  ;; - Formater de manière lisible et attractive
  )

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
  ;; TODO: Implémenter l'affichage de trace
  ;; - Récupérer la trace avec obtenir-trace-inference
  ;; - Afficher chronologiquement
  ;; - Format : Étape N : Règle RX -> Fait Y
  )

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

(defun lire-entier (message &optional (min nil) (max nil))
  "Lit un entier depuis l'entrée utilisateur avec validation.
   Paramètres :
     - message : message à afficher
     - min : valeur minimale acceptée (optionnel)
     - max : valeur maximale acceptée (optionnel)
   Retour : entier saisi"
  ;; TODO: Implémenter la lecture avec validation
  ;; - Afficher le message
  ;; - Lire l'entrée
  ;; - Valider que c'est un nombre
  ;; - Valider les bornes si spécifiées
  ;; - Redemander en cas d'erreur
  )

(defun lire-oui-non (message)
  "Lit une réponse oui/non depuis l'entrée utilisateur.
   Paramètres :
     - message : message à afficher
   Retour : t pour oui, nil pour non"
  ;; TODO: Implémenter la lecture oui/non
  ;; - Accepter : o/oui/y/yes pour t
  ;; - Accepter : n/non/no pour nil
  ;; - Redemander en cas d'entrée invalide
  )

(defun lire-choix-multiple (message options)
  "Lit un choix multiple depuis l'entrée utilisateur.
   Paramètres :
     - message : message à afficher
     - options : liste des options possibles
   Retour : liste des options sélectionnées"
  ;; TODO: Implémenter la sélection multiple
  ;; - Afficher les options numérotées
  ;; - Permettre sélection par numéros séparés par espaces
  ;; - Valider les entrées
  )

(defun pause ()
  "Met en pause et attend que l'utilisateur appuie sur Entrée."
  (format t "~%Appuyez sur Entrée pour continuer...")
  (read-line))

(defun effacer-ecran ()
  "Efface l'écran du terminal (multiplateforme)."
  ;; TODO: Implémenter le nettoyage d'écran
  ;; - Utiliser les codes ANSI ou commande système
  )

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
