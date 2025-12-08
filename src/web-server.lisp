;;;; ============================================================================
;;;; SERVEUR WEB - HUNCHENTOOT
;;;; ============================================================================
;;;; Description : Serveur HTTP pour l'interface web du système expert
;;;;               Gère le démarrage/arrêt du serveur et sert les fichiers statiques
;;;; ============================================================================

;;; Chargement des dépendances
;;; Note : Les dépendances doivent être installées via apt :
;;;   sudo apt install cl-hunchentoot cl-yason

(asdf:load-system :hunchentoot)
(asdf:load-system :yason)

;;; ----------------------------------------------------------------------------
;;; VARIABLES GLOBALES
;;; ----------------------------------------------------------------------------

(defvar *web-server* nil
  "Instance du serveur Hunchentoot en cours d'exécution")

(defvar *web-port* 8080
  "Port par défaut du serveur web")

(defvar *web-root* "web/"
  "Répertoire racine des fichiers statiques (HTML, CSS, JS)")

;;; ----------------------------------------------------------------------------
;;; GESTION DU SERVEUR
;;; ----------------------------------------------------------------------------

(defun demarrer-serveur-web (&optional (port *web-port*))
  "Démarre le serveur web Hunchentoot sur le port spécifié.
   Paramètres :
     - port : port d'écoute (défaut : 8080)
   Retour : instance du serveur"
  (when (and *web-server* (hunchentoot:started-p *web-server*))
    (format t "Serveur déjà démarré sur le port ~D~%" *web-port*)
    (return-from demarrer-serveur-web *web-server*))
  
  (setf *web-port* port)
  (setf *web-server* (make-instance 'hunchentoot:easy-acceptor
                                     :port port
                                     :document-root *web-root*))
  
  (hunchentoot:start *web-server*)
  (format t "Serveur web démarré sur http://localhost:~D~%" port)
  *web-server*)

(defun arreter-serveur-web ()
  "Arrête le serveur web en cours d'exécution.
   Retour : t si arrêté, nil si aucun serveur actif"
  (if (and *web-server* (hunchentoot:started-p *web-server*))
      (progn
        (hunchentoot:stop *web-server*)
        (setf *web-server* nil)
        (format t "Serveur web arrêté~%")
        t)
      (progn
        (format t "Aucun serveur actif~%")
        nil)))

(defun redemarrer-serveur-web (&optional (port *web-port*))
  "Redémarre le serveur web (arrêt puis démarrage).
   Paramètres :
     - port : nouveau port d'écoute (défaut : 8080)"
  (arreter-serveur-web)
  (demarrer-serveur-web port))

(defun serveur-actif-p ()
  "Vérifie si le serveur web est actuellement actif.
   Retour : t si actif, nil sinon"
  (and *web-server* (hunchentoot:started-p *web-server*)))

;;; ----------------------------------------------------------------------------
;;; CONFIGURATION DES ROUTES STATIQUES
;;; ----------------------------------------------------------------------------

(defun configurer-fichiers-statiques ()
  "Configure la distribution des fichiers statiques (HTML, CSS, JS).
   Les fichiers du répertoire 'web/' seront accessibles directement."
  (push (hunchentoot:create-folder-dispatcher-and-handler "/" *web-root*)
        hunchentoot:*dispatch-table*))

;;; ----------------------------------------------------------------------------
;;; HANDLERS HTTP PRINCIPAUX
;;; ----------------------------------------------------------------------------

(hunchentoot:define-easy-handler (page-accueil :uri "/") ()
  "Page d'accueil - redirige vers index.html"
  (hunchentoot:redirect "/index.html"))

(hunchentoot:define-easy-handler (page-404 :uri "/404") ()
  "Page d'erreur 404"
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  "<html><head><title>404 - Page non trouvée</title></head><body><h1>404 - Page non trouvée</h1><p>La page demandée n'existe pas.</p><a href='/'>Retour à l'accueil</a></body></html>")

;;; ----------------------------------------------------------------------------
;;; UTILITAIRES
;;; ----------------------------------------------------------------------------

(defun obtenir-url-serveur ()
  "Retourne l'URL complète du serveur web.
   Retour : chaîne de caractères (ex: 'http://localhost:8080')"
  (format nil "http://localhost:~D" *web-port*))

(defun afficher-info-serveur ()
  "Affiche les informations sur le serveur web en cours d'exécution."
  (format t "~%=== Informations serveur web ===~%")
  (format t "URL: ~A~%" (obtenir-url-serveur))
  (format t "État: ~A~%" (if (serveur-actif-p) "Actif" "Inactif"))
  (format t "Répertoire web: ~A~%" *web-root*))

;;; ----------------------------------------------------------------------------
;;; GESTION DES ERREURS
;;; ----------------------------------------------------------------------------

(defun gerer-erreur-serveur (condition)
  "Gestionnaire d'erreurs pour le serveur web.
   Paramètres :
     - condition : condition d'erreur Lisp"
  (format t "Erreur serveur: ~A~%" condition)
  (setf (hunchentoot:return-code*) 500)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Erreur interne du serveur: ~A" condition))
