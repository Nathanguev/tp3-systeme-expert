;;;; ============================================================================
;;;; DONNÉES DES RECETTES - FORMAT BRUT
;;;; ============================================================================
;;;; Description : Données brutes des recettes au format simplifié
;;;;               Ces données seront parsées et converties en règles
;;;;               par le fichier donnees/recettes.lisp
;;;; ============================================================================
;;;; Format : nom_recette ((ingredients) (materiel) [saisons] [type] [vegetarien])
;;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; COMPOSITIONS INTERMÉDIAIRES
;;; ----------------------------------------------------------------------------

(defparameter *donnees-compositions*
  '(
    (pate_brisee 
     ((farine 200) (oeuf 1) (eau 4) (beurre 100))
     (bol rouleau)
     nil nil vegetarien)
    
    (pate_crepe 
     ((beurre 40) (oeuf 4) (farine 250) (lait 60) (sucre 40))
     (bol fouet)
     nil nil vegetarien)
    ))

;;; ----------------------------------------------------------------------------
;;; RECETTES FINALES
;;; ----------------------------------------------------------------------------

(defparameter *donnees-recettes*
  '(
    (riz_pilaf 
     ((oignon 1) (riz_long 300) (beurre 30) (eau 60))
     (casserole)
     (printemps ete automne hiver)
     plat
     vegetarien)
    
    (celeri_roti 
     ((celeri 1) (ail 2) (huile 2) (beurre 20))
     (four)
     (automne hiver)
     plat
     vegetarien)
    
    (gratin_dauphinois 
     ((beurre 10) (pomme_de_terre 1200) (lait 40) (ail 5) (creme 40))
     (four)
     (automne hiver)
     plat
     vegetarien)
    
    (fond_blanc_volaille 
     ((carotte 2) (poireau 0.5) (oignon 1) (ail 2) (volaille 1) (eau 200))
     (casserole ecumoire)
     (printemps ete automne hiver)
     plat
     nil)
    
    (quiche_lorraine 
     ((beurre 20) (oeuf 8) (creme 40) (lard 180) (gruyere 70) (pate_brisee 1))
     (moule_tarte fouet bol)
     (printemps ete automne hiver)
     (plat entree)
     nil)
    
    (tarte_tatin 
     ((pomme 15) (sucre 100) (eau 5) (beurre 50) (pate_brisee 1))
     (moule_tarte casserole)
     (automne hiver)
     dessert
     vegetarien)
    
    (crepe_suzette 
     ((huile 4) (sucre 30) (jus_orange 10) (beurre 50) (pate_crepe 1))
     (poele spatule assiette)
     (printemps ete automne hiver)
     dessert
     vegetarien)
    
    (salade_composee 
     ((lard 80) (tomate 3) (emmental 80) (pomme_de_terre 500) 
      (surimi 10) (oeuf 4) (cornichon 15))
     (saladier casserole)
     (printemps ete)
     entree
     nil)
    
    (boeuf_bourguignon 
     ((beurre 60) (oignon 1) (farine 30) (boeuf 600) (champignon 30))
     (cocotte)
     (automne)
     plat
     nil)
    
    (lasagne 
     ((pate_lasagne 1) (bechamel 1) (farce_viande 1))
     (four)
     (printemps ete automne hiver)
     plat
     nil)
    
    (omelette_champignons 
     ((oeuf 3) (champignon 150) (beurre 1))
     (poele spatule bol)
     (printemps ete automne hiver)
     plat
     vegetarien)
    
    (pates_carbonara 
     ((pates 200) (lardons 100) (oeuf 2))
     (casserole poele)
     (printemps ete automne hiver)
     plat
     nil)
    
    (soupe_potiron 
     ((potiron 400) (oignon 1) (bouillon_legumes 1))
     (casserole mixeur)
     (automne hiver)
     plat
     vegetarien)
    
    (cannellonis 
     ((pate_lasagne 1) (sauce_bolognaise 1) (bechamel 1) (parmesan 50))
     (four)
     (printemps ete automne hiver)
     plat
     nil)
    ))

;;; ----------------------------------------------------------------------------
;;; COMPOSITIONS INTERMÉDIAIRES SUPPLÉMENTAIRES
;;; ----------------------------------------------------------------------------

(defparameter *donnees-compositions-avancees*
  '(
    (pate_lasagne 
     ((farine 175) (oeuf 6))
     (bol rouleau)
     nil nil nil)
    
    (bechamel 
     ((beurre 30) (farine 30) (lait 45))
     (casserole fouet)
     nil nil nil)
    
    (farce_viande 
     ((oignon 1) (ail 2) (huile 2) (boeuf 600))
     (poele spatule)
     nil nil nil)
    
    (concasse 
     ((tomate 5) (oignon 1) (ail 3) (huile 2))
     (poele)
     nil nil nil)
    
    (sauce_bolognaise 
     ((concasse 1) (farce_viande 1))
     (casserole)
     nil nil nil)
    ))
