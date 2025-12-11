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
    (pate_choux ; OK
     ((beurre 100) (lait 125) (eau 125) (farine 150) (oeuf 5))
     (bol casserole fouet)
     nil nil vegetarien)

    (pate_lasagnes ; OK
     ((farine 350) (oeuf 13) (huile 15))
     (bol rouleau)
     nil nil nil)

    (pate_tempura ; OK
     ((farine 160) (fecule 10) (oeuf 2) (biere 300))
     (bol fouet)
     nil nil vegetarien)

    (pain_burger ; OK
     ((lait 230) (levure 18) (beurre 40) (farine 500) (oeuf 3) (sesame 60))
     (bol four)
     nil nil vegetarien)

    (pate_pizza ; OK
     ((lait 25) (levure 12) (farine 250) (eau 125) (huile 25))
     (bol rouleau)
     nil nil vegetarien)

    (pate_brisee ; OK
     ((farine 200) (oeuf 1) (eau 40) (beurre 100))
     (bol rouleau)
     nil nil vegetarien)
    
    (pate_crumble ; OK
     ((beurre 100) (farine 260) (sucre 130) (oeuf 1))
     (bol)
     nil nil vegetarien)

    (pate_crepe ; OK
     ((beurre 40) (oeuf 4) (farine 250) (lait 600) (sucre 40))
     (bol fouet)
     nil nil vegetarien)

    (pate_beignets ; OK
     ((farine 200) (oeuf 2) (lait 150) (biere 150) (sucre 20))
     (bol fouet)
     nil nil vegetarien)

    (bouillon_legumes ; OK
     ((carotte 3) (poireau 1) (fenouil 1) (navet 1) (oignon 1) (ail 5) (huile 45) (vin_blanc 200) (eau 30))
     (casserole ecumoire)
     nil nil vegetarien)

    (fond_blanc_volaille ; OK
     ((carotte 2) (poireau 1) (oignon 1) (ail 2) (volaille 1) (eau 2000))
     (casserole ecumoire)
     nil nil nil)

    (fond_boeuf ; OK
     ((boeuf 800) (huile 60) (carotte 2) (oignon 2) (tomate 2) (poireau 1) (ail 4) (farine 30) (vin_blanc 150) (eau 2000))
     (casserole ecumoire)
      nil nil nil)

    (puree ; OK
     ((pomme_de_terre 10) (lait 200) (beurre 200))
     (casserole)
     nil nil vegetarien)
    
    (bechamel ; OK
     ((beurre 30) (farine 30) (lait 300))
     (casserole fouet)
     nil nil nil)

    (sauce_bolognaise ; OK
     ((concasse_tomates 1) (farce_viande 1))
     (casserole)
     nil nil nil)

    (creme_anglaise ; OK
     ((lait 250) (creme 250) (oeuf 5) (sucre 60))
     (casserole fouet)
     nil nil vegetarien)

    (creme_patissiere ; OK
     ((lait 350) (oeuf 4) (sucre 50) (farine 35) (creme 175))
     (casserole fouet)
     nil nil vegetarien)

    (chantilly ; OK
     ((creme 400) (sucre 80))
     (bol fouet)
     nil nil vegetarien)
    ))

;;; ----------------------------------------------------------------------------
;;; RECETTES FINALES
;;; ----------------------------------------------------------------------------

(defparameter *donnees-recettes*
  '(
    (meringue_francaise ; OK
     ((oeuf 3) (sucre 150))
     (bol fouet)
     (printemps ete automne hiver)
     dessert
     vegetarien)

    (taboule ; OK
     ((semoule 250) (eau 550) (tomate 2) (poivron 1) (huile 50) (vinaigre 50))
     (saladier)
     (printemps ete)
     entree
     vegetarien)

    (potage ; OK
     ((pain 60) (ail 5) (huile 45) (carotte 2) (oignon 1) (lard 150) (lentille 250) (vin_blanc 50) (eau 1750))
     (casserole mixeur)
     (automne hiver)
     plat
     nil)

    (salade_lentilles ; OK
     ((lentille 240) (carotte 1) (oignon 1) (ail 1) (huile 15) (beurre 20) (vin_blanc 50) (fond_boeuf 1))
     (casserole saladier)
     (ete automne)
     entree
     nil)

    (salade_haricots_blancs ; OK
     ((haricot_blanc 240) (carotte 1) (oignon 1) (ail 3) (beurre 20) (huile 150) (lard 150) (fond_blanc_volaille 1))
     (casserole saladier)
     (ete automne)
     entree
     nil)

    (risotto ; OK
     ((oignon 1) (riz_rond 350) (vin_blanc 100) (eau 850) (mascarpone 100) (parmesan 50))
     (casserole)
     (printemps ete automne hiver)
     plat
     vegetarien)

    (riz_pilaf ; OK
     ((oignon 1) (riz_long 300) (beurre 30) (eau 600))
     (casserole)
     (printemps ete automne hiver)
     plat
     vegetarien)

    (poireaux_vinaigrette ; OK
     ((poireau 3) (echalote 1) (beurre 10) (huile 15) (vin_blanc 60) (fond_boeuf 1) (farine 50)) ; vinaigrette
     (casserole saladier)
     (printemps ete automne hiver)
     entree
     nil)

    (macedoine_legumes ; OK
     ((carotte 2) (petit_pois 120) (haricot_vert 120) (pomme_de_terre 2) (navet 2)) ; mayonnaise
     (casserole saladier)
     (printemps ete)
     entree
     vegetarien)
    
    (remoulade_celeri_rave ; OK
     ((celeri_rave 1) (pomme 2) (citron 1) (moutarde 15)) ; mayonnaise
     (saladier)
     (automne hiver)
     entree
     vegetarien)

    (soupe_oignon ; OK
     ((oignon 2) (huile 45) (beurre 40) (vin_blanc 100) (pain 100) (gruyere 170) (eau 1000))
     (casserole mixeur)
     (automne hiver)
     plat
     vegetarien)

    (veloute_legumes ; OK
     ((oignon 1) (ail 2) (potimarron 700) (huile 15) (bouillon_legumes 1) (creme 500) (beurre 60))
     (casserole mixeur)
     (automne hiver)
     plat
     vegetarien)

    (ratatouille ; OK
     ((aubergine 2) (courgette 2) (poivron 2) (tomate 4) (oignon 2) (ail 4) (huile 50) (bouillon_legumes 1))
     (casserole)
     (ete automne)
     plat
     vegetarien)

    (ravioles ; OK
     ((pate_lasagnes 1) (champignon 750) (echalote 3) (ail 6) (huile 75) (creme 400) (vin_blanc 230))
     (casserole)
     (printemps ete automne hiver)
     plat
     vegetarien)

    (flan_legumes ; OK
     ((courgette 2) (ail 2) (beurre 10) (huile 45) (oeuf 4) (creme 300) (lait 200))
     (bol four moule_cake fouet)
     (printemps ete automne hiver)
     plat
     vegetarien)

    (celeri_roti ; OK
     ((celeri_rave 1) (ail 2) (huile 2) (beurre 20))
     (four)
     (automne hiver)
     plat
     vegetarien)

    (legumes_braise ; OK
     ((carotte 2) (fenouil 2) (oignon 1) (ail 2) (huile 30) (bouillon_legumes 1))
     (poele)
     (automne hiver)
     plat
     vegetarien)

    (gratin_chicons ; OK
     ((endive 4) (citron 1) (huile 15) (beurre 30) (jambon 200) (gruyere 100) (bechamel 1))
     (four poele)
     (automne hiver)
     plat
     nil)
    
    (gratin_dauphinois ; OK
     ((beurre 10) (pomme_de_terre 12) (lait 400) (ail 5) (creme 400))
     (four)
     (automne hiver)
     plat
     vegetarien)
    
    (gnocchis ; OK
     ((pomme_de_terre 5) (farine 200) (oeuf 1) (parmesan 50) (huile 60))
     (bol casserole)
     (printemps ete automne hiver)
     plat
     vegetarien)

    (pommes_paillasson ; OK
     ((pomme_de_terre 8) (huile 30) (beurre 90))
     (poele saladier)
     (printemps ete automne hiver)
     plat
     vegetarien)

    (frites ; OK
     ((pomme_de_terre 6) (huile 1000))
     (friteuse)
     (printemps ete automne hiver)
     plat
     vegetarien)

    ; Recettes poisson
    
    (volaille_coffre ; OK
     ((volaille 1) (citron 1) (beurre 180) (huile 15))
     (four poele)
     (printemps ete automne hiver)
     plat
     nil)

    (cordons_bleus ; OK
     ((volaille 1) (jambon 100) (emmental 200) (oeuf 2) (farine 80) (chapelure 120) (huile 20) (beurre 40))
     (poele bol)
     (printemps ete automne hiver)
     plat
     nil)

    (poulet_basquaise ; OK
     ((volaille 1) (oignon 1) (poivron 3) (farine 20) (fond_blanc_volaille 1) (huile 100))
     (casserole)
     (printemps ete automne hiver)
     plat
     nil)

    (volaille_citron ; OK
     ((volaille 1) (citron 2) (oignon 2) (ail 4) (gingembre 20) (huile 15) (farine 20) (olive 60) (fond_blanc_volaille 1))
     (casserole)
     (printemps ete automne hiver)
     plat
     nil)

    (oeufs_mimosa ; OK
     ((oeuf 7) (moutarde 15)) ; mayonnaise
     (casserole bol)
     (printemps ete automne hiver)
     entree
     vegetarien)

    (oeufs_mollets ; OK
     ((oeuf 6) (epinard 300) (beurre 40) (ail 1) (gruyere 140))
     (casserole four)
     (printemps ete automne hiver)
     plat
     vegetarien)

    (tortilla ; OK
     ((oeuf 6) (pomme_de_terre 5) (oignon 1) (ail 2) (huile 45))
     (poele saladier)
     (printemps ete automne hiver)
     plat
     vegetarien)

    (quiche_lorraine ; OK
     ((beurre 20) (oeuf 8) (creme 400) (lait 400) (lard 180) (gruyere 70) (pate_brisee 1))
     (moule_tarte fouet saladier)
     (printemps ete automne hiver)
     (plat entree)
     nil)

    (pates_carbonara ; OK
     ((pates 300) (lardons 100) (oeuf 3) (parmesan 50))
     (casserole poele)
     (printemps ete automne hiver)
     plat
     nil)

    (tomates_farcies ; OK
     ((tomate 4) (farce_viande 1) (vinaigre 80) (beurre 20) (huile 15))
     (four)
     (ete automne)
     plat
     nil)

    (rougail_saucisse ; OK
     ((saucisse 2) (tomate 4) (oignon 2) (ail 4) (lard 150) (gingembre 30) (huile 20) (concasse_tomates 1) (eau 500))
     (casserole)
     (printemps ete automne hiver)
     plat
     nil)

    (potee ; OK
     ((oignon 1) (carotte 4) (navet 4) (chou_vert 1) (ail 6) (pommes_de_terre 5) (huile 60) (lard 500) (eau 3500))
     (casserole)
     (automne hiver)
     plat
     nil)

    (hachis_parmentier ; OK
     ((boeuf 800) (oignon 2) (ail 5) (beurre 10) (huile 15) (fond_boeuf 1) (puree 1) (chapelure 60))
     (casserole four)
     (automne hiver)
     plat
     nil)

    (lasagnes ; OK
     ((pate_lasagnes 1) (bechamel 1) (sauce_bolognaise 1) (gruyere 100))
     (four)
     (printemps ete automne hiver)
     plat
     nil)

    (gougeres_fromage ; OK
     ((pate_choux 1) (emmental 40) (oeuf 1))
     (four)
     (printemps ete automne hiver)
     entree
     vegetarien)

    (souffle_fromage ; OK
     ((oeuf 4) (beurre 10) (fecule 30) (lait 100) (compte 400) (creme 300))
     (four saladier fouet moule_cake)
     (printemps ete automne hiver)
     entree
     vegetarien)

    (ile_flottante ; OK
     ((oeuf 5) (sucre 100) (eau 30) (creme_anglaise 1))
     (saladier)
     (printemps ete automne hiver)
     dessert
     vegetarien)
    
    (flan_patissier ; OK
     ((beurre 20) (sucre 50) (pate_brisee 1) (creme_patissiere 2))
     (bol four)
     (printemps ete automne hiver)
     dessert
     vegetarien)

    (gateau_yaourt ; OK
     ((yaourt 1) (oeuf 4) (sucre 250) (farine 375) (levure 8) (huile 10) (beurre 10))
     (saladier four moule_cake fouet)
     (printemps ete automne hiver)
     dessert
     vegetarien)

    (choux_chantilly ; OK
     ((pate_choux 1) (chantilly 1))
     ()
     (printemps ete automne hiver)
     dessert
     vegetarien)

    (creme_caramel ; OK
     ((oeuf 3) (sucre 180) (lait 500) (eau 30))
     (bol four)
     (printemps ete automne hiver)
     dessert
     vegetarien)

    (tarte_tatin ; OK
     ((pomme 10) (sucre 100) (eau 50) (beurre 50) (pate_brisee 1))
     (moule_tarte poele)
     (automne hiver)
     dessert
     vegetarien)

    (crumble_pommes ; OK
     ((pomme 6) (sucre 20) (beurre 30) (citron 1) (pate_crumble 1))
     (four)
     (automne hiver)
     dessert
     vegetarien)

    (beignets_pommes ; OK
     ((pomme 4) (sucre 30) (huile 1000) (pate_beignets 1))
     (friteuse)
     (printemps ete automne hiver)
     dessert
     vegetarien)

    (tarte_bourdaloue ; OK
     ((pate_brisee 1) (creme_patissiere 1) (poire 4) (poudre_amande 100) (sucre 180) (beurre 20) (eau 700) (citron 1))
     (four bol)
     (printemps ete automne hiver)
     dessert
     vegetarien)
    
    (souffle_orange ; OK
     ((oeuf 6) (sucre 40) (orange 1) (beurre 10) (fecule 10) (creme_patissiere 1))
     (four saladier fouet moule_cake)
     (printemps ete automne hiver)
     dessert
     vegetarien)

    (crepe_suzette ; OK
     ((huile 40) (sucre 30) (jus_orange 100) (beurre 50) (pate_crepe 1))
     (poele spatule assiette)
     (printemps ete automne hiver)
     dessert
     vegetarien)
    
    (mousse_chocolat ; OK
     ((chocolat 200) (oeuf 8) (sucre 40) (beurre 100))
     (casserole bol fouet)
     (printemps ete automne hiver)
     dessert
     vegetarien)
    
    (boeuf_bourguignon ; OK
     ((beurre 60) (oignon 2) (carotte 4) (ail 4) (poireau 1) (farine 30) (boeuf 1000) (vin_rouge 1000) 
      (fond_boeuf 1) (huile 45))
     (cocotte)
     (automne)
     plat
     nil)
    ))

;;; ----------------------------------------------------------------------------
;;; COMPOSITIONS INTERMÉDIAIRES SUPPLÉMENTAIRES
;;; ----------------------------------------------------------------------------

(defparameter *donnees-compositions-avancees*
  '(
    (farce_viande ; OK
     ((oignon 1) (ail 2) (huile 30) (boeuf 600))
     (poele spatule)
     nil nil nil)
    
    (concasse_tomates ; OK
     ((tomate 5) (oignon 1) (ail 3) (huile 2))
     (poele)
     nil nil nil)
    ))
