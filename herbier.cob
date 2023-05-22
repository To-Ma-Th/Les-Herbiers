IDENTIFICATION DIVISION.
PROGRAM-ID. Herbier.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL. 
       SELECT fplan ASSIGN TO "plante.dat"
       ORGANIZATION INDEXED
       ACCESS MODE IS DYNAMIC
       RECORD KEY IS pl_id
       ALTERNATE RECORD KEY IS pl_nom
       ALTERNATE RECORD KEY IS pl_nomLatin
       ALTERNATE RECORD KEY IS pl_type WITH DUPLICATES
       ALTERNATE RECORD KEY IS pl_saison WITH DUPLICATES
       FILE STATUS IS cr_fplan.

       SELECT fher ASSIGN TO "herbier.dat"
       ORGANIZATION INDEXED
       ACCESS MODE IS DYNAMIC
       RECORD KEY IS fh_id
       ALTERNATE RECORD KEY IS fh_utilisateur WITH DUPLICATES
       ALTERNATE RECORD KEY IS fh_type WITH DUPLICATES
       FILE STATUS IS cr_fher.

       SELECT futil ASSIGN TO "utilsateur.dat"
       ORGANIZATION INDEXED
       ACCESS MODE IS DYNAMIC
       RECORD KEY IS fu_id
       ALTERNATE RECORD KEY IS fu_login
       ALTERNATE RECORD KEY IS fu_role WITH DUPLICATES
       ALTERNATE RECORD KEY IS fu_type WITH DUPLICATES
       FILE STATUS IS cr_futil.

       SELECT fhpl ASSIGN TO "herbier_plante.dat"
       ORGANIZATION INDEXED
       ACCESS MODE IS DYNAMIC
       RECORD KEY IS fhpl_id
       ALTERNATE RECORD KEY IS fhpl_lieu WITH DUPLICATES
       ALTERNATE RECORD KEY IS fhpl_idHerbier WITH DUPLICATES
       ALTERNATE RECORD KEY IS fhpl_idPlante WITH DUPLICATES
       FILE STATUS IS cr_fhpl.


DATA DIVISION.
FILE SECTION.
FD fplan.
       01 tamp_fplan.
           02 pl_id PIC 9(3).
           02 pl_nom PIC A(30).
           02 pl_nomLatin PIC A(45).
           02 pl_habitat PIC A(15).
           02 pl_saison PIC A(15).
           02 pl_duree PIC 9(2).
           02 pl_type PIC A(15).
                
FD fher.
       01 tamp_fher.
           02 fh_id PIC 9(3).
           02 fh_nom PIC A(30).
           02 fh_utilisateur PIC 9(3).
           02 fh_date PIC A(15).
           02 fh_type PIC A(20).
              
FD futil.
       01 tamp_futil.
           02 fu_id PIC 9(3).
           02 fu_login PIC A(20).
           02 fu_mdp PIC A(20).
           02 fu_role PIC A(15).
           02 fu_type PIC A(15).
                
FD fhpl.
       01 tamp_fhpl.
           02 fhpl_id PIC 9(3).
           02 fhpl_idHerbier PIC 9(3).
           02 fhpl_idPlante PIC 9(3).
           02 fhpl_date PIC A(15).
           02 fhpl_taille PIC 9(5).
           02 fhpl_lieu PIC A(40).
               

               
WORKING-STORAGE SECTION.
      *> Constantes
       77 CONST_ROLE_READER PIC A(15).
       77 CONST_ROLE_WAITING PIC A(15).
       77 CONST_ROLE_EDITOR PIC A(15).
       77 CONST_ROLE_ADMIN PIC A(15).
       77 CONST_USER_PRO PIC A(15).
       77 CONST_USER_AMATEUR PIC A(15).
       77 CONST_PLANT_LEAF PIC A(15).
       77 CONST_PLANT_FLOWER PIC A(15).
       77 CONST_HERBIER_LEAF PIC A(15).
       77 CONST_HERBIER_FLOWER PIC A(15).
       77 CONST_HERBIER_MIXTE PIC A(15).
       77 CONST_SAISON_HIVER PIC A(15).
       77 CONST_SAISON_PRINTEMPS PIC A(15).
       77 CONST_SAISON_ETE PIC A(15).
       77 CONST_SAISON_AUTOMNE PIC A(15).

       77 CONST_DISPLAY_MENU PIC A(30).
       77 CONST_ACTION_SENTENCE PIC A(38).
       77 CONST_ACTION_IMPOSSIBLE PIC A(19).

      *> CRs
       77 cr_fplan PIC 9(2).
       77 cr_fher PIC 9(2).
       77 cr_futil PIC 9(2).
       77 cr_fhpl PIC 9(2).

      *> Variables globales
       77 l_h_id PIC 9(3).
       77 wEndOfFile PIC 9(1).
       77 wUtilisateursCount PIC 9(3).
       77 wHerbierCount PIC 9(3).
       77 wMaxUserId PIC 9(3).
       77 wConnectedUser PIC 9(3).
       77 wConnectedUserRole PIC A(15).
       77 wExitProgramme PIC 9(1).
       77 wLoginTrialsCount PIC 9(1).
       77 wPassword PIC A(20).
       77 wIsAnonymous PIC 9(1).
       77 wLogin PIC A(20).
       77 wUniqueLogin PIC 9(1).
       77 wWaitlistEmpty PIC 9(1).

       77 wNoHerbier PIC 9(1).
       77 wSelectedHerbierId PIC 9(3).
       77 wSelectedHerbierType PIC A(20).
       77 wLastHerbierPlantId PIC 9(3).
       77 wNoHerbierPlante PIC 9(3).
       77 wHerbierExists PIC 9(1).
       
       77 wActionChosen PIC 9(2).
       77 wValidInput PIC 9(1).
       77 wExitFunction PIC 9(1).
       77 wInsertionPlante PIC 9(1).
       77 wDeletePlante PIC 9(1).
       77 wHerbierUpdate PIC 9(1).

       77 wLastPlantId PIC 9(3).
       77 wPlanteName PIC A(30).
       77 wPlanteLatinName PIC A(45).
       77 wTypePlante PIC A(15).
       77 wHabitat PIC A(15).
       77 wSaison PIC A(15).
       77 wDuree PIC 9(2).
       77 wUniquePlanteName PIC 9(1).
       77 wUniquePlanteLatinName PIC 9(1).
       77 wNoPlante PIC 9(1).
       77 wSelectedPlanteId PIC 9(3).
       77 wSelectedPlanteHerbierId PIC 9(3).
       77 wDeletePlanteId PIC 9(3).
       77 wPlantCount PIC 9(3).
       77 wPlantTotalCount PIC 9(3).
       77 wPlantExists PIC 9(1).
       77 wCurrentHerbierId PIC 9(3).
       77 wEndOfZone PIC 9(1).
       77 wAvgHerbierByUser PIC 9(3).

       77 wDateMonth PIC A(2).


      *> Variables fonction distanciel
       77 wDistUserType PIC A(15).
       77 wDistMonth PIC A(2).
       77 wDistPlace PIC A(40).
       
       
PROCEDURE DIVISION.

*> Initialisation du programme (opérations à faire avant les interac-
*> tions avec l'utilisateur).
OPEN I-O fplan
IF cr_fplan=35 THEN
        OPEN OUTPUT fplan
END-IF
CLOSE fplan

OPEN I-O fher
IF cr_fher=35 THEN
        OPEN OUTPUT fher
END-IF
CLOSE fher

OPEN I-O futil
IF cr_futil=35 THEN
        OPEN OUTPUT futil
END-IF
CLOSE futil

OPEN I-O fhpl
IF cr_fhpl=35 THEN
        OPEN OUTPUT fhpl
END-IF
CLOSE fhpl

*> Définition des constantes
MOVE "Visiteur" TO CONST_ROLE_READER.
MOVE "En attente" TO CONST_ROLE_WAITING.
MOVE "Editeur" TO CONST_ROLE_EDITOR.
MOVE "Administrateur" TO CONST_ROLE_ADMIN.
MOVE "Professionnel" TO CONST_USER_PRO.
MOVE "Amateur" TO CONST_USER_AMATEUR.
MOVE "Feuille" TO CONST_PLANT_LEAF.
MOVE "Fleur" TO CONST_PLANT_FLOWER.
MOVE "Feuille" TO CONST_HERBIER_LEAF.
MOVE "Fleur" TO CONST_HERBIER_FLOWER.
MOVE "Mixte" TO CONST_HERBIER_MIXTE.
MOVE "Hiver" TO CONST_SAISON_HIVER.
MOVE "Printemps" TO CONST_SAISON_PRINTEMPS.
MOVE "Ete" TO CONST_SAISON_ETE.
MOVE "Automne" TO CONST_SAISON_AUTOMNE.

MOVE "------------------------------" TO CONST_DISPLAY_MENU.
MOVE "Indiquez ce que vous souhaitez faire :" TO CONST_ACTION_SENTENCE.
MOVE "Action impossible !" TO CONST_ACTION_IMPOSSIBLE.

*> Initialisation des valeurs par défaut
MOVE 1 TO wIsAnonymous.
MOVE 0 TO wExitProgramme.
MOVE 0 TO wSelectedHerbierId.
MOVE " " TO wSelectedHerbierType
MOVE CONST_ROLE_READER TO wConnectedUserRole.

*> Insertion automatique d'un untilisateur s'il n'y en a pas déjà.
*> Cette instruction permet aussi de récupérer le dernier identifiant
*> des utilisateurs et leur nombre (voir count_utilisateurs).
PERFORM add_default_user_if_first_start.

*> Récupération des derniers identifiant de chaque fichier
PERFORM last_herbier_id.
PERFORM last_plante_id.
PERFORM last_herbier_plante_id.

*> Écran de bienvenue
DISPLAY "Bienvenue dans l'application Les herbiers, un projet de ",
        "l'association Herb'achat."
DISPLAY " "
DISPLAY "Cette application ne gère pas encore les accents, ainsi, pour",
        " éviter tout inconfort à l'utilisation (notamment des ",
        "décalages graphiques), nous vous recommandons d'éviter d'en ",
        "faire usage."
*> Appel du menu général
PERFORM display_global_menu.

STOP RUN.

*> herbier


       last_herbier_id.
*> Parcourt le fichier herbier à la recherche du plus grand id. Une fois
*> trouvé, on le stocke dans l_h_id
*>
*> Variables utilisées :
*> - l_h_id
*> - wEndOfFile
*> - wHerbierCount
*>
*> Nombre de lectures : Autant qu'il y a d'herbiers dans le fichier fher
       OPEN INPUT fher
       MOVE 0 TO wEndOfFile
       MOVE 0 TO wHerbierCount
       MOVE 0 TO l_h_id
       PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
           READ fher
           AT END MOVE 1 TO wEndOfFile
           NOT AT END
               ADD 1 TO wHerbierCount
               IF fh_id > l_h_id THEN
                   MOVE fh_id TO l_h_id
               END-IF
           END-READ
       END-PERFORM
       CLOSE fher.

       check_herbier_exists.
*> Fonction qui vérifie si il existe un enregistrement pour l'id d'herb-
*> ier contenu dans wSelectedHerbierId.
*>
*> Variables utilisées :
*> - wSelectedHerbierId
*> - wHerbierExists
*>
*> Nombre de lectures : 1
       MOVE wSelectedHerbierId TO fh_id

       OPEN INPUT fher
       READ fher
       INVALID KEY         MOVE 0 TO wHerbierExists
       NOT INVALID KEY     MOVE 1 TO wHerbierExists
       END-READ
       CLOSE fher.
       
       
       add_herbier.
*> Permet d'ajouter un herbier dans le fichier herbier
*> 
*> Variables utilisées :
*> - l_h_id
*> - wHerbierCount
*> - wConnectedUser
*>
*> Nombre de lectures : aucune
       DISPLAY "Nom de l'herbier ?"
       ACCEPT fh_nom
       MOVE wConnectedUser TO fh_utilisateur
       MOVE FUNCTION CURRENT-DATE TO fh_date
       PERFORM WITH TEST AFTER UNTIL fh_type = CONST_HERBIER_LEAF
               OR fh_type = CONST_HERBIER_FLOWER
               OR fh_type = CONST_HERBIER_MIXTE 
           DISPLAY "Type de l'herbier"
           DISPLAY "Feuille/Fleur/Mixte (attention à la majuscule)"
           ACCEPT fh_type
       END-PERFORM
       ADD 1 TO l_h_id
       ADD 1 TO wHerbierCount
       MOVE l_h_id TO fh_id
       OPEN I-O fher
       WRITE tamp_fher
       END-WRITE
       CLOSE fher.
        
       delete_herbier.
*> Permet de supprimer un herbier dans le fichier herbier
*> 
*> Variables utilisées :
*> - l_h_id
*> - wHerbierCount
*> - wEndOfFile
*> - wNoHerbier
*> - wSelectedHerbierId
*>
*> Nombre de lectures : autant qu'il y a de plantes dans l'herbier à
*> supprimer, +1
       MOVE 0 TO wEndOfFile
       MOVE 0 TO wNoHerbier

      *> On commence par supprimer tous les enregistrements de fhpl liés
      *> à l'herbier à supprimer
       OPEN I-O fhpl
       MOVE wSelectedHerbierId TO fhpl_idHerbier
       START fhpl, KEY IS = fhpl_id
       INVALID KEY     MOVE 1 TO wNoHerbier
       NOT INVALID KEY
           PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
               READ fhpl NEXT
               AT END      MOVE 1 TO wEndOfFile
               NOT AT END 
                   IF fhpl_id = wSelectedHerbierId THEN
                       DELETE fhpl RECORD
                   ELSE
                       MOVE 1 TO wEndOfFile
                   END-IF
               END-READ
           END-PERFORM
       END-START
       CLOSE fhpl
         
       OPEN I-O fher
       MOVE 0 TO wNoHerbier
       MOVE wSelectedHerbierId TO fh_id
       READ fher
       INVALID KEY 
           DISPLAY "Cet herbier n'existe pas"
           MOVE 1 TO wNoHerbier
       NOT INVALID KEY 
           ADD -1 TO wHerbierCount
           DELETE fher RECORD
       END-READ
       CLOSE fher.
        
        update_herbier.
*> Permet de mettre un herbier à jour dans le fichier herbier
*> 
*> Variables utilisées :
*> wSelectedHerbierId
*>
*> Nombre de lectures : 1
       MOVE wSelectedHerbierId TO fh_id

       OPEN I-O fher
       READ fher
       NOT INVALID KEY
           PERFORM update_herbier_input
           REWRITE tamp_fher
       END-READ
       CLOSE fher.

       update_herbier_input.
*> Gère les entrées utilisateur pour la modification d'un herbier.
*> ⚠️ Cette fonction ne peut fonctionner que si on a déjà ouvert le
*> fichier herbier et lu jusqu'à l'herbier qui nous intéresse !
*>
*> Variables utilisées :
*> - wActionChosen
*>
*> Nombre de lectures : aucune
       MOVE 0 TO wActionChosen
       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen < 2
           
           IF wActionChosen < 0 OR wActionChosen > 2 THEN
               DISPLAY "Choix impossible"
           END-IF

           DISPLAY "Souhaitez-vous modifier le nom de ",
                   "l'herbier ? (0 : non, 1 : oui)"
           ACCEPT wActionChosen
       END-PERFORM
       
       IF wActionChosen = 1 THEN
           DISPLAY "Entrez le nouveau nom :"
           ACCEPT fh_nom
       END-IF
        
       MOVE 0 TO wActionChosen
       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen < 2
           
           IF wActionChosen < 0 OR wActionChosen > 2 THEN
               DISPLAY "Choix impossible"
           END-IF

           DISPLAY "Souhaitez-vous modifier le type de ",
                   "l'herbier ? (0 : non, 1 : oui)"
           ACCEPT wActionChosen
       END-PERFORM
       
       IF wActionChosen = 1 THEN
           PERFORM WITH TEST AFTER UNTIL fh_type = CONST_HERBIER_LEAF
                   OR fh_type = CONST_HERBIER_FLOWER
                   OR fh_type = CONST_HERBIER_MIXTE 
               DISPLAY "Type de l'herbier"
               DISPLAY "Feuille/Fleur/Mixte (attention à la majuscule)"
               ACCEPT fh_type
           END-PERFORM
       END-IF.
        
       display_all_herbier.
*> Permet d'afficher l'id, le nom et le type de tout les herbiers
*>
*> Variables utilisées :
*> - wEndOfFile
*> - l_h_id
*>
*> Nombre de lectures :
       IF l_h_id = 0 THEN
          DISPLAY "Aucun herbier"
       ELSE
           OPEN INPUT fher
           MOVE 0 TO wEndOfFile
           DISPLAY "ID  | Nom de l'herbier               | ",
               "Type de l'herbier    | ID de l'utilsateur "
           DISPLAY "----|--------------------------------|-",
               "---------------------|--------------------"
           PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
               READ fher
               AT END      MOVE 1 TO wEndOfFile
               NOT AT END 
                  DISPLAY fh_id, " | ", fh_nom, " | ", fh_type, " | ",
                       fh_utilisateur
               END-READ
           END-PERFORM
           CLOSE fher
       END-IF.
        
       display_user_herbier.
*> Permet d'afficher l'id, le nom et le type des herbiers de
*> l'utilisateur courant
*>
*> Variables utilisées :
*> - wEndOfFile
*> - wConnectedUser    
*> - wNoHerbier
*>
*> Nombre de lectures : autant que l'utilisateur n'a d'herbiers
       MOVE 0 TO wEndOfFile
       MOVE 0 TO wNoHerbier
       MOVE wConnectedUser TO fh_utilisateur

       OPEN INPUT fher
       START fher, KEY IS = fh_utilisateur
       INVALID KEY 
           DISPLAY "Vous n'avez pas créé d'herbier"
           MOVE 1 TO wNoHerbier
       NOT INVALID KEY
           DISPLAY "ID  | Nom de l'herbier               | ",
                   "Type de l'herbier    "
           DISPLAY "----|--------------------------------|-",
                   "---------------------"
           PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
               READ fher NEXT
               AT END MOVE 1 TO wEndOfFile
               NOT AT END 
                   IF fh_utilisateur = wConnectedUser THEN
                       DISPLAY fh_id, " | ", fh_nom, " | ", fh_type
                   ELSE
                       MOVE 1 TO wEndOfFile
                   END-IF
               END-READ
           END-PERFORM
       END-START
       CLOSE fher.
       
       display_admin_herbier_menu.
*> Affiche le menu administrateur pour les herbiers.
*>
*> Variables utilisées :
*> - wActionChosen
*> - wIsAnonymous
*> - wConnectedUserRole
*> - wAvgHerbierByUser
*>
*> Nombre de lectures : aucune
       MOVE 0 TO wActionChosen

       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen <= 3
           DISPLAY " "
           DISPLAY CONST_DISPLAY_MENU
           DISPLAY " "

           IF wActionChosen > 3 OR wActionChosen < 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           
           IF wIsAnonymous = 0 THEN
               IF wConnectedUserRole = CONST_ROLE_ADMIN THEN
                   DISPLAY CONST_ACTION_SENTENCE
                   DISPLAY " "
		
                   DISPLAY "        1 : consulter les herbiers"
                   DISPLAY "        2 : gérer un herbier"
                   DISPLAY "        3 : consulter les statistiques"
           
                   DISPLAY " "
                   DISPLAY "        0 : retourner au menu"
                   DISPLAY " "
               END-IF
           ELSE
               EXIT
           END-IF
           ACCEPT wActionChosen
           
           DISPLAY " "
       END-PERFORM

       EVALUATE wActionChosen
       WHEN 1
          *> consulter les herbiers
           PERFORM display_all_herbier
           PERFORM display_admin_herbier_menu
       WHEN 2
          *> gérer un herbier
           PERFORM display_update_herbier_admin
           PERFORM display_admin_herbier_menu
       WHEN 3
          *> statistique 
           PERFORM compute_average_herbier_by_user
           DISPLAY "Nombre d'herbier moyen par utilisateur : ", 
                   wAvgHerbierByUser
           PERFORM display_admin_herbier_menu
       END-EVALUATE.
       
       display_herbier_gesture_menu.
*> Affiche le menu correspondant aux herbiers. Cette fonction s'éxécute
*> lorsqu'un utilisateur est connecté et veux consulter ses herbiers.
*>
*> Variables utilisées :
*> - wActionChosen
*> - wIsAnonymous
*>
*> Nombre de lectures : aucune
       MOVE 0 TO wActionChosen

       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen <= 4
           DISPLAY " "
           DISPLAY CONST_DISPLAY_MENU
           DISPLAY " "

           IF wActionChosen > 4 OR wActionChosen < 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           
           IF wIsAnonymous = 0 THEN
               DISPLAY CONST_ACTION_SENTENCE
               DISPLAY " "
		
               DISPLAY "        1 : consulter ses herbiers"
               DISPLAY "        2 : consulter les plantes d'un herbier"
               DISPLAY "        3 : créer un herbier"
               DISPLAY "        4 : gérer un de ses herbiers"
           
               DISPLAY " "
               DISPLAY "        0 : retourner au menu"
               DISPLAY " "
           ELSE
               EXIT
           END-IF
           ACCEPT wActionChosen
           DISPLAY " "
       END-PERFORM

       EVALUATE wActionChosen
       WHEN 1
          *> consulter ses herbiers
           PERFORM display_user_herbier
           PERFORM display_herbier_gesture_menu
       WHEN 2
          *> consulter les plantes d'un herbier
           PERFORM display_plante_herbier_utilisateur
           PERFORM display_herbier_gesture_menu
       WHEN 3
          *> créer un herbier
           PERFORM add_herbier
           PERFORM display_herbier_gesture_menu
       WHEN 4
          *> gérer ses herbiers
           PERFORM display_update_herbier_utilisateur
           PERFORM display_herbier_gesture_menu
       END-EVALUATE.

       display_herbier_menu.
*> Affiche un menu permettant à n'importe qui de consulter les herbiers
*>
*> Variables utilisées :
*> - wActionChosen
*>
*> Nombre de lectures : aucune
       MOVE 0 TO wActionChosen

       PERFORM display_all_herbier
       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen <= 1
           DISPLAY " "
           DISPLAY CONST_DISPLAY_MENU
           DISPLAY " "

           IF wActionChosen > 1 OR wActionChosen < 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           
           DISPLAY CONST_ACTION_SENTENCE
           DISPLAY " "
		
           DISPLAY "        1 : consulter les plantes d'un herbier"
           
           DISPLAY " "
           DISPLAY "        0 : retourner au menu"
           DISPLAY " "

           ACCEPT wActionChosen
           DISPLAY " "
       END-PERFORM

       EVALUATE wActionChosen
       WHEN 1
          *> consulter ses herbiers
           PERFORM display_plante_herbier
           PERFORM display_herbier_menu
       END-EVALUATE.

       display_plante_herbier.
*> Fonction qui permet de sélectionner un herbier et d'afficher toutes
*> les plantes qu'il contient.
*>
*> Variables utilisées :
*> - wHerbierExists
*> - wSelectedHerbierId
*> - wSelectedHerbierType
*>
*> Nombre de lectures : aucune
       MOVE 1 TO wHerbierExists

       PERFORM WITH TEST AFTER UNTIL wHerbierExists = 1
           IF wHerbierExists = 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE, " Aucun herbier avec ",
                       "cet identifiant n'a pu être trouvé."
           END-IF

           DISPLAY "Entrez l'identifiant de l'herbier que vous ",
                   "souhaitez consulter"
           ACCEPT wSelectedHerbierId
           MOVE " " TO wSelectedHerbierType

           PERFORM check_herbier_exists
       END-PERFORM

       DISPLAY " "
       PERFORM display_herbier_plantes

      *> Cette fonction ne doit pas conserver les éléments sélectionnés
       MOVE 0 TO wSelectedHerbierId
       MOVE " " TO wSelectedHerbierType.
       
       display_plante_herbier_utilisateur.
*> Fonction qui permet de sélectionner un herbier d'un
*> utilisateur puis d'afficher toutes les plantes contenues dedans
*>
*> Variables utilisées :
*> - wNoHerbier
*> - wSelectedHerbierId
*> - wSelectedHerbierType
*> - wHerbierUpdate
*>
*> Nombre de lectures : aucune       
       PERFORM display_user_herbier
       IF wNoHerbier = 0 THEN
       
          PERFORM WITH TEST AFTER UNTIL wHerbierUpdate = 1
                                 
           DISPLAY "Quel identifiant herbier souhaitez vous consulter"
           ACCEPT wSelectedHerbierId
           MOVE " " TO wSelectedHerbierType

           PERFORM check_herbier_be_update
           IF wHerbierUpdate = 0 THEN
              DISPLAY "Vous ne pouvez pas consulter cet herbier"
           END-IF
          END-PERFORM
          DISPLAY " "
          PERFORM display_herbier_plantes
       END-IF
       
      *> Cette fonction ne doit pas conserver les sélecteurs
       MOVE 0 TO wSelectedHerbierId
       MOVE " " TO wSelectedHerbierType.
       
       display_update_herbier_utilisateur.
*> Fonction qui permet de sélectionner un herbier puis afficher un menu
*> proposant des opéarations CRUD sur les plantes de l'herbier et sur
*> l'herbier lui-même.
*> Cette fonction est destinée à être utilisée uniquement pour gérer les
*> herbiers appartenant à l'utilisateur courant (connecté).
*>
*> Variables utilisées :
*> - wNoHerbier
*> - wHerbierUpdate
*> - wSelectedHerbierId
*> - wSelectedHerbierType
*> - wActionChosen
*> - wIsAnonymous
*>
*> Nombre de lectures : aucune
       PERFORM display_user_herbier
       IF wNoHerbier = 0 THEN
       
           IF wSelectedHerbierId = 0 THEN
               PERFORM WITH TEST AFTER UNTIL wHerbierUpdate = 1
                                     
                   DISPLAY "Entrez l'identifiant de l'herbier à gérer"
                   ACCEPT wSelectedHerbierId
                   MOVE " " TO wSelectedHerbierType

                   PERFORM check_herbier_be_update
                   IF wHerbierUpdate = 0 THEN
                       DISPLAY "Vous ne pouvez pas modifier cet herbier"
                   ELSE
                       MOVE fh_type TO wSelectedHerbierType
                   END-IF
               END-PERFORM
           END-IF
           
           DISPLAY " "
           PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                     AND wActionChosen < 5
               DISPLAY " "
               DISPLAY CONST_DISPLAY_MENU
               DISPLAY " "

               IF wActionChosen > 4 OR wActionChosen < 0 THEN
                   DISPLAY CONST_ACTION_IMPOSSIBLE
               END-IF

           
               IF wIsAnonymous = 0 THEN
                   DISPLAY CONST_ACTION_SENTENCE
                   DISPLAY " "
		
                   DISPLAY "        1 : modifier les informations de ",
                           "l'herbier"
                   DISPLAY "        2 : ajouter une plante à l'herbier"
                   DISPLAY "        3 : supprimer une plante de ",
                           "l'herbier"
                   DISPLAY "        4 : supprimer l'herbier"
           
                   DISPLAY " "
                   DISPLAY "        0 : retourner au menu"
                   DISPLAY " "
               ELSE
                   EXIT
               END-IF
               ACCEPT wActionChosen
               DISPLAY " "
           END-PERFORM
       
           EVALUATE wActionChosen
           WHEN 1
              *> modifier les informations de l'herbier
               PERFORM update_herbier
               PERFORM display_update_herbier_utilisateur
           WHEN 2
              *> ajouter une plante à l'herbier
               PERFORM add_herbier_plante
               PERFORM display_update_herbier_utilisateur
           WHEN 3
              *> supprimer une plante de l'herbier
               PERFORM delete_herbier_plante
               PERFORM display_update_herbier_utilisateur
           WHEN 4
              *> supprimerl'herbier
               PERFORM delete_herbier_menu
               IF NOT wSelectedHerbierId = 0 THEN
                   PERFORM display_update_herbier_utilisateur
               END-IF
           END-EVALUATE
       END-IF.
       
       delete_herbier_menu.
*> Fonction qui affiche une confirmation avant de lancer la suppression
*> de l'herbier dans wSelectedHerbierId
*>
*> Variables utilisées :
*> - wActionChosen
*> - wSelectedHerbierId
*>
*> Nombre de lectures : aucune
       PERFORM WITH TEST AFTER UNTIL wActionChosen = 0
                                  OR wActionChosen = 1
                                 
           DISPLAY "Souhaitez vous supprimer l'herbier ? 1 = oui,", 
                   " 0 = non"
           ACCEPT wActionChosen
       END-PERFORM

       IF wActionChosen = 1 THEN
           PERFORM delete_herbier
           MOVE 0 TO wSelectedHerbierId
       END-IF.
       
       display_update_herbier_admin.
*> Fonction qui permet de sélectionner n'importe quel herbier et d'af-
*> ficher un menu pour le gérer (opérations CRUD).
*> Ce menu ne devrait être accessible que pour les administrateurs.
*>
*> Variables utilisées :
*> - wActionChosen
*> - wIsAnonymous
*> - wSelectedHerbierId
*> - wSelectedHerbierType
*> - wNoHerbier
*> - l_h_id
*>
*> Nombre de lectures : aucune
       PERFORM display_all_herbier
       IF wNoHerbier = 0 THEN
           IF wSelectedHerbierId = 0 THEN
               PERFORM WITH TEST AFTER UNTIL wSelectedHerbierId > 0
                                         AND wSelectedHerbierId <=l_h_id
                                 
                   DISPLAY "Quel identifiant herbier souhaitez vous consulter"
                   ACCEPT wSelectedHerbierId
                   MOVE " " TO wSelectedHerbierType

               END-PERFORM
           END-IF
          
           DISPLAY " "
           PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                     AND wActionChosen < 5
               DISPLAY " "
               DISPLAY CONST_DISPLAY_MENU
               DISPLAY " "

               IF wActionChosen > 4 OR wActionChosen < 0 THEN
                   DISPLAY CONST_ACTION_IMPOSSIBLE
               END-IF

           
               IF wIsAnonymous = 0 THEN
                   DISPLAY CONST_ACTION_SENTENCE
                   DISPLAY " "
		
                   DISPLAY "        1 : modifier les informations de ",
                           "l'herbier"
                   DISPLAY "        2 : ajouter une plante à l'herbier"
                   DISPLAY "        3 : supprimer une plante de ",
                           "l'herbier"
                   DISPLAY "        4 : supprimer l'herbier"
           
                   DISPLAY " "
                   DISPLAY "        0 : retourner au menu"
                   DISPLAY " "
               ELSE
                   EXIT
               END-IF
               ACCEPT wActionChosen
               
               DISPLAY " "
           END-PERFORM
       
           EVALUATE wActionChosen
           WHEN 1
              *> modifier les informations de l'herbier
               PERFORM update_herbier
               PERFORM display_update_herbier_admin
           WHEN 2
              *> ajouter une plante à l'herbier
               PERFORM display_herbier_plantes
               PERFORM add_herbier_plante
               PERFORM display_update_herbier_admin
           WHEN 3
              *> supprimer une plante de l'herbier
               PERFORM delete_herbier_plante
               PERFORM display_update_herbier_admin
           WHEN 4
              *> supprimer l'herbier
               PERFORM delete_herbier_menu
               IF NOT wSelectedHerbierId = 0 THEN
                   PERFORM display_update_herbier_admin
               END-IF
           END-EVALUATE
       END-IF.
       
       check_herbier_be_update.
*> Permet de vérifier si l'herbier selectionné peut être modifié par
*> l'utilisateur connecté
*>
*> Variables utilisées :
*> - wConnectedUser
*> - wSelectedHerbierId 
*> - wNoHerbier  
*> - wHerbierUpdate  
*>      
*> Nombre de lectures : 1
       MOVE 0 TO wNoHerbier
       MOVE wSelectedHerbierId TO fh_id

       OPEN INPUT fher
       READ fher
       INVALID KEY 
           DISPLAY "Cet herbier n'existe pas"
           MOVE 1 TO wNoHerbier
       NOT INVALID KEY 
           IF fh_utilisateur = wConnectedUser THEN
               MOVE 1 TO wHerbierUpdate
           ELSE 
               MOVE 0 TO wHerbierUpdate
           END-IF
       END-READ
       CLOSE fher.

       count_utilisateurs.
*> Compte le nombre d'utilisateurs présents dans la fichier utilisateurs
*> et stocke le résultat dans wUtilisateursCount. On en profite pour
*> actualiser la valeur de wMaxUserId, qui sert pour l'insertion.
*>
*> Variables utilisées :
*> - wUtilisateursCount
*> - wEndOfFile
*> - wMaxUserId
*>
*> Nombre de lectures :
*> - Autant qu'il y a d'utilisateurs dans le fichiers utilisateurs
       MOVE 0 TO wEndOfFile
       MOVE 0 TO wUtilisateursCount
       MOVE 0 TO wMaxUserId

       OPEN INPUT futil
       PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
           READ futil
           AT END          MOVE 1 TO wEndOfFile
           NOT AT END
               ADD 1 TO wUtilisateursCount
               IF fu_id > wMaxUserId THEN
                   MOVE fu_id TO wMaxUserId
               END-IF
           END-READ
       END-PERFORM
       CLOSE futil.



       add_default_user_if_first_start.
*> Ajoute l'utilisateur par défaut (cf readme.md) si aucun utilisateur
*> n'est déjà présent dans le fichier
*> 
*> Variables utilisées :
*> - wUtilisateursCount
*>
*> Nombre de lectures :
*> - Autant qu'il y a d'utilisateurs dans le fichier utilisateurs
       PERFORM count_utilisateurs
       IF wUtilisateursCount < 1 THEN
           MOVE 1 TO fu_id
           MOVE "admin" TO fu_login
           MOVE "admin" TO fu_mdp
           MOVE "Administrateur" TO fu_role
           MOVE "Professionnel" TO fu_type

           OPEN I-O futil
           WRITE tamp_futil
           END-WRITE
           CLOSE futil

           ADD 1 TO wUtilisateursCount
       END-IF.

       login.
*> Permet aux utilisateurs de s'authentifier en entrant un nom d'utili-
*> sateur et un mot de passe. ⚠️ Si la combinaison est incorrecte au
*> bout de trois essais, le programme doit se fermer.
*>
*> Variables utilisées :
*> - wConnectedUser
*> - wExitProgramme
*> - wLoginTrialsCount
*> - wPassword
*>
*> Nombre de lectures :
*> 1 maximum à chaque demande (trois au pire) puisqu'il s'agit d'une
*> recherche directe (CSSD)
       MOVE 0 TO wExitProgramme
       MOVE 0 TO wLoginTrialsCount
       DISPLAY "Veuillez entrer votre login, puis votre mot de passe ",
           "après un retour à la ligne. Vous avez trois tentatives ",
           "avant que le logiciel ne se ferme."

       PERFORM WITH TEST AFTER UNTIL wLoginTrialsCount = 3
                              OR NOT wConnectedUser = 0
      *> On déconnecte l'utilisateur (à voir si on garde cette
      *> étape)
           MOVE 0 TO wConnectedUser
           ADD 1 TO wLoginTrialsCount

           IF wLoginTrialsCount > 1 THEN
               DISPLAY "Combinaison non reconnue. Veuillez réessayer :"
           END-IF
     
           ACCEPT fu_login
           ACCEPT wPassword
     
   *> On cherche l'utilisateur pour voir s'il existe, si oui on 
   *> regarde si le mot de passe correspond.
           OPEN INPUT futil
           READ futil
           KEY IS fu_login
               NOT INVALID KEY     MOVE fu_id TO wConnectedUser
               MOVE fu_role TO wConnectedUserRole
           END-READ
           CLOSE futil

           IF NOT wConnectedUser = 0 AND NOT wPassword = fu_mdp
               OR fu_role = CONST_ROLE_WAITING THEN
      *> Mot de passe incorrect ou utilisateur pas encore validé
      *> -> on repasse à 0
               MOVE 0 TO wConnectedUser
           END-IF
       END-PERFORM

       IF wLoginTrialsCount = 3 AND wConnectedUser = 0 THEN
           MOVE 1 TO wExitProgramme
       END-IF
       IF NOT wConnectedUser = 0 THEN
           DISPLAY " "
           DISPLAY "Vous êtes connecté en tant que :"
           PERFORM display_user_info
           DISPLAY " "
       END-IF.

       log_out.
*> Permet de déconnecter l'utilisateur actuellement connecté.
*>
*> Variables utilisées :
*> - wConnectedUser
*> - wIsAnonymous
*> - wConnectedUserRole
*>
*> Nombre de lectures : aucune
       MOVE 0 TO wConnectedUser
       MOVE 1 TO wIsAnonymous
       MOVE CONST_ROLE_READER TO wConnectedUserRole.

       check_connection.
*> Vérifie si l'utilisateur est connecté.
*>
*> Variables utilisées :
*> - wConnectedUser
*> - wIsAnonymous
*>
*> Nombre de lectures : 1
       MOVE 1 TO wIsAnonymous
       MOVE wConnectedUser TO fu_id

       OPEN INPUT futil
       READ futil
       KEY IS fu_id
           NOT INVALID KEY
           IF NOT fu_role = CONST_ROLE_WAITING THEN
               MOVE 0 TO wIsAnonymous
           ELSE
      *> On a détecté un cas qui ne devrait pas arriver, alors
      *> on ferme la connexion par précaution.
               PERFORM log_out
           END-IF
       END-READ
       CLOSE futil.


       display_user_info.
*> Affiche le login (nom) ainsi que le rôle de l'utilisateur actuelle-
*> ment connecté.
*>
*> Variables utilisées :
*> - wIsAnonymous
*>
*> Nombre de lectures : aucune
       PERFORM check_connection

       IF wIsAnonymous = 1 THEN
           DISPLAY "Nom : Anonyme                  Role : Visiteur"
       ELSE
           DISPLAY "Nom : ", fu_login, "           ",
                   "Role : ", fu_role
           DISPLAY "Type : ", fu_type
       END-IF.

       check_unique_login.
*> Vérifie si un login donné via la variable wLogin est unique
*>
*> Variables utilisées :
*> wLogin
*> wUniqueLogin
*> 
*> Nombre de lectures : 1
       MOVE 0 TO wUniqueLogin
       MOVE wLogin TO fu_login

       OPEN INPUT futil
       READ futil
       KEY IS fu_login
           INVALID KEY     MOVE 1 TO wUniqueLogin
       END-READ
       CLOSE futil.

       sign_in.
*> Permet à un utilisateur de poser une candidature pour obtenir un
*> compte.
*>
*> Variables utilisées :
*> - wUniqueLogin
*> - wLogin
*> - wPassword
*> - wMaxUserId
*> - wUtilisateursCount
*>
*> Nombre de lectures : aucune
       MOVE 1 TO wUniqueLogin
       PERFORM WITH TEST AFTER UNTIL wUniqueLogin = 1
           IF wUniqueLogin = 0 THEN
               DISPLAY "Ce nom est déjà pris..."
           END-IF
           DISPLAY "Veuillez entrer un nom d'utilisateur"

           ACCEPT wLogin
           PERFORM check_unique_login
       END-PERFORM

       DISPLAY "Choisissez votre mot de passe"
       ACCEPT wPassword
       DISPLAY " "

      *> Prêt à insérer, on incrémente l'index utilisateur et son compte
       ADD 1 TO wMaxUserId
       ADD 1 TO wUtilisateursCount

       MOVE wMaxUserId TO fu_id
       MOVE wLogin TO fu_login
       MOVE wPassword TO fu_mdp
       MOVE CONST_ROLE_WAITING TO fu_role
       MOVE CONST_USER_AMATEUR TO fu_type

       OPEN I-O futil
       WRITE tamp_futil
       END-WRITE
       CLOSE futil.

       display_waiting_users.
*> Affiche sous la forme d'une table les utilisateurs en attente de va-
*> lidation par un administrateur.
*>
*> Variables utilisées :
*> - wEndOfFile
*> - wWaitlistEmpty
*>
*> Nombre de lectures : Autant qu'il y a d'utilisteurs en attente (lec-
*> ture sur zone)
       MOVE 0 TO wEndOfFile
       MOVE 0 TO wWaitlistEmpty
       MOVE CONST_ROLE_WAITING TO fu_role

       
       OPEN INPUT futil
       START futil, KEY IS = fu_role
       INVALID KEY
           DISPLAY "Aucune inscription à vérifier."
           MOVE 1 TO wWaitlistEmpty
       NOT INVALID KEY
           DISPLAY "ID  | Login choisi         | Role actuel    "
           DISPLAY "----|----------------------|----------------"
           PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
               READ futil NEXT
               AT END      MOVE 1 TO wEndOfFile
               NOT AT END
                   IF fu_role = CONST_ROLE_WAITING THEN
                       DISPLAY fu_id, " | ", fu_login, " | ",
                               fu_role
                   ELSE
                       MOVE 1 TO wEndOfFile
                   END-IF
               END-READ
           END-PERFORM
       END-START
       CLOSE futil.

       display_waiting_users_menu.
*> Affiche le menu (table + actions) pour gérer les utilistaeurs en at-
*> tente.
*>
*> Variables utilisées :
*> - wActionChosen
*> - wWaitlistEmpty
*> 
*> Nombre de lectures : aucune
       MOVE 0 TO wActionChosen

       PERFORM WITH TEST AFTER UNTIL wActionChosen < 4
                                 AND wActionChosen >= 0
           DISPLAY " "
           DISPLAY CONST_DISPLAY_MENU
           PERFORM display_waiting_users
           DISPLAY " "

           IF wActionChosen > 3 OR wActionChosen < 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF
    
           DISPLAY CONST_ACTION_SENTENCE
           DISPLAY " "
           
           IF wWaitlistEmpty = 0 THEN
               DISPLAY "        1 : accepter en tant qu'",
                       CONST_ROLE_EDITOR
               DISPLAY "        2 : accpeter en tant qu'",
                       CONST_ROLE_ADMIN
               DISPLAY "        3 : refuser (supprimera la candidature)"
               DISPLAY " "
           END-IF
           DISPLAY "        0 : retourner à la page d'acceuil"
           DISPLAY " "
    
           ACCEPT wActionChosen
           DISPLAY " "
       END-PERFORM

       EVALUATE wActionChosen
       WHEN 1
          *> passage de l'utilisateur en tant qu'éditeur
           PERFORM accept_user_editor
           PERFORM display_waiting_users_menu
       WHEN 2
          *> passage de l'utilisateur en tant qu'administrateur
           PERFORM accept_user_admin
           PERFORM display_waiting_users_menu
       WHEN 3
          *> suppression de l'utilisateur
           PERFORM deny_user
           PERFORM display_waiting_users_menu
       END-EVALUATE.

       accept_user_editor.
*> Change le rôle d'un utilisateur donné pour le rôle Éditeur
*>
*> Variables utilisées :
*> - wValidInput
*> - wLogin
*> - wExitFunction
*>
*> Nombre de lectures : une pour récupérer l'utilisateur dont le login
*> est entré.
       MOVE 1 TO wValidInput
       MOVE 0 TO wExitFunction
       
       DISPLAY CONST_DISPLAY_MENU
       PERFORM WITH TEST AFTER UNTIL wValidInput = 1
           IF wValidInput = 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           DISPLAY "Entrez le login de l'utilisateur à accepter en ",
                   "tant qu'", CONST_ROLE_EDITOR, " ou 0 pour revenir ",
                   "en arrière."
           ACCEPT wLogin
           DISPLAY " "

           MOVE wLogin TO fu_login
           OPEN INPUT futil
           READ futil
           KEY IS fu_login
               INVALID KEY     MOVE 0 TO wValidInput
               NOT INVALID KEY
                   IF fu_role = CONST_ROLE_WAITING THEN
                       MOVE 1 TO wValidInput
                    END-IF
           END-READ
           CLOSE futil

           IF wLogin = "0" THEN
               MOVE 1 TO wValidInput
               MOVE 1 TO wExitFunction
           END-IF
       END-PERFORM

       IF wExitFunction = 0 THEN
           OPEN I-O futil
           READ futil
           NOT INVALID KEY
               MOVE CONST_ROLE_EDITOR TO fu_role
               REWRITE tamp_futil
           END-READ
           CLOSE futil
       END-IF.

       accept_user_admin.
*> Change le rôle d'un utilisateur donné pour le rôle Administrateur
*>
*> Variables utilisées :
*> - wValidInput
*> - wLogin
*> - wExitFunction
*>
*> Nombre de lectures : une pour récupérer l'utilisateur dont le login
*> est entré.
       MOVE 1 TO wValidInput
       MOVE 0 TO wExitFunction

       DISPLAY CONST_DISPLAY_MENU
       PERFORM WITH TEST AFTER UNTIL wValidInput = 1
           IF wValidInput = 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           DISPLAY "Entrez le login de l'utilisateur à accepter en ",
                   "tant qu'", CONST_ROLE_ADMIN, " ou 0 pour revenir ",
                   "en arrière."
           ACCEPT wLogin
           DISPLAY " "

           MOVE wLogin TO fu_login
           OPEN INPUT futil
           READ futil
           KEY IS fu_login
               INVALID KEY     MOVE 0 TO wValidInput
               NOT INVALID KEY
                   IF fu_role = CONST_ROLE_WAITING THEN
                       MOVE 1 TO wValidInput
                    END-IF
           END-READ
           CLOSE futil

           IF wLogin = "0" THEN
               MOVE 1 TO wValidInput
               MOVE 1 TO wExitFunction
           END-IF
       END-PERFORM

       IF wExitFunction = 0 THEN
           OPEN I-O futil
           READ futil
           NOT INVALID KEY
               MOVE CONST_ROLE_ADMIN TO fu_role
               REWRITE tamp_futil
           END-READ
           CLOSE futil
       END-IF.

       deny_user.
*> Supprime un utilisateur en attente
*>
*> Variables utilisées :
*> - wValidInput
*> - wLogin
*> - wExitFunction
*> - wUtilisateursCount
*>
*> Nombre de lectures : 1
       MOVE 1 TO wValidInput
       MOVE 0 TO wExitFunction

       DISPLAY CONST_DISPLAY_MENU
       PERFORM WITH TEST AFTER UNTIL wValidInput = 1
           IF wValidInput = 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           DISPLAY "Entrez le login de l'utilisateur à supprimer ou 0 ",
                   "pour revenir en arrière."
           ACCEPT wLogin
           DISPLAY " "

           MOVE wLogin TO fu_login
           OPEN INPUT futil
           READ futil
           KEY IS fu_login
               INVALID KEY     MOVE 0 TO wValidInput
               NOT INVALID KEY
                   IF fu_role = CONST_ROLE_WAITING THEN
                       MOVE 1 TO wValidInput
                    END-IF
           END-READ
           CLOSE futil

           IF wLogin = "0" THEN
               MOVE 1 TO wValidInput
               MOVE 1 TO wExitFunction
           END-IF
       END-PERFORM

       IF wExitFunction = 0 THEN
           OPEN I-O futil
           READ futil
           NOT INVALID KEY
               DELETE futil RECORD
               ADD -1 TO wUtilisateursCount
           END-READ
           CLOSE futil
       END-IF.

       display_users.
*> Affiche sous la forme d'une table les utilisateurs de l'application
*>
*> Variables utilisées :
*> - wEndOfFile
*>
*> Nombre de lectures : Autant qu'il y a d'utilisteurs dans l'appli
       MOVE 0 TO wEndOfFile

       DISPLAY "ID  | Login                | Role            | ",
               "Type           "
       DISPLAY "----|----------------------|-----------------|-",
               "---------------"

       OPEN INPUT futil
       PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
           READ futil
           AT END          MOVE 1 TO wEndOfFile
           NOT AT END
               DISPLAY fu_id, " | ", fu_login, " | ", fu_role, " | ",
                       fu_type
           END-READ
       END-PERFORM
       CLOSE futil.

       display_users_menu.
*> Affiche le menu de gestion des utilisateurs. Avant d'exécuter cette
*> fonction, il est nécessaire de vérifier que l'utilisateur connecté
*> est bien un administrateur !
*>
*> Variables utilisées :
*> - wActionChosen
*>
*> Nombre de lectures : aucune
       MOVE 0 TO wActionChosen

       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen < 3
           DISPLAY " "
           DISPLAY CONST_DISPLAY_MENU
           PERFORM display_users
           DISPLAY " "

           IF wActionChosen > 2 OR wActionChosen < 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           DISPLAY CONST_ACTION_SENTENCE
           DISPLAY " "

           DISPLAY "        1 : modifer un utilisateur"
           DISPLAY "        2 : supprimer un utilisateur"
           DISPLAY " "
           DISPLAY "        0 : retourner à la page d'acceuil"
           DISPLAY " "

           ACCEPT wActionChosen
           DISPLAY " "
       END-PERFORM

       EVALUATE wActionChosen
       WHEN 0
          *> retour à la page d'accueil
       WHEN 1
          *> modification d'un utilisateur
           PERFORM update_user_menu
           PERFORM display_users_menu
       WHEN 2
          *> suppression d'un utilisateur
           PERFORM delete_user_menu
           PERFORM display_users_menu
       END-EVALUATE.

       display_account_menu.
*> Affiche le menu de gestion de son propre compte (consultation & édi-
*> tion des infos)
*>
*> Variables utilisées :
*> - wActionChosen
*>
*> Nombre de lectures : aucune
       MOVE 0 TO wActionChosen

       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen < 3
           DISPLAY " "
           DISPLAY CONST_DISPLAY_MENU
           PERFORM display_user_info
           DISPLAY " "

           IF wActionChosen > 2 OR wActionChosen < 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           DISPLAY CONST_ACTION_SENTENCE
           DISPLAY " "

           DISPLAY "        1 : modifer le nom d'utilisateur"
           DISPLAY "        2 : modifier le mot de passe"
           DISPLAY " "
           DISPLAY "        0 : retourner à la page d'acceuil"
           DISPLAY " "

           ACCEPT wActionChosen
           DISPLAY " "
       END-PERFORM

       EVALUATE wActionChosen
       WHEN 1
          *> modification du login
           PERFORM update_self_login
           PERFORM display_account_menu
       WHEN 2
          *> suppression du mot de passe
           PERFORM update_self_password
           PERFORM display_account_menu
       END-EVALUATE.

       update_self_login.
*> Permet de modifier son propre login.
*>
*> Variables utilisées :
*> - wLogin
*> - wConnectedUser
*> - wUniqueLogin
*>
*> Nombre de lectures : Une.
       MOVE 1 TO wUniqueLogin
           
       PERFORM WITH TEST AFTER UNTIL wUniqueLogin = 1
           IF wUniqueLogin = 0 THEN
               DISPLAY "Ce nom d'utilisateur est déjà utilisé."
           END-IF
           DISPLAY "Entrez votre nouveau nom d'utilisateur"
           ACCEPT wLogin
    
           PERFORM check_unique_login
       END-PERFORM

       MOVE wConnectedUser TO fu_id
       OPEN I-O futil
       READ futil
       NOT INVALID KEY
           MOVE wLogin TO fu_login
           REWRITE tamp_futil
       END-READ
       CLOSE futil.

       update_self_password.
*> Permet de modifier son propre mot de passe.
*>
*> Variables utilisées :
*> - wPassword
*> - wConnectedUser
*>
*> Nombre de lectures : Une.
       DISPLAY "Choisissez votre nouveau mot de passe (20 caractères ",
               "max)"
       ACCEPT wPassword
       DISPLAY " "

       MOVE wConnectedUser TO fu_id
       OPEN I-O futil
       READ futil
       NOT INVALID KEY
           MOVE wPassword TO fu_mdp
           REWRITE tamp_futil
       END-READ
       CLOSE futil.

       update_user_menu.
*> Permet de sélectionner l'utilisateur à modifier
*>
*> Variables utilisées :
*> - wValidInput
*> - wExitFunction
*> - wLogin
*>
*> Nombre de lectures : 2
       MOVE 1 TO wValidInput
       MOVE 0 TO wExitFunction
       
       DISPLAY CONST_DISPLAY_MENU
       PERFORM WITH TEST AFTER UNTIL wValidInput = 1
           IF wValidInput = 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           DISPLAY "Entrez le login de l'utilisateur à modifier ou 0 ",
                   "pour revenir en arrière."
           ACCEPT wLogin
           DISPLAY " "

           MOVE wLogin TO fu_login
           OPEN INPUT futil
           READ futil
           KEY IS fu_login
               INVALID KEY     MOVE 0 TO wValidInput
               NOT INVALID KEY
                   MOVE 1 TO wValidInput
           END-READ
           CLOSE futil

           IF wLogin = "0" THEN
               MOVE 1 TO wValidInput
               MOVE 1 TO wExitFunction
           END-IF
       END-PERFORM

       IF wExitFunction = 0 THEN
           OPEN I-O futil
           READ futil
           NOT INVALID KEY
               PERFORM update_user
               REWRITE tamp_futil
           END-READ
           CLOSE futil
       END-IF.

       update_user.
*> Permet de modifier un utilisateur. Cependant, l'ID et le login ne
*> peuvent pas être modifiés !
*>
*> Variables utilisées :
*> - wActionChosen
*> 
*> Nombre de lectures : aucune
       MOVE 0 TO wActionChosen
       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen < 2
           
           IF wActionChosen < 0 OR wActionChosen > 2 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           DISPLAY "Souhaitez-vous modifier le mot de passe de ",
                   "l'utilisateur ? (0 : non, 1 : oui)"
           ACCEPT wActionChosen
       END-PERFORM

       IF wActionChosen = 1 THEN
           DISPLAY "Entrez le nouveau mot de passe (20 caractères max)",
                   " :"
           ACCEPT fu_mdp
       END-IF

       MOVE 0 TO wActionChosen
       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen < 2
           
           IF wActionChosen < 0 OR wActionChosen > 2 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           DISPLAY "Souhaitez-vous modifier le rôle de l'utilisateur ?",
                   " (0 : non, 1 : oui)"
           ACCEPT wActionChosen
       END-PERFORM

       IF wActionChosen = 1 THEN
           PERFORM WITH TEST AFTER UNTIL fu_role = CONST_ROLE_EDITOR
                                      OR fu_role = CONST_ROLE_ADMIN
               
               IF NOT fu_role = CONST_ROLE_EDITOR 
                       OR NOT fu_role = CONST_ROLE_ADMIN THEN
                   DISPLAY CONST_ACTION_IMPOSSIBLE
               END-IF

              DISPLAY "Entrez le nouveau rôle (", CONST_ROLE_EDITOR, "/",
                       CONST_ROLE_ADMIN, ") :"
               ACCEPT fu_role
           END-PERFORM
       END-IF

       MOVE 0 TO wActionChosen
       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen < 2
           
           IF wActionChosen < 0 OR wActionChosen > 2 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           DISPLAY "Souhaitez-vous modifier le type de l'utilisateur ?",
                   " (0 : non, 1 : oui)"
           ACCEPT wActionChosen
       END-PERFORM

       IF wActionChosen = 1 THEN
           PERFORM WITH TEST AFTER UNTIL fu_type = CONST_USER_AMATEUR
                                      OR fu_type = CONST_USER_PRO
               
               IF NOT fu_type = CONST_USER_AMATEUR 
                       OR NOT fu_type = CONST_USER_PRO THEN
                   DISPLAY CONST_ACTION_IMPOSSIBLE
               END-IF

               DISPLAY "Entrez le nouveau type (", CONST_USER_AMATEUR, 
                       "/", CONST_USER_PRO, ") :"
               ACCEPT fu_type
           END-PERFORM
       END-IF.

       delete_user_menu.
*> Permet de supprimer un utilisateur. Cependant, il est impossible de
*> supprimer l'utilisateur actuellement connecté.
*> 
*> Variables utilisées :
*> - wValidInput
*> - wExitFunction
*> - wLogin
*> - wConnectedUser
*>
*> Nombre de lectures : 2
       MOVE 1 TO wValidInput
       MOVE 0 TO wExitFunction
       
       DISPLAY CONST_DISPLAY_MENU
       PERFORM WITH TEST AFTER UNTIL wValidInput = 1
           IF wValidInput = 0 AND NOT fu_id = wConnectedUser THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF
           IF wValidInput = 0 AND fu_id = wConnectedUser THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE, " Vous ne pouvez pas ",
                       "supprimer l'utilisateur auquel vous êtes ",
                       "actuellement connecté."
           END-IF

           DISPLAY "Entrez le login de l'utilisateur à supprimer ou 0 ",
                   "pour revenir en arrière."
           ACCEPT wLogin
           DISPLAY " "

           MOVE wLogin TO fu_login
           OPEN INPUT futil
           READ futil
           KEY IS fu_login
               INVALID KEY     MOVE 0 TO wValidInput
               NOT INVALID KEY
                   IF NOT fu_id = wConnectedUser THEN
                       MOVE 1 TO wValidInput
                   ELSE
                       MOVE 0 TO wValidInput
                   END-IF
           END-READ
           CLOSE futil

           IF wLogin = "0" THEN
               MOVE 1 TO wValidInput
               MOVE 1 TO wExitFunction
           END-IF
       END-PERFORM

       IF wExitFunction = 0 THEN
           OPEN I-O futil
           READ futil
           NOT INVALID KEY
               DELETE futil RECORD
               ADD -1 TO wUtilisateursCount
           END-READ
           CLOSE futil
       END-IF.
       
       display_admin_user_menu.
*> Affiche le menu de gestion des utilisateurs. Avant d'exécuter cette
*> fonction, il est nécessaire de vérifier que l'utilisateur connecté
*> est bien un administrateur !
*>
*> Variables utilisées :
*> - wActionChosen
*>
*> Nombre de lectures : aucune
       MOVE 0 TO wActionChosen

       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen < 3
           DISPLAY " "
           DISPLAY CONST_DISPLAY_MENU
           PERFORM display_users
           DISPLAY " "

           IF wActionChosen > 2 OR wActionChosen < 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           DISPLAY CONST_ACTION_SENTENCE
           DISPLAY " "

           DISPLAY "        1 : gérer les utilisateurs en attente"
           DISPLAY "        2 : gérer tous les utilisateurs"
           DISPLAY " "
           DISPLAY "        0 : retourner à la page d'acceuil"
           DISPLAY " "

           ACCEPT wActionChosen
           DISPLAY " "
       END-PERFORM

       EVALUATE wActionChosen
       WHEN 1
*> modification d'un utilisateur
           PERFORM display_waiting_users_menu
           PERFORM display_admin_user_menu
       WHEN 2
*> suppression d'un utilisateur
           PERFORM display_users_menu
           PERFORM display_admin_user_menu
       END-EVALUATE.

       last_plante_id.
*> Parcourt le fichier plante à la recherche du plus grand id. Une fois
*> trouvé, on le stocke dans wLastPlantId
*>
*> Variables utilisées :
*> - wLastPlantId
*> - wEndOfFile
*>
*> Nombre de lectures : autant qu'il y a de plantes dans le fichier
*> plante.
       MOVE 0 TO wEndOfFile
       MOVE 0 TO wLastPlantId

       OPEN INPUT fplan
       PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
           READ fplan
           AT END MOVE 1 TO wEndOfFile
           NOT AT END
               IF pl_id > wLastPlantId THEN
                   MOVE pl_id TO wLastPlantId
               END-IF
           END-READ
       END-PERFORM
       CLOSE fplan.

       check_plant_exists.
*> Fonction qui vérifie si il existe un enregistrement pour l'id de
*> plante contenu dans wSelectedPlanteId.
*>
*> Variables utilisées :
*> - wSelectedPlanteId
*> - wPlantExists
*>
*> Nombre de lectures : 1
       MOVE wSelectedPlanteId TO pl_id

       OPEN INPUT fplan
       READ fplan
       INVALID KEY         MOVE 0 TO wPlantExists
       NOT INVALID KEY     MOVE 1 TO wPlantExists
       END-READ
       CLOSE fplan.

       check_unique_plante_name.
*> Vérifie que le nom de plante dans wPlanteName est unique. Le résultat
*> est stocké dans wUniquePlanteName
*>
*> Variables utilisées :
*> - wUniquePlanteName
*> - wPlanteName
*>
*> Nombre de lectures : 1
       MOVE 0 TO wUniquePlanteName
       MOVE wPlanteName TO pl_nom

       OPEN INPUT fplan
       READ fplan
       KEY IS pl_nom
           INVALID KEY     MOVE 1 TO wUniquePlanteName
       END-READ
       CLOSE fplan.

       check_unique_plante_latin_name.
*> Vérifie que le nom latin de plante dans wPlanteLatinName est unique.
*> Le résultat est stocké dans wUniquePlanteLatinName
*>
*> Variables utilisées :
*> - wUniquePlanteLatinName
*> - wPlanteLatinName
*>
*> Nombre de lectures : 1
       MOVE 0 TO wUniquePlanteLatinName
       MOVE wPlanteLatinName TO pl_nomLatin

       OPEN INPUT fplan
       READ fplan
       KEY IS pl_nomLatin
           INVALID KEY     MOVE 1 TO wUniquePlanteLatinName
       END-READ
       CLOSE fplan.

       keep_going.
*> Demande à l'utilisateur s'il souhaite continuer l'action qu'il a
*> commencée ou non. Si il choisit de retourner en arrière,
*> wExitFunction prendra la valeur 1, et il faudra le précipiter vers
*> la sortie de la fonction.
*>
*> Variables utilisées :
*> wActionChosen
*> wExitFunction
*>
*> Nombre de lectures : aucune
       PERFORM WITH TEST AFTER UNTIL wActionChosen < 2
                                 AND wActionChosen >= 0
           DISPLAY CONST_ACTION_SENTENCE
           DISPLAY " "
           DISPLAY "        1 : réessayer"
           DISPLAY " "
           DISPLAY "        0 : retour en arrière"
           DISPLAY " "
    
           ACCEPT wActionChosen
           DISPLAY " "
       END-PERFORM

       IF wActionChosen = 0 THEN
           MOVE 1 TO wExitFunction
       END-IF.

       add_plante.
*> Permet d'ajouter une plante dans le fichier plante
*> 
*> Variables utilisées :
*> - wUniquePlanteName
*> - wUniquePlanteLatinName
*> - wTypePlante
*> - wSaison
*> - wDuree
*> - wExitFunction
*> - wPlanteName
*> - wPlanteLatinName
*> - wHabitat
*> - wLastPlantId
*>
*> Nombre de lectures : aucune
       MOVE 1 TO wUniquePlanteName
       MOVE 1 TO wUniquePlanteLatinName
       MOVE CONST_SAISON_HIVER TO wSaison
       MOVE CONST_PLANT_LEAF TO wTypePlante
       MOVE 0 TO wExitFunction
       PERFORM WITH TEST AFTER UNTIL wUniquePlanteName = 1
                                  OR wExitFunction = 1
           IF wUniquePlanteName = 0 THEN
               DISPLAY "Ce nom de plante existe déjà."
               DISPLAY " "
               PERFORM keep_going
           END-IF

           IF NOT wExitFunction = 1 THEN
               DISPLAY "Veuillez saisir le nom de votre plante"
               ACCEPT wPlanteName
               PERFORM check_unique_plante_name
           END-IF
       END-PERFORM

       IF NOT wExitFunction = 1 THEN
           PERFORM WITH TEST AFTER UNTIL wUniquePlanteLatinName = 1
                                      OR wExitFunction = 1
               IF wUniquePlanteLatinName = 0 THEN
                   DISPLAY "Ce nom latin de plante existe déjà."
                   DISPLAY " "
                   PERFORM keep_going
               END-IF
    
               IF NOT wExitFunction = 1 THEN
                   DISPLAY "Veuillez saisir le nom latin de votre ",
                           "plante"
                   ACCEPT wPlanteLatinName
                   PERFORM check_unique_plante_latin_name
               END-IF
           END-PERFORM
       END-IF

       IF NOT wExitFunction = 1 THEN
           PERFORM WITH TEST AFTER UNTIL wTypePlante = CONST_PLANT_LEAF
                                    OR wTypePlante = CONST_PLANT_FLOWER
               IF NOT wTypePlante = CONST_PLANT_LEAF AND
                  NOT wTypePlante = CONST_PLANT_FLOWER
               THEN
                   DISPLAY "Type incorrect."
               END-IF

               DISPLAY "Veuillez saisir le type de plante dont il ",
                       "s'agit."
               DISPLAY CONST_PLANT_FLOWER, CONST_PLANT_LEAF, 
                       " (attention à la majuscule)"
               ACCEPT wTypePlante
           END-PERFORM
           
           DISPLAY "Quel est l'habitat naturel de la plante ? (désert,", 
                   " montagne, prairies, forêt, tropiques...)"
           ACCEPT wHabitat

           PERFORM WITH TEST AFTER UNTIL wSaison = CONST_SAISON_HIVER
                                     OR wSaison = CONST_SAISON_PRINTEMPS
                                      OR wSaison = CONST_SAISON_ETE
                                      OR wSaison = CONST_SAISON_AUTOMNE
               IF NOT wSaison = CONST_SAISON_HIVER AND
                  NOT wSaison = CONST_SAISON_PRINTEMPS AND
                  NOT wSaison = CONST_SAISON_ETE AND
                  NOT wSaison = CONST_SAISON_AUTOMNE
               THEN
                   DISPLAY "Saison incorrecte."
               END-IF

               DISPLAY "Veuillez entrez la saison associée à votre ",
                       "plante."
               DISPLAY "Pour une fleur, entrez la saison à laquelle la",
                       " plante fleurit. Poue une feuille, entrez la",
                       " saison à laquelle on en voit les premier",
                       " plants émerger."
               DISPLAY CONST_SAISON_HIVER, CONST_SAISON_PRINTEMPS,
                       CONST_SAISON_ETE, CONST_SAISON_AUTOMNE,
                       " (attention à la majuscule et aux accents)"
               ACCEPT wSaison
           END-PERFORM

           DISPLAY "Quel est la durée de séchage recommendée (en jours",
                   ") pour la plante ?"
           ACCEPT wDuree

           ADD 1 TO wLastPlantId
           MOVE wLastPlantId TO pl_id
           MOVE wPlanteName TO pl_nom
           MOVE wPlanteLatinName TO pl_nomLatin
           MOVE wTypePlante TO pl_type
           MOVE wHabitat TO pl_habitat
           MOVE wSaison TO pl_saison
           MOVE wDuree TO pl_duree

           OPEN I-O fplan
           WRITE tamp_fplan
           END-WRITE
           CLOSE fplan
       END-IF.

       display_plantes_table_header.
*> Simple fonction d'affichage pour mutualiser l'impression de l'en-tête
*> de table d'affichage pour les plantes
*>
*> Variables utilisées : aucune
*>
*> Nombre de lectures : aucune
       DISPLAY "ID  | Nom                            | ",
               "Nom latin                                     | ",
               "Type            | Habitat naturel | Saison          | ",
               "Durée de séchage"
       DISPLAY "----|--------------------------------|-",
               "----------------------------------------------|-",
               "----------------|-----------------|-----------------|-",
               "----------------".

       display_plantes_table_line.
*> Simple fonction d'affichage pour mutualiser l'impression des lignes
*> de table d'affichage pour les plantes
*>
*> Variables utilisées : aucune
*>
*> Nombre de lectures : aucune
       DISPLAY pl_id, " | ", pl_nom, " | ", pl_nomLatin, " | ",
               pl_type, " | ", pl_habitat, " | ", pl_saison, " | ",
               pl_duree, " jours".

       display_plantes.
*> Affiche sous la forme d'une table les plantes de l'application
*>
*> Variables utilisées :
*> - wEndOfFile
*>
*> Nombre de lectures : Autant qu'il y a de plantes dans l'appli
       MOVE 0 TO wEndOfFile

       PERFORM display_plantes_table_header

       OPEN INPUT fplan
       PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
           READ fplan
           AT END          MOVE 1 TO wEndOfFile
           NOT AT END
               PERFORM display_plantes_table_line
           END-READ
       END-PERFORM
       CLOSE fplan.

       display_plantes_type.
*> Affiche sous la forme d'une table les plantes de l'application dont
*> le type correspond à celui dans wTypePlante
*>
*> Variables utilisées :
*> - wTypePlante
*> - wEndOfFile
*>
*> Nombre de lectures : autant qu'il y a de plantes du type donné dans
*> le fichier plante
       MOVE 0 TO wEndOfFile
       MOVE wTypePlante TO pl_type

       
       OPEN INPUT fplan
       START fplan, KEY IS = pl_type
       INVALID KEY
           DISPLAY "Aucune plante pour le type cherché"
       NOT INVALID KEY
           PERFORM display_plantes_table_header
           PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
               READ fplan NEXT
               AT END      MOVE 1 TO wEndOfFile
               NOT AT END
                   IF pl_type = wTypePlante THEN
                       PERFORM display_plantes_table_line
                   ELSE
                       MOVE 1 TO wEndOfFile
                   END-IF
               END-READ
           END-PERFORM
       END-START
       CLOSE fplan.

       update_plante.
*> Permet de mettre une plante à jour dans le fichier plante.
*>
*> Variables utilisées :
*> - wSelectedPlanteId
*>
*> Nombre de lectures : 1
       MOVE wSelectedPlanteId TO pl_id

       OPEN I-O fplan
       READ fplan
       NOT INVALID KEY
           PERFORM update_plante_input
           REWRITE tamp_fplan
       END-READ
       CLOSE fplan.
       
       update_plante_input.
*> Gère les entrées utilisateur pour la modification d'une plante.
*> ⚠️ Cette fonction ne peut fonctionner que si on a déjà ouvert le
*> fichier plante et lu jusqu'à la plante qui nous intéresse !
*> 
*> Variables utilisées :
*> - wActionChosen
*> - wUniquePlanteName
*> - wExitFunction
*> - wPlanteName
*> - wUniquePlanteLatinName
*> - wPlanteLatinName
*>
*> Nombre de lectures : aucune
       MOVE 0 TO wActionChosen
       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen < 2
           
           IF wActionChosen < 0 OR wActionChosen > 2 THEN
               DISPLAY "Choix impossible"
           END-IF

           DISPLAY "Souhaitez-vous modifier le nom de ",
                   "la plante ? (0 : non, 1 : oui)"
           ACCEPT wActionChosen
       END-PERFORM
       
       IF wActionChosen = 1 THEN
           MOVE 1 TO wUniquePlanteName
           MOVE 0 TO wExitFunction
           PERFORM WITH TEST AFTER UNTIL wUniquePlanteName = 1
                                          OR wExitFunction = 1
               IF wUniquePlanteName = 0 THEN
                   DISPLAY "Ce nom de plante existe déjà."
                   DISPLAY " "
                   PERFORM keep_going
               END-IF

               IF NOT wExitFunction = 1 THEN
                   DISPLAY "Veuillez saisir le nom de votre plante"
                   ACCEPT wPlanteName
                   PERFORM check_unique_plante_name
               END-IF
           END-PERFORM

           IF wExitFunction = 0 THEN
               MOVE wPlanteName TO pl_nom
           END-IF
       END-IF
        
       MOVE 0 TO wActionChosen
       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen < 2
           
           IF wActionChosen < 0 OR wActionChosen > 2 THEN
               DISPLAY "Choix impossible"
           END-IF

           DISPLAY "Souhaitez-vous modifier le nom latin de ",
                   "la plante ? (0 : non, 1 : oui)"
           ACCEPT wActionChosen
       END-PERFORM
       
       IF wActionChosen = 1 THEN
           MOVE 1 TO wUniquePlanteLatinName
           MOVE 0 TO wExitFunction
           PERFORM WITH TEST AFTER UNTIL wUniquePlanteLatinName = 1
                                      OR wExitFunction = 1
               IF wUniquePlanteLatinName = 0 THEN
                   DISPLAY "Ce nom latin de plante existe déjà."
                   DISPLAY " "
                   PERFORM keep_going
               END-IF
    
               IF NOT wExitFunction = 1 THEN
                   DISPLAY "Veuillez saisir le nom latin de votre ",
                           "plante"
                   ACCEPT wPlanteLatinName
                   PERFORM check_unique_plante_latin_name
               END-IF
           END-PERFORM

           IF wExitFunction = 0 THEN
               MOVE wPlanteLatinName TO pl_nomLatin
           END-IF
       END-IF
       
       MOVE 0 TO wActionChosen
       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen < 2
           
           IF wActionChosen < 0 OR wActionChosen > 2 THEN
               DISPLAY "Choix impossible"
           END-IF

           DISPLAY "Souhaitez-vous modifier l'habitat de ",
                   "la plante ? (0 : non, 1 : oui)"
           ACCEPT wActionChosen
       END-PERFORM
       
       IF wActionChosen = 1 THEN
           DISPLAY "Entrez le nouvel habitat :"
           ACCEPT pl_habitat
       END-IF
        
       MOVE 0 TO wActionChosen
       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen < 2
           
           IF wActionChosen < 0 OR wActionChosen > 2 THEN
               DISPLAY "Choix impossible"
           END-IF

           DISPLAY "Souhaitez-vous modifier la saison de ",
                   "la plante ? (0 : non, 1 : oui)"
           ACCEPT wActionChosen
       END-PERFORM
       
        IF wActionChosen = 1 THEN
           DISPLAY "Entrez la nouvelle saison :"
           ACCEPT pl_saison
       END-IF
       
       MOVE 0 TO wActionChosen
       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen < 2
           
           IF wActionChosen < 0 OR wActionChosen > 2 THEN
               DISPLAY "Choix impossible"
           END-IF

           DISPLAY "Souhaitez-vous modifier la durée de ",
                   "séchage la plante ? (0 : non, 1 : oui)"
           ACCEPT wActionChosen
       END-PERFORM
       
        IF wActionChosen = 1 THEN
           DISPLAY "Entrez la nouvelle durée de séchage :"
           ACCEPT pl_duree
       END-IF
       
       MOVE 0 TO wActionChosen
       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen < 2
           
           IF wActionChosen < 0 OR wActionChosen > 2 THEN
               DISPLAY "Choix impossible"
           END-IF

           DISPLAY "Souhaitez-vous modifier le type de ",
                   "la plante ? (0 : non, 1 : oui)"
           ACCEPT wActionChosen
       END-PERFORM
       
       IF wActionChosen = 1 THEN
           PERFORM WITH TEST AFTER UNTIL pl_type = CONST_PLANT_LEAF
                   OR pl_type = CONST_PLANT_FLOWER
               DISPLAY "Type de l'herbier"
               DISPLAY "Feuille/Fleur (attention à la majuscule)"
               ACCEPT pl_type
           END-PERFORM
       END-IF.
       
       delete_plante.
*> Permet de supprimer une plante dans le fichier plante
*> 
*> Variables utilisées :
*> - wSelectedPlanteId
*> - wEndOfFile
*> - wNoPlante
*>
*> Nombre de lectures : le nombre de fois que la plante était contenue
*> dans un herbier, +1
       MOVE 0 TO wEndOfFile
       MOVE 0 TO wNoPlante
       MOVE wSelectedPlanteId TO fhpl_idPlante

       OPEN I-O fhpl
       START fhpl, KEY IS = fhpl_idPlante
       INVALID KEY     MOVE 1 TO wNoPlante
       NOT INVALID KEY
           PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
               READ fhpl NEXT
               AT END      MOVE 1 TO wEndOfFile
               NOT AT END 
                   IF fhpl_idPlante = wSelectedPlanteId THEN
                       DELETE fhpl RECORD
                   ELSE
                       MOVE 1 TO wEndOfFile
                   END-IF
               END-READ
           END-PERFORM
       END-START
       CLOSE fhpl
         
       OPEN I-O fplan
       MOVE 0 TO wNoPlante
       MOVE wSelectedPlanteId TO pl_id
       READ fplan
       INVALID KEY 
           DISPLAY "Cette plante n'existe pas"
           MOVE 1 TO wNoPlante
       NOT INVALID KEY 
           DELETE fplan RECORD
       END-READ
       CLOSE fplan.

       display_plantes_menu.
*> Affiche le menu de consultation des plantes. Ce menu permet d'effec-
*> tuer les opérations de création et de mise à jour si l'utilisateur
*> connecté n'est pas anonyme, et dans tous les cas permet de consulter
*> les statistiques d'une plante
*>
*> Variables utilisées :
*> - wActionChosen
*> - wIsAnonymous
*>
*> Nombre de lectures : aucune
       MOVE 0 TO wActionChosen

       PERFORM display_plantes_type
       PERFORM WITH TEST AFTER UNTIL
                           wActionChosen >= 0 AND 
                           (
                               wActionChosen <= 1 AND
                               wIsAnonymous = 1
                           ) OR
                           (
                               wActionChosen <= 3 AND
                               wIsAnonymous = 0
                           )
           
           DISPLAY " "
           DISPLAY CONST_DISPLAY_MENU
           DISPLAY " "

           IF 
               wActionChosen < 0 OR
               (
                   wActionChosen > 1 AND
                   wIsAnonymous = 1
               ) OR
               (
                   wActionChosen > 3 AND
                   wIsAnonymous = 0
               )
           THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           DISPLAY CONST_ACTION_SENTENCE
           DISPLAY " "
		
           DISPLAY "        1 : consulter les statistiques d'une plante"

           IF NOT wIsAnonymous = 1 THEN
               DISPLAY "        2 : créer une nouvelle plante"
               DISPLAY "        3 : modifier une plante"
           END-IF
           
           DISPLAY " "
           DISPLAY "        0 : retourner au menu"
           DISPLAY " "

           ACCEPT wActionChosen
           DISPLAY " "
       END-PERFORM

       EVALUATE wActionChosen
       WHEN 1
          *> Consulter les stats d'une plante
           PERFORM display_plante_stats_menu
           PERFORM display_plantes_menu
       WHEN 2
          *> Créer une nouvelle plante
           PERFORM add_plante
           PERFORM display_plantes_menu
       WHEN 3
          *> Modifier une plante existante
           PERFORM display_update_plante
           PERFORM display_plantes_menu
       END-EVALUATE.

       display_admin_plante_menu.
*> Affiche le menu correspondant aux herbiers. Cette fonction doit être
*> appelée lorsqu'un administrateur est connecté et veux consulter tous
*> les herbiers.
*>
*> Variables utilisées :
*> - wActionChosen
*> - wIsAnonymous
*>
*> Nombre de lectures : aucune
       MOVE 0 TO wActionChosen

       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen < 6
           DISPLAY " "
           DISPLAY CONST_DISPLAY_MENU
           DISPLAY " "

           IF wActionChosen > 5 OR wActionChosen < 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           
           IF wIsAnonymous = 0 THEN
               DISPLAY CONST_ACTION_SENTENCE
               DISPLAY " "
		
               DISPLAY "        1 : consulter les plantes"
               DISPLAY "        2 : supprimer une plante"
               DISPLAY "        3 : ajouter une plante"
               DISPLAY "        4 : modifier les informations d'une ",
                       "plante"
               DISPLAY "        5 : consulter les statistiques d'une ",
                       "plante"
           
               DISPLAY " "
               DISPLAY "        0 : retourner au menu"
               DISPLAY " "
           ELSE
               EXIT
           END-IF
           ACCEPT wActionChosen

           DISPLAY " "
       END-PERFORM

       EVALUATE wActionChosen
       WHEN 1
          *> consulter les plantes
           PERFORM display_plantes
           PERFORM display_admin_plante_menu
       WHEN 2
          *> supprimer une plante
           PERFORM display_delete_plante
           PERFORM display_admin_plante_menu
       WHEN 3
          *> ajouter une plante
           PERFORM add_plante
           PERFORM display_admin_plante_menu
       WHEN 4
          *> gérer une plante
           PERFORM display_update_plante
           PERFORM display_admin_plante_menu
       WHEN 5
          *> consulter statistique
           PERFORM display_plante_stats_menu
           PERFORM display_admin_plante_menu
       END-EVALUATE.
       
       display_update_plante.
*> Affiche le menu de choix de la plante à modifier.
*>
*> Variables utilisées :
*> - wSelectedPlanteId
*> - wPlantExists
*>
*> Nombre de lectures : aucune
       MOVE 0 TO wSelectedPlanteId
       MOVE 1 TO wPlantExists

       PERFORM WITH TEST AFTER UNTIL wPlantExists = 1
           IF wPlantExists = 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE, " Aucune plante avec ",
                       "cet identifiant n'a pu être trouvée."
           END-IF

           DISPLAY "Entrez l'identifiant de la plante que vous ",
                   "souhaitez modifier"
           ACCEPT wSelectedPlanteId

           PERFORM check_plant_exists
       END-PERFORM

       DISPLAY " "
       PERFORM update_plante.
       
       display_delete_plante.
*> Affiche un menu permettant de sélectionner la plante à supprimer.
*>
*> Variables utilisées :
*> - wSelectedPlanteId
*> - wPlantExists
*>
*> Nombre de lectures : aucune
       PERFORM display_plantes
       PERFORM WITH TEST AFTER UNTIL wPlantExists = 1    
           DISPLAY "Quel identifiant de plante souhaitez vous supprimer"
           ACCEPT wSelectedPlanteId

           PERFORM check_plant_exists
       END-PERFORM

       DISPLAY " "
       PERFORM delete_plante.
       
       add_herbier_plante.
*> Permet d'ajouter une plante dans un herbier avec herbier_plante
*> 
*> Variables utilisées :
*> - wSelectedHerbierId
*> - wSelectedHerbierType
*> - wLastHerbierPlantId
*> - wInsertionPlante
*> - wTypePlante
*> - wSelectedPlanteId
*>
*> Nombre de lectures : aucune
       IF wSelectedHerbierType = CONST_HERBIER_LEAF OR
          wSelectedHerbierType = CONST_HERBIER_FLOWER
       THEN
           MOVE wSelectedHerbierType TO wTypePlante
           PERFORM display_plantes_type
       ELSE
           PERFORM display_plantes
       END-IF
       
       PERFORM WITH TEST AFTER UNTIL wInsertionPlante = 1
           DISPLAY "Veuillez saisir l'identifiant de la plante que", 
           "que vous souhaitez ajouter à l'herbier"
           ACCEPT wSelectedPlanteId
           IF wSelectedHerbierType = CONST_HERBIER_LEAF OR 
              wSelectedHerbierType = CONST_HERBIER_FLOWER
           THEN
               PERFORM check_plante_right_type
           END-IF
           
           IF wSelectedHerbierType = CONST_HERBIER_MIXTE THEN
               MOVE 1 TO wInsertionPlante
           END-IF
       END-PERFORM

       
       DISPLAY "Quelle est la date de cueillette de la plante ? ",
               "(format jj/mm/aaaa)"
       ACCEPT fhpl_date
           
       DISPLAY "Quelle est la taille en centimètres de la plante ",
               "ceuillie ?"
       ACCEPT fhpl_taille
           
       DISPLAY "Quel est le nom du lieu où la plante a été ceuillie ?"
       ACCEPT fhpl_lieu

       ADD 1 TO wLastHerbierPlantId
       MOVE wLastHerbierPlantId TO fhpl_id
       MOVE wSelectedHerbierId TO fhpl_idHerbier
       MOVE wSelectedPlanteId TO fhpl_idPlante

       OPEN I-O fhpl
       WRITE tamp_fhpl
       END-WRITE
       CLOSE fhpl.
       
       check_plante_right_type.
*> Vérifie que le type de la plante dans wSelectedPlanteId correspond au
*> type dans wSelectedHerbierType.
*> Le résultat est stocké dans wInsertionPlante
*>
*> Variables utilisées :
*> - wSelectedPlanteId
*> - wSelectedHerbierType
*> - wInsertionPlante
*>
*> Nombre de lectures : 1
       MOVE 0 TO wInsertionPlante
       MOVE wSelectedPlanteId TO pl_id

       OPEN INPUT fplan
       READ fplan
       KEY IS pl_id
           INVALID KEY     MOVE 0 TO wInsertionPlante
           NOT INVALID KEY 
               IF pl_type = wSelectedHerbierType THEN
                   MOVE 1 TO wInsertionPlante
               END-IF
       END-READ
       CLOSE fplan.
       
       last_herbier_plante_id.
*> Parcourt le fichier herbier_plante à la recherche du plus grand id. 
*> Une fois trouvé, on le stocke dans wLastHerbierPlantId
*>
*> Variables utilisées :
*> - wLastHerbierPlantId
*> - wEndOfFile
*>
*> Nombre de lectures : autant qu'il y a de plantes dans chaque herbier
*> cumulés
       MOVE 0 TO wEndOfFile
       MOVE 0 TO wLastHerbierPlantId
       
       OPEN INPUT fhpl
       PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
           READ fhpl
           AT END MOVE 1 TO wEndOfFile
           NOT AT END
               IF fhpl_id > wLastHerbierPlantId THEN
                   MOVE fhpl_id TO wLastHerbierPlantId
               END-IF
           END-READ
       END-PERFORM
       CLOSE fhpl.

       display_plante_herbier_table_header.
*> Affiche l'en-tête de la table pour afficher les plantes d'un herbier
*>
*> Variables utilisées : aucune
*> Nombre de lectures : aucune
       DISPLAY "ID  | Nom de la plante               | Type de plante ",
               " | Date de ceuillette | Taille | Lieu de ceuillette"
       DISPLAY "----|--------------------------------|----------------",
               "-|--------------------|--------|----------------------",
               "-------------------".
       
       display_plante_herbier_table_line.
*> Affiche l'entité plante herbier actuellement dans le tampon sous
*> forme de table.
*> ⚠️ La jointure avec la plante correspondante y est faite ! Il est
*> donc important que le fichier plantes ne soit pas ouvert avant de
*> l'appeler !
*>
*> Variables utilisées : aucune
*> Nombre de lectures : 1
       MOVE fhpl_idPlante TO pl_id

       OPEN INPUT fplan
       READ fplan
       END-READ
       CLOSE fplan
       
       DISPLAY fhpl_id, " | ", pl_nom, " | ", pl_type, " | ", 
               fhpl_date, "    |  ", fhpl_taille, " | ", fhpl_lieu.
       
       display_herbier_plantes.
*> Affiche sous la forme d'une table les plantes contenues dans un 
*> herbier.
*>
*> Variables utilisées :
*> - wNoHerbierPlante
*>
*> Nombre de lectures : Autant qu'il y a de plantes dans l'herbier
       MOVE wSelectedHerbierId TO fhpl_idHerbier
       
       OPEN INPUT fhpl
       START fhpl, KEY IS = fhpl_idHerbier
       INVALID KEY
           DISPLAY "Aucune plante pour l'herbier cherché"
           MOVE 1 TO wNoHerbierPlante
       NOT INVALID KEY
       	   MOVE 0 TO wNoHerbierPlante
           PERFORM display_plante_herbier_table_header
           PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
               READ fhpl NEXT
               AT END      MOVE 1 TO wEndOfFile
               NOT AT END
                   IF wSelectedHerbierId = fhpl_idHerbier THEN
                       PERFORM display_plante_herbier_table_line
                   ELSE
                       MOVE 1 TO wEndOfFile
                   END-IF
               END-READ
           END-PERFORM
       END-START
       CLOSE fhpl.

       display_plante_stats_menu.
*> Petit menu qui permet de choisir la plante dont l'utilisateur sou-
*> haite consulter les statistiques.
*>
*> Variables utilisées :
*> - wSelectedPlanteId
*> - wPlantExists
*>
*> Nombre de lectures : 1
       MOVE 0 TO wSelectedPlanteId
       MOVE 1 TO wPlantExists

       PERFORM WITH TEST AFTER UNTIL wPlantExists = 1
           IF wPlantExists = 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE, " Aucune plante avec ",
                       "cet identifiant n'a pu être trouvée."
           END-IF

           DISPLAY "Entrez l'identifiant de la plante dont vous ",
                   "souhaitez consulter les statistiques"
           ACCEPT wSelectedPlanteId

           PERFORM check_plant_exists
       END-PERFORM

       DISPLAY " "
       PERFORM display_plante_stats.


       display_plante_stats.
*> Cette fonction permet d'afficher, pour la plante courante (dont l'id
*> est contenu dans wSelectedPlanteId), le nombre d'herbiers dans lesquels
*> elle apparaît ainsi que leur nom, et son nombre total de précences.
*>
*> Variables utilisées :
*> - wSelectedPlanteId
*> - wEndOfFile
*> - wEndOfZone
*> - wPlantCount
*> - wPlantTotalCount
*>
*> Nombre de lectures : autant qu'il y a d'herbiers, plus autant qu'il y
*> a de plantes dans les herbiers (on va faire une recherche séquen-
*> tielle sur les herbiers, et pour chaque, on va faire une recherche
*> sur zone sur les herbiers dans le fichier herbier_plante pour regar-
*> der combien on a de plantes correspondant à celle recherchée).
       MOVE 0 TO wEndOfFile
       MOVE 0 TO wPlantTotalCount

       DISPLAY "ID de l'herbier | Titre                          | ",
               "Nombre de présences"
       DISPLAY "----------------|--------------------------------|-",
               "-------------------"
       

       OPEN INPUT fher
       PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
           READ fher
           AT END      MOVE 1 TO wEndOfFile
           NOT AT END
              *> Recherche sur zone des plantes de l'herbier
               MOVE fh_id TO fhpl_idHerbier
               MOVE 0 TO wEndOfZone
               MOVE 0 TO wPlantCount

               OPEN INPUT fhpl
               START fhpl, KEY IS = fhpl_idHerbier
               INVALID KEY     MOVE 1 TO wEndOfZone
               NOT INVALID KEY
                   PERFORM WITH TEST AFTER UNTIL wEndOfZone = 1
                       READ fhpl NEXT
                       AT END      MOVE 1 TO wEndOfZone
                       NOT AT END
                           IF fhpl_idHerbier = fh_id
                               IF fhpl_idPlante = wSelectedPlanteId THEN
                                   ADD 1 TO wPlantCount
                               END-IF
                           ELSE
                              MOVE 1 TO wEndOfZone
                           END-IF
                       END-READ
                   END-PERFORM
               END-START
               CLOSE fhpl

               IF wPlantCount > 0 THEN
                   DISPLAY "            ", fh_id, " | ", fh_nom, " | ",
                           "                ", wPlantCount
                   ADD wPlantCount TO wPlantTotalCount
               END-IF
           END-READ
       END-PERFORM
       CLOSE fher

       DISPLAY " "
       DISPLAY "Nombre de présences total : ", wPlantTotalCount.

       compute_average_herbier_by_user.
*> Compte le nombre total d'herbiers, d'utilisateurs, et renvoie dans
*> wAvgHerbierByUser le résultat du rapport du premier sur le second.
*>
*> Variables utilisées :
*> - wHerbierCount
*> - wUtilisateursCount
*> - wAvgHerbierByUser
*>
*> Nombre de lectures : Aucune, on charge en mémoire le nombre d'herb-
*> iers et d'utilisateurs au lancement et on en garde la trace au fil
*> des manipulations de l'utilisateur
       DIVIDE wHerbierCount BY wUtilisateursCount
           GIVING wAvgHerbierByUser
           SIZE ERROR
               DISPLAY "Il faut au moins un utilisateur pour pouvoir ",
                       "calculer le nombre moyen d'herbiers par ",
                       "utilisateur !"
       END-DIVIDE.

       month_from_herbier.
*> Fonction permettant de récupérer le mois depuis une date entière.
*>
*> Variables utilisées :
*> - wDateMonth
*>
*> Nombre de lectures : aucune
       MOVE fh_date(5:6) TO wDateMonth.
       
       delete_herbier_plante.
*> Permet de supprimer une plante d'un herbier dans le fichier
*> herbier_plante.
*> 
*> Variables utilisées :
*> - wNoHerbierPlante
*> - wDeletePlante
*> - wSelectedPlanteHerbierId
*>
*> Nombre de lectures : 1
       PERFORM display_herbier_plantes
       
       IF wNoHerbierPlante = 0 THEN 
       	   PERFORM WITH TEST AFTER UNTIL wDeletePlante = 1
               DISPLAY "Veuillez saisir l'identifiant de la plante que", 
                       " vous souhaitez supprimer de l'herbier :"
               ACCEPT wSelectedPlanteHerbierId
               PERFORM check_plant_herbier_in_herbier
           END-PERFORM
        
           MOVE wSelectedPlanteHerbierId TO fhpl_id

           OPEN I-O fhpl
           READ fhpl
           NOT INVALID KEY 
               DELETE fhpl RECORD
           END-READ
           CLOSE fhpl
       END-IF.
       
       check_plant_herbier_in_herbier.
*> Vérifie que la plante dans wSelectedPlanteHerbierId est une plante 
*> dans l'herbier.
*> Le résultat est stocké dans wDeletePlante
*>
*> Variables utilisées :
*> - wSelectedPlanteHerbierId
*> - wSelectedHerbierId
*> - wDeletePlante
*>
*> Nombre de lectures : 1
       MOVE wSelectedPlanteHerbierId TO fhpl_id
       
       OPEN INPUT fhpl
       READ fhpl
       INVALID KEY         MOVE 0 TO wDeletePlante
       NOT INVALID KEY
           IF fhpl_idHerbier = wSelectedHerbierId THEN
               MOVE 1 TO wDeletePlante
           ELSE
               MOVE 0 TO wDeletePlante
           END-IF
       END-READ
       CLOSE fhpl.
       
       display_global_menu.
*> Affiche le menu général de l'application. Cette fonction s'éxécute
*> à l'ouverture de l'application.
*> Pour faciliter la gestion des droits et limiter les responsabilités
*> de chaque fonction, l'affichage du menu principal a été séparé en
*> trois sous-fonctions, dépendant du type de l'utilisateur. Certaines
*> parties de code étant communes, elles ont été regroupées dans
*> d'autres fonctions pour éviter les duplications de code.
*>
*> Variables utilisées :
*> - wConnectedUserRole
*>
*> Nombre de lectures : aucune
       IF NOT wExitProgramme = 1 THEN
           EVALUATE wConnectedUserRole
           WHEN CONST_ROLE_READER
               PERFORM display_anonymous_global_menu
           WHEN CONST_ROLE_EDITOR
               PERFORM display_editor_global_menu
           WHEN CONST_ROLE_ADMIN
               PERFORM display_admin_global_menu
           END-EVALUATE
       END-IF.

       display_anonymous_global_menu.
*> Affiche le menu global pour les utilisateurs de type Anonyme.
*>
*> Variables utilisées :
*> - wActionChosen
*> - wTypePlante
*>
*> Nombre de lectures : aucune
       MOVE 0 TO wActionChosen

       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen <= 5
           DISPLAY " "
           DISPLAY CONST_DISPLAY_MENU
           DISPLAY " "

           IF wActionChosen > 5 OR wActionChosen < 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           DISPLAY CONST_ACTION_SENTENCE
           DISPLAY " "

           PERFORM display_standard_global_menu_options
          *> Options spécifiques aux utilisateurs anonymes
           DISPLAY "        4 : créer un compte"
           DISPLAY "        5 : se connecter"

           PERFORM display_exit_option

           ACCEPT wActionChosen
           DISPLAY " "
       END-PERFORM

       EVALUATE wActionChosen
       WHEN 0
          *> Sortie du logiciel
           EXIT
       WHEN 1
          *> Consultation des herbiers
           PERFORM display_herbier_menu
           PERFORM display_global_menu
       WHEN 2
          *> Consultation des plantes de type feuille
           MOVE CONST_PLANT_LEAF TO wTypePlante
          *> PERFORM display_plantes_type
           PERFORM display_plantes_menu
           PERFORM display_global_menu
       WHEN 3
          *> Consultation des plantes de type fleur
           MOVE CONST_PLANT_LEAF TO wTypePlante
          *> PERFORM display_plantes_type
           PERFORM display_plantes_menu
           PERFORM display_global_menu
       WHEN 4
          *> Création d'un compte
           PERFORM sign_in
           PERFORM display_global_menu
       WHEN 5
          *> Connexion
           PERFORM login
           PERFORM display_global_menu
       END-EVALUATE.

       display_editor_global_menu.
*> Affiche le menu global pour les utilisateurs de type Editeur.
*>
*> Variables utilisées :
*> - wActionChosen
*> - wTypePlante
*>
*> Nombre de lectures : aucune
       MOVE 0 TO wActionChosen

       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen <= 6
           DISPLAY " "
           DISPLAY CONST_DISPLAY_MENU
           DISPLAY " "

           IF wActionChosen > 6 OR wActionChosen < 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           DISPLAY CONST_ACTION_SENTENCE
           DISPLAY " "

           PERFORM display_standard_global_menu_options
           PERFORM display_connected_users_global_menu_options

           PERFORM display_exit_option

           ACCEPT wActionChosen
           DISPLAY " "
       END-PERFORM

       EVALUATE wActionChosen
       WHEN 0
          *> Sortie du logiciel
           EXIT
       WHEN 1
          *> Consultation des herbiers
           PERFORM display_herbier_menu
           PERFORM display_global_menu
       WHEN 2
          *> Consultation des plantes de type feuille
           MOVE CONST_PLANT_LEAF TO wTypePlante
          *> PERFORM display_plantes_type
           PERFORM display_plantes_menu
           PERFORM display_global_menu
       WHEN 3
          *> Consultation des plantes de type fleur
           MOVE CONST_PLANT_LEAF TO wTypePlante
          *> PERFORM display_plantes_type
           PERFORM display_plantes_menu
           PERFORM display_global_menu
       WHEN 4
          *> Gérer ses herbiers
           PERFORM display_herbier_gesture_menu
           PERFORM display_global_menu
       WHEN 5
          *> Gestion de ses informations
           PERFORM display_account_menu
           PERFORM display_global_menu
       WHEN 6
          *> Déconnexion
           PERFORM log_out
           PERFORM display_global_menu
       END-EVALUATE.

       display_admin_global_menu.
*> Affiche le menu global pour les utilisateurs de type Editeur.
*>
*> Variables utilisées :
*> - wActionChosen
*> - wTypePlante
*>
*> Nombre de lectures : aucune
       MOVE 0 TO wActionChosen

       PERFORM WITH TEST AFTER UNTIL wActionChosen >= 0
                                 AND wActionChosen <= 10
           DISPLAY " "
           DISPLAY CONST_DISPLAY_MENU
           DISPLAY " "

           IF wActionChosen > 10 OR wActionChosen < 0 THEN
               DISPLAY CONST_ACTION_IMPOSSIBLE
           END-IF

           DISPLAY CONST_ACTION_SENTENCE
           DISPLAY " "

           PERFORM display_standard_global_menu_options
           PERFORM display_connected_users_global_menu_options
          *> Options spécifiques aux utilisateurs administrateurs
           DISPLAY "        7 : gérer les herbiers"
           DISPLAY "        8 : gérer les plantes"
           DISPLAY "        9 : gérer les utilisateurs"
           DISPLAY " "
           DISPLAY "       10 : utiliser la fonction distanciel"

           PERFORM display_exit_option

           ACCEPT wActionChosen
           DISPLAY " "
       END-PERFORM

       EVALUATE wActionChosen
       WHEN 0
          *> Sortie du logiciel
           EXIT
       WHEN 1
          *> Consultation des herbiers
           PERFORM display_herbier_menu
           PERFORM display_global_menu
       WHEN 2
          *> Consultation des plantes de type feuille
           MOVE CONST_PLANT_LEAF TO wTypePlante
          *> PERFORM display_plantes_type
           PERFORM display_plantes_menu
           PERFORM display_global_menu
       WHEN 3
          *> Consultation des plantes de type fleur
           MOVE CONST_PLANT_LEAF TO wTypePlante
          *> PERFORM display_plantes_type
           PERFORM display_plantes_menu
           PERFORM display_global_menu
       WHEN 4
          *> Gérer ses herbiers
           PERFORM display_herbier_gesture_menu
           PERFORM display_global_menu
       WHEN 5
          *> Gestion de ses informations
           PERFORM display_account_menu
           PERFORM display_global_menu
       WHEN 6
          *> Déconnexion
           PERFORM log_out
           PERFORM display_global_menu
       WHEN 7
          *> Gestion des herbiers (en tant qu'admin)
           PERFORM display_admin_herbier_menu
           PERFORM display_global_menu
       WHEN 8
          *> Gestion des plantes (en tant qu'admin)
           PERFORM display_admin_plante_menu
           PERFORM display_global_menu
       WHEN 9
          *> Gestion des utilisateurs (en tant qu'admin)
           PERFORM display_admin_user_menu
           PERFORM display_global_menu
       WHEN 10
          *> Fonction distanciel
           PERFORM dist_menu
           PERFORM display_global_menu
       END-EVALUATE.

       display_standard_global_menu_options.
*> Affiche les options standard du menu global. Ces options sont dispo-
*> nibles pour tous à tout instant.
*>
*> Variables utilisées : aucune
*> Nombre de lectures : aucune
       DISPLAY "        1 : consulter les herbiers"
       DISPLAY "        2 : consulter les feuilles"
       DISPLAY "        3 : consulter les fleurs".

       display_connected_users_global_menu_options.
*> Affiche les options pour les utilisateurs connectés du menu global.
*>
*> Variables utilisées : aucune
*> Nombre de lectures : aucune
       DISPLAY "        4 : gérer ses herbiers"
       DISPLAY "        5 : gérer ses informations"
       DISPLAY "        6 : se déconnecter".

       display_exit_option.
*> Affiche la dernière option d'un menu (0) pour retour en arrière.
*>
*> Variables utilisées : aucune
*> Nombre de lectures : aucune
       DISPLAY " "
       DISPLAY "        0 : quitter l'application"
       DISPLAY " ".

*> FONCTION DISTANCIEL :
       dist_menu.
*> Affiche un menu permettant de rentrer les données nécessaires à l'ap-
*> pel de la fonction distanciel.
*> 
*> Variables utilisées :
*> - wDistUserType
*> - wDistMonth
*> - wDistPlace
*>
*> Nombre de lectures : aucune
       MOVE " " TO wDistUserType
       MOVE " " TO wDistMonth
       MOVE " " TO wDistPlace

       DISPLAY "Le but de cette fonction est d'afficher le nom des ",
               "herbiers créés à un mois donné et appartenant à un ",
               "type d'utilisateurs donné qui contiennent au moins une",
               " plante ceuillie à un lieu donné"
       
       DISPLAY " "
       PERFORM WITH TEST AFTER UNTIL wDistMonth = "01"
                                  OR wDistMonth = "02"
                                  OR wDistMonth = "03"
                                  OR wDistMonth = "04"
                                  OR wDistMonth = "05"
                                  OR wDistMonth = "06"
                                  OR wDistMonth = "07"
                                  OR wDistMonth = "08"
                                  OR wDistMonth = "09"
                                  OR wDistMonth = "10"
                                  OR wDistMonth = "11"
                                  OR wDistMonth = "12"
           DISPLAY "Entrez le mois de création des herbiers (format mm)"
           ACCEPT wDistMonth
       END-PERFORM
       
       DISPLAY " "
       PERFORM WITH TEST AFTER UNTIL wDistUserType = CONST_USER_AMATEUR
                                  OR wDistUserType = CONST_USER_PRO
           DISPLAY "Entrez le type d'utilisateurs auquel les herbiers ",
                   "dervaient appartenir (", CONST_USER_AMATEUR,
                   CONST_USER_PRO, ")"
           ACCEPT wDistUserType
       END-PERFORM

       DISPLAY " "
       DISPLAY "Entrez le lieu de ceuillete"
       ACCEPT wDistPlace

       PERFORM herbier_name_from_month_and_user_type_with_plant_origin.

       herbier_name_from_month_and_user_type_with_plant_origin.
*> Pour un type d'utilisateur stocké dans wDistUserType, affiche le nom
*> des herbiers créés à un mois contenu dans wDistMonth avec au moins
*> une plante cueillie dans le lieu contenu dans wDistPlace.
*>
*> Variables utilisées :
*> - wDistUserType
*> - wDistMonth
*> - wDistPlace
*> - wEndOfZone
*> - wDateMonth
*>
*> Nombre de lectures : voir rendu distanciel
       MOVE 0 TO wEndOfZone
       MOVE wDistPlace TO fhpl_lieu

      *> Lecture sur zone de herbier_plante (tri par lieu)
       OPEN INPUT fhpl
       START fhpl, KEY IS = fhpl_lieu
       INVALID KEY     DISPLAY "Aucun herbier correspondant"
       NOT INVALID KEY
           PERFORM WITH TEST AFTER UNTIL wEndOfZone = 1
               READ fhpl NEXT
               AT END      MOVE 1 TO wEndOfZone
               NOT AT END
                   IF fhpl_lieu = wDistPlace THEN
                      *> Lecture directe de l'herbier lié
                       MOVE fhpl_idHerbier TO fh_id
                       OPEN INPUT fher
                       READ fher
                       NOT INVALID KEY
                           PERFORM month_from_herbier
                           IF wDateMonth = wDistMonth THEN
                              *> Lecture directe de l'utilisateur lié
                               MOVE fh_utilisateur TO fu_id
                               OPEN INPUT futil
                               READ futil
                               NOT INVALID KEY
                                   IF fu_type = wDistUserType THEN
                                       DISPLAY fh_nom
                                   END-IF
                               END-READ
                               CLOSE futil
                           END-IF
                       END-READ
                       CLOSE fher
                   ELSE
                       MOVE 1 TO wEndOfZone
                   END-IF
               END-READ
           END-PERFORM
       END-START
       CLOSE fhpl.
