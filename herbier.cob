IDENTIFICATION DIVISION.
PROGRAM-ID. Herbier.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL. 
        select fplan assign to "plante.dat"
        organization indexed
        access mode is dynamic
        record key is pl_id
        alternate record key is pl_nom
        alternate record key is pl_nomLatin
        alternate record key is pl_saison WITH DUPLICATES
        file status is cr_fplan.
        
        select fher assign to "herbier.dat"
        organization indexed
        access mode is dynamic
        record key is fh_id
        alternate record key is fh_utilisateur WITH DUPLICATES
        alternate record key is fh_type WITH DUPLICATES
        file status is cr_fher.
        
        select futil assign to "utilsateur.dat"
        organization indexed
        access mode is dynamic
        record key is fu_id
        alternate record key is fu_login
        alternate record key is fu_role WITH DUPLICATES
        alternate record key is fu_type WITH DUPLICATES
        file status is cr_futil.
        
        select fhpl assign to "herbier_plante.dat"
        organization indexed
        access mode is dynamic
        record key is fhpl_id
        alternate record key is fhpl_lieu WITH DUPLICATES
        alternate record key is fhpl_idHerbier WITH DUPLICATES
        alternate record key is fhpl_idPlante WITH DUPLICATES
        file status is cr_fhpl.
        
        
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
                02 fhpl_id PIC 9(2).
                02 fhpl_idHerbier PIC 9(3).
                02 fhpl_idPlante PIC 9(3).
                02 fhpl_date PIC A(15).
                02 fhpl_taille PIC 9(5).
                02 fhpl_lieu PIC A(40).
               

               
WORKING-STORAGE SECTION.
      *> Constantes
       77 CONST_ROLE_READER PIC A(15).
       77 CONST_ROLE_EDITOR PIC A(15).
       77 CONST_ROLE_ADMIN PIC A(15).
       77 CONST_PLANT_LEAF PIC A(15).
       77 CONST_PLANT_FLOWER PIC A(15).
       77 CONST_HERBIER_LEAF PIC A(15).
       77 CONST_HERBIER_FLOWER PIC A(15).
       77 CONST_HERBIER_MIXTE PIC A(15).

      *> CRs
       77 cr_fplan PIC 9(2).
       77 cr_fher PIC 9(2).
       77 cr_futil PIC 9(2).
       77 cr_fhpl PIC 9(2).

      *> Variables globales
       77 l_h_id PIC 9(3).
       77 wEndOfFile PIC 9(1).
       77 wUtilisateursCount PIC 9(3).
       77 wConnectedUser PIC 9(3).
       77 wNoHerbier PIC 9(1).
       77 wSelectedHerbierId PIC 9(3). 
       77 wActionChosen PIC 9(1). 
        
        
PROCEDURE DIVISION.

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
MOVE "Éditeur" TO CONST_ROLE_EDITOR.
MOVE "Administrateur" TO CONST_ROLE_ADMIN.
MOVE "Feuille" TO CONST_PLANT_LEAF.
MOVE "Fleur" TO CONST_PLANT_FLOWER.
MOVE "Feuille" TO CONST_HERBIER_LEAF.
MOVE "Fleur" TO CONST_HERBIER_FLOWER.
MOVE "Mixte" TO CONST_HERBIER_MIXTE.

*> Insertion automatique d'un untilisateur s'il n'y en a pas déjà
PERFORM add_default_user_if_first_start.
PERFORM last_herbier_id

MOVE fu_id TO wConnectedUser.
PERFORM display_all_herbier.
DISPLAY " ".
PERFORM display_user_herbier.
DISPLAY " ".
PERFORM display_herbier_by_id.
DISPLAY " ".
MOVE 2 TO wConnectedUser.
PERFORM display_user_herbier.
DISPLAY " ".
PERFORM display_herbier_by_id.
DISPLAY " ".

STOP RUN.

*> herbier


       last_herbier_id.
*> Compte le nombre d'herbier présents dans la fichier herbier
*> et stocke le résultat dans l_h_id
*>
*> Variables utilisées :
*> - l_h_id
*> - wEndOfFile
       OPEN INPUT fher
       MOVE 0 TO wEndOfFile
       MOVE 0 TO l_h_id
       PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
           READ fher
           AT END MOVE 1 TO wEndOfFile
           NOT AT END
               ADD 1 TO l_h_id
           END-READ
       END-PERFORM
       CLOSE fher.
       
       
        add_herbier.
*> Permet d'ajouter un herbier dans le fichier herbier
*> 
*> Variables utilisées :
*> - l_h_id
         ADD 1 TO l_h_id
         MOVE l_h_id TO fh_id 
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
        OPEN I-O fher
        WRITE tamp_fher
        END-WRITE
        CLOSE fher.
        
        delete_herbier.
*> Permet de supprimer un herbier dans le fichier herbier
*> 
*> Variables utilisées :
*> - l_h_id
                 
        OPEN I-O fhpl
        MOVE 0 TO wEndOfFile
        Move 0 TO wNoHerbier
        MOVE wSelectedHerbierId TO fhpl_id
        START fhpl, KEY IS = fhpl_id
        INVALID KEY 
         MOVE 1 TO wNoHerbier
        NOT INVALID KEY
           PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
              READ fhpl NEXT
              AT END MOVE 1 TO wEndOfFile
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
         ADD -1 TO l_h_id
         DELETE fher RECORD
        END-READ
        CLOSE fher.
        
        update_herbier.
*> Permet de supprimer un herbier dans le fichier herbier
*> 
*> Variables utilisées :
*> - l_h_id

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
        END-IF
        
         OPEN I-O fher
         READ fher
         NOT INVALID KEY
            REWRITE tamp_fher
         END-READ
         CLOSE fher.
        
        display_all_herbier.
*> Permet d'afficher l'id, le nom et le type de tout les herbiers
*>
*> Variables utilisées :
*> - wEndOfFile
        OPEN INPUT fher
        MOVE 0 TO wEndOfFile
        DISPLAY "ID  | Nom de l'herbier               | Type de l'herbi" 
        "er    | ID de l'utilsateur  "
        DISPLAY "----|--------------------------------|----------------"
        "------|--------------------"
        PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
                READ fher
                AT END MOVE 1 TO wEndOfFile
                NOT AT END 
                 DISPLAY fh_id, " | ", fh_nom, " | ",
                               fh_type, " | ",
                               fh_utilisateur
                END-READ
        END-PERFORM
        CLOSE fher.
        
        display_user_herbier.
*> Permet d'afficher l'id, le nom et le type des herbiers de
*> l'utilisateur courant
*>
*> Variables utilisées :
*> - wEndOfFile
*> - wConnectedUser        
        OPEN INPUT fher
        MOVE 0 TO wEndOfFile
        Move 0 TO wNoHerbier
        MOVE wConnectedUser TO fh_utilisateur
        START fher, KEY IS = fh_utilisateur
        INVALID KEY 
         DISPLAY "Vous n'avez pas créer d'herbier"
         MOVE 1 TO wNoHerbier
        NOT INVALID KEY
        DISPLAY "ID  | Nom de l'herbier               | Type de l'herbi" 
        "er    "
        DISPLAY "----|--------------------------------|----------------"
        "------"
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
        
        display_herbier_by_id.
*> Permet d'afficher toutes les infos d'un herbier en fonction de son
*> id
*>
*> Variables utilisées :
*> - wEndOfFile
*> - wSelectedHerbierId 
*> - wConnectedUser           
        PERFORM display_all_herbier
        DISPLAY "Quel herbier voulez vous sélectionner ?"
        ACCEPT wSelectedHerbierId
        
        OPEN INPUT fher
        MOVE 0 TO wNoHerbier
        MOVE wSelectedHerbierId TO fh_id
        READ fher
        INVALID KEY 
         DISPLAY "Cet herbier n'existe pas"
         MOVE 1 TO wNoHerbier
        NOT INVALID KEY 
         DISPLAY "ID  | Nom de l'herbier               | Type de l'herb" 
         "ier    | Date de création | ID de l'utilsateur  "
         DISPLAY "----|--------------------------------|---------------"
         "-------|------------------|---------------------"
         DISPLAY fh_id, " | ", fh_nom, " | ", fh_type, " | ",
         fh_date , " | ", fh_utilisateur
         IF fh_utilisateur = wConnectedUser THEN
          DISPLAY "Cet herbier peut être modifier"
         ELSE 
          DISPLAY "Cet herbier ne peut pas être modifier"
         END-IF
        END-READ
        CLOSE fher.
  

       count_utilisateurs.
*> Compte le nombre d'utilisateurs présents dans la fichier utilisateurs
*> et stocke le résultat dans wUtilisateursCount
*>
*> Variables utilisées :
*> - wUtilisateursCount
*> - wEndOfFile
*>
*> Nombre de lectures :
*> - Autant qu'il y a d'utilisateurs dans le fichiers utilisateurs
       OPEN INPUT futil
       MOVE 0 TO wEndOfFile
       MOVE 0 TO wUtilisateursCount
       PERFORM WITH TEST AFTER UNTIL wEndOfFile = 1
           READ futil
           AT END          MOVE 1 TO wEndOfFile
           NOT AT END
               ADD 1 TO wUtilisateursCount
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
       END-IF.

