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
       77 wExitProgramme PIC 9(1).
       77 wLoginTrialsCount PIC 9(1).
       77 wPassword PIC A(20).
        
        
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
MOVE "Éditeur" TO CONST_ROLE_EDITOR.
MOVE "Administrateur" TO CONST_ROLE_ADMIN.
MOVE "Feuille" TO CONST_PLANT_LEAF.
MOVE "Fleur" TO CONST_PLANT_FLOWER.
MOVE "Feuille" TO CONST_HERBIER_LEAF.
MOVE "Fleur" TO CONST_HERBIER_FLOWER.
MOVE "Mixte" TO CONST_HERBIER_MIXTE.

*> Insertion automatique d'un untilisateur s'il n'y en a pas déjà
PERFORM add_default_user_if_first_start.

*> >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*> La boucle du main (c'est ici qu'il faudra rajouter les instructions à
*> effectuer pendant la vie du programme, comme l'affichage du menu
*> principal, ...)
PERFORM WITH TEST AFTER UNTIL wExitProgramme = 1
      *> TODO
END-PERFORM.

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
       PERFORM last_herbier_id
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
       END-PERFORM.
  

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
           END-READ
           CLOSE futil

           IF NOT wConnectedUser = 0 AND NOT wPassword = fu_mdp THEN
              *> Mot de passe incorrect, on repasse à 0
               MOVE 0 TO wConnectedUser
           END-IF
       END-PERFORM

       IF wLoginTrialsCount = 3 AND wConnectedUser = 0 THEN
           MOVE 1 TO wExitProgramme
       END-IF
       IF NOT wConnectedUser = 0 THEN
           DISPLAY "Vous êtes connecté en tant que :"
          *> PERFORM display_user_info
       END-IF.
