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
                
FD fher.
        01 tamp_fher.
                02 fh_id PIC 9(3).
                02 fh_nom PIC A(30).
                02 fh_utilisateur PIC A(30).
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
        77 cr_fplan PIC 9(2).
        77 cr_fher PIC 9(2).
        77 cr_futil PIC 9(2).
        77 cr_fhpl PIC 9(2).

        77 wEndOfFile PIC 9(1).

        77 wUtilisateursCount PIC 9(3).
        
        
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

*> Insertion automatique d'un untilisateur s'il n'y en a pas déjà
PERFORM add_default_user_if_first_start.

STOP RUN.


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

