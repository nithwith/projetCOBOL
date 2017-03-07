*> LOURDELET MARTIN / LEVRARD MARTIN / MARTY THEO / GOUX ALEXANDRE
IDENTIFICATION DIVISION.
PROGRAM-ID. Projet.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.

       SELECT reclamations ASSIGN TO "reclamations.dat"
       ORGANIZATION INDEXED
       ACCESS MODE IS DYNAMIC
       RECORD KEY frecl_id
       ALTERNATE RECORD KEY frecl_etat WITH DUPLICATES
       FILE STATUS IS frecl_stat.

       SELECT clients ASSIGN TO "clients.dat"
       ORGANIZATION sequential
       ACCESS IS sequential
       FILE STATUS IS fcl_stat.

       SELECT commandes ASSIGN TO "commandes.dat"
       ORGANIZATION INDEXED
       ACCESS MODE IS DYNAMIC
       RECORD KEY fco_id
       ALTERNATE RECORD KEY fco_date WITH DUPLICATES
       FILE STATUS IS fco_stat.

       SELECT articles ASSIGN TO "articles.dat"
       ORGANIZATION INDEXED
       ACCESS MODE IS DYNAMIC
       RECORD KEY fart_id
       ALTERNATE RECORD KEY fart_type WITH DUPLICATES
       FILE STATUS IS fart_stat.

       SELECT statistiquesArticles ASSIGN TO "statistiquesArticles.dat"
       ORGANIZATION INDEXED
       ACCESS MODE IS DYNAMIC
       RECORD KEY fstata_idart
       ALTERNATE RECORD KEY fstata_classe WITH DUPLICATES
       FILE STATUS IS fstata_stat.

       SELECT statistiquesClients ASSIGN TO "statistiquesClients.dat"
       ORGANIZATION sequential
       ACCESS IS sequential
       FILE STATUS IS fstatc_stat.

DATA DIVISION.
FILE SECTION.

        FD reclamations.
        01 freclTampon.
          02 frecl_id PIC X(6).
          02 frecl_idco PIC X(6).
          02 frecl_motif PIC A(20).
          02 frecl_description PIC A(40).
          02 frecl_etat PIC A(10).
          02 frecl_note PIC X(2).
          02 frecl_date PIC A(15).

        FD clients.
        01 fclTampon.
          02 fcl_id PIC X(6).
          02 fcl_nom PIC A(20).
          02 fcl_prenom PIC A(20).
          02 fcl_tel PIC A(10).
          02 fcl_mail PIC A(20).

        FD commandes.
        01 fcoTampon.
          02 fco_id PIC X(6).
          02 fco_idcl PIC X(6).
          02 fco_idart PIC X(6).
          02 fco_date PIC A(15).
          02 fco_promo PIC X(4).

        FD articles.
        01 fartTampon.
          02 fart_id PIC X(6).
          02 fart_nom PIC A(20).
          02 fart_type PIC A(10).
          02 fart_description PIC A(40).
          02 fart_prix PIC X(4).
          02 fart_dureegaranti PIC A(20).

        FD statistiquesArticles.
        01 fstataTampon.
          02 fstata_idart PIC X(6).
          02 fstata_date PIC A(15).
          02 fstata_nbrecl PIC X(6).
          02 fstata_nbCommande PIC X(6).
          02 fstata_classe PIC A(1).

        FD statistiquesClients.
        01 fstatcTampon.
          02 fstatc_idart PIC X(6).
          02 fstatc_date PIC A(15).
          02 fstatc_nbrecl PIC X(6).
          02 fstatc_nbCommande PIC X(6).
          02 fstatc_classe PIC A(1).

WORKING-STORAGE SECTION.
        77 frecl_stat PIC 9(2).
        77 fcl_stat PIC 9(2).
        77 fco_stat PIC 9(2).
        77 fart_stat PIC 9(2).
        77 fstata_stat PIC 9(2).
        77 fstatc_stat PIC 9(2).
        77 frecl_idtemp PIC X(6).
        77 Wrep PIC 9.
        77 Wmenu PIC 9(1).
        77 Wfin PIC 9.
        77 Wnom PIC A(20).
        77 Wtrouve PIC 9.
        77 Wind PIC 9(6).
        77 W1 PIC 9(2).

PROCEDURE DIVISION.
        PERFORM WITH TEST AFTER UNTIL Wmenu = 8
        DISPLAY '-------- Gestion S.A.V. --------' WITH BLANK SCREEN
            DISPLAY '1 --> Ajouter nouveau Client' LINE 2 COL 1
            DISPLAY '2 --> Ajouter nouvel article' LINE 3 COL 1
            DISPLAY '3 --> Ajouter nouvelle commande' LINE 4 COL 1
            DISPLAY '4 --> Ajouter nouvelle réclamation' LINE 5 COL 1
            DISPLAY '5 --> Afficher Client' LINE 6 COL 1
            DISPLAY '6 --> Afficher Commande' LINE 7 COL 1
            DISPLAY '7 --> Afficher Réclamation' LINE 8 COL 1
            DISPLAY '8 --> Quitter' LINE 9 COL 1
            ACCEPT Wmenu LINE 10 COL 1
            IF Wmenu = 1 THEN
                PERFORM AJOUT_CLIENT
            END-IF
            IF Wmenu = 2 THEN
                PERFORM AJOUT_ARTICLE
            END-IF
            IF Wmenu = 3 THEN
                PERFORM AJOUT_COMMANDE
            END-IF
            IF Wmenu = 4 THEN
                PERFORM EFFECTUER_RECLAMATION
            END-IF
            IF Wmenu = 5 THEN
                DISPLAY 'Wait'
            END-IF
            IF Wmenu = 6 THEN
                DISPLAY 'Wait'
            END-IF
            IF Wmenu = 7 THEN
                PERFORM AFFICHAGE_RECLAMATION
            END-IF
        END-PERFORM
        STOP RUN.

*> numero de réclamation commence a 1.
        NOMBRE_RECLAMATIONS.
        MOVE 1 TO W1
        MOVE 0 TO Wind
        MOVE 0 TO Wfin
        OPEN INPUT reclamations
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ reclamations NEXT
           AT END MOVE 1 TO Wfin
*>              DISPLAY 'Aucune réclamation'
           NOT AT END
                COMPUTE Wind=Wind+W1
           END-READ
        END-PERFORM
        COMPUTE Wind=Wind+W1
        MOVE Wind TO frecl_idtemp
        CLOSE reclamations.

        EFFECTUER_RECLAMATION.
          PERFORM WITH TEST AFTER UNTIL Wrep = 0
                PERFORM NOMBRE_RECLAMATIONS
                OPEN I-O reclamations
                MOVE frecl_idtemp TO frecl_id
                DISPLAY "------ RECLAMATION ------" WITH BLANK SCREEN
                DISPLAY "Veuillez remplir ce formulaire:" LINE 2 COL 1
                DISPLAY 'Numéro de commande?' LINE 3 COL 1
                ACCEPT frecl_idco LINE 4 COL 1
                DISPLAY 'Motif de votre réclamation?' LINE 5 COL 1
                ACCEPT frecl_motif LINE 6 COL 1
                DISPLAY 'Détaillez votre problème:' LINE 7 COL 1
                ACCEPT frecl_description LINE 8 COL 1
                DISPLAY "Date d'aujourd'hui ?" LINE 9 COL 1
                ACCEPT frecl_date LINE 10 COL 1
                MOVE 'ouvert' TO frecl_etat
                DISPLAY 'Infos enregistrées avec succès' LINE 11 COL 1
                DISPLAY 'Nous traitons votre requête' LINE 12 COL 35
                WRITE freclTampon END-WRITE
                PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                  DISPLAY 'Nouvelle reclamation ? 1 ou 0' LINE 13 COL 1
                  ACCEPT Wrep LINE 14 COL 1
                END-PERFORM
          END-PERFORM
          CLOSE reclamations.

        AFFICHAGE_RECLAMATION.
        OPEN INPUT reclamations
        MOVE 0 TO Wfin
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ reclamations NEXT
           AT END MOVE 1 TO Wfin
           NOT AT END
                DISPLAY 'ID Réclamation :', frecl_id WITH BLANK SCREEN
                DISPLAY 'ID Commande :', frecl_idco LINE 2 COL 1
                DISPLAY 'Motif :', frecl_motif LINE 3 COL 1
                DISPLAY 'Description :', frecl_description LINE 4 COL 1
                DISPLAY 'Etat :', frecl_etat LINE 5 COL 1
                DISPLAY 'note :', frecl_note LINE 6 COL 1
           END-READ
        END-PERFORM
        CLOSE reclamations.

        AJOUT_CLIENT.
        OPEN EXTEND clients
        IF fcl_stat = 35 THEN
                OPEN OUTPUT clients
                CLOSE clients
                OPEN EXTEND clients
        END-IF
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
        DISPLAY '------- AJOUT CLIENT -------' WITH BLANK SCREEN
          DISPLAY 'Numero id client ?' LINE 2 COL 1
          ACCEPT fcl_id LINE 3 COL 1
          DISPLAY 'Nom Client ?' LINE 4 COL 1
          ACCEPT fcl_nom LINE 5 COL 1
          DISPLAY 'Prenom Client ?' LINE 6 COL 1
          ACCEPT fcl_prenom LINE 7 COL 1
          DISPLAY 'Telephone Client ?' LINE 8 COL 1
          ACCEPT fcl_tel LINE 9 COL 1
          DISPLAY 'Mail client ?' LINE 10 COL 1
          ACCEPT fcl_mail LINE 11 COL 1
          WRITE fclTampon END-WRITE
          PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Ajouter un autre client ? 1 ou 0' LINE 12 COL 1
             ACCEPT Wrep LINE 13 COL 1
          END-PERFORM
        END-PERFORM
        CLOSE clients.

        AJOUT_COMMANDE.
          OPEN I-O commandes
          PERFORM WITH TEST AFTER UNTIL Wrep = 0
          DISPLAY '------- AJOUT COMMANDE -------' WITH BLANK SCREEN
          DISPLAY 'Identifiant Commande :' LINE 2 COL 1
          ACCEPT fco_id LINE 3 COL 1
          DISPLAY 'Identifiant Client :' LINE 4 COL 1
          ACCEPT fco_idcl LINE 5 COL 1
          DISPLAY 'Identifiant Article :' LINE 6 COL 1
          ACCEPT fco_idart LINE 7 COL 1
          DISPLAY 'Date de commande :' LINE 8 COL 1
          ACCEPT fco_date LINE 9 COL 1
          DISPLAY 'Code Promo :' LINE 10 COL 1
          ACCEPT fco_promo LINE 11 COL 1
          WRITE fcoTampon END-WRITE
          PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
            DISPLAY 'Ajouter une autre commande ? 1 ou 0' LINE 12 COL 1
            ACCEPT Wrep LINE 13 COL 1
          END-PERFORM
          END-PERFORM
          CLOSE commandes.

        AJOUT_ARTICLE.
          OPEN I-O articles
          PERFORM WITH TEST AFTER UNTIL Wrep = 0
          DISPLAY "------- AJOUT ARTICLE -------" WITH BLANK SCREEN
          DISPLAY "id artcile : " LINE 2 COL 1
          ACCEPT fart_id LINE 3 COL 1
          DISPLAY "nom article : " LINE 4 COL 1
          ACCEPT fart_nom LINE 5 COL 1
          DISPLAY "type article : " LINE 6 COL 1
          ACCEPT fart_type LINE 7 COL 1
          DISPLAY "description article : " LINE 8 COL 1
          ACCEPT fart_description LINE 9 COL 1
          DISPLAY "prix article : " LINE 10 COL 1
          ACCEPT fart_prix LINE 11 COL 1
          DISPLAY "duree garantie : " LINE 12 COL 1
          ACCEPT fart_dureegaranti LINE 13 COL 1
          WRITE fartTampon END-WRITE
          PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Ajouter un autre bureau ? 1 ou 0' LINE 14 COL 1
             ACCEPT Wrep LINE 15 COL 1
            END-PERFORM
          END-PERFORM
          CLOSE articles.
