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
       ORGANIZATION INDEXED
       ACCESS IS DYNAMIC
       RECORD KEY fcl_id
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

       SELECT compteurs ASSIGN TO "compteurs.dat"
       ORGANIZATION INDEXED
       ACCESS MODE IS DYNAMIC
       RECORD KEY fcpt_id
       FILE STATUS IS fcpt_stat.

DATA DIVISION.
FILE SECTION.

        FD compteurs.
        01 fcptTampon.
            02 fcpt_id PIC X(1).
            02 fcpt_val PIC 9(3).

        FD reclamations.
        01 freclTampon.
          02 frecl_id PIC 9(3).
          02 frecl_idco PIC X(6).
          02 frecl_motif PIC A(20).
          02 frecl_description PIC A(40).
          02 frecl_etat PIC A(10).
          02 frecl_note PIC X(2).
          02 frecl_date PIC A(15).

        FD clients.
        01 fclTampon.
          02 fcl_id PIC 9(3).
          02 fcl_nom PIC A(20).
          02 fcl_prenom PIC A(20).
          02 fcl_tel PIC A(10).
          02 fcl_mail PIC A(20).

        FD commandes.
        01 fcoTampon.
          02 fco_id PIC 9(3).
          02 fco_idcl PIC 9(3).
          02 fco_idart PIC 9(3).
          02 fco_date PIC A(15).
          02 fco_promo PIC X(4).

        FD articles.
        01 fartTampon.
          02 fart_id PIC 9(3).
          02 fart_nom PIC A(20).
          02 fart_type PIC A(10).
          02 fart_description PIC A(40).
          02 fart_prix PIC X(4).
          02 fart_dureegaranti PIC A(20).

        FD statistiquesArticles.
        01 fstataTampon.
          02 fstata_idart PIC 9(3).
          02 fstata_date PIC A(15).
          02 fstata_nbrecl PIC X(6).
          02 fstata_nbCommande PIC X(6).
          02 fstata_classe PIC A(1).

WORKING-STORAGE SECTION.
        77 frecl_stat PIC 9(2).
        77 fcl_stat PIC 9(2).
        77 fco_stat PIC 9(2).
        77 fart_stat PIC 9(2).
        77 fstata_stat PIC 9(2).
        77 fstatc_stat PIC 9(2).
        77 fcpt_stat PIC 9(2).
        77 frecl_idtemp PIC X(6).
        77 Wrep PIC 9.
        77 Wfin PIC 9.
        77 Wmenu PIC 9(1).
        77 Wnom PIC A(20).
        77 Wind PIC 9(6).
        77 W1 PIC 9(2).
        77 tmp_id_article PIC X(6).

PROCEDURE DIVISION.

        PERFORM WITH TEST AFTER UNTIL Wmenu = 0
        DISPLAY '-------- Gestion S.A.V. --------' WITH BLANK SCREEN
            DISPLAY '1 --> Ajout Client' LINE 2 COL 1
            DISPLAY '2 --> Ajout Article' LINE 3 COL 1
            DISPLAY '3 --> Ajout Commande' LINE 4 COL 1
            DISPLAY '4 --> Ajout Réclamation' LINE 5 COL 1
            DISPLAY '5 --> Afficher Client' LINE 6 COL 1
            DISPLAY '6 --> Afficher Commande' LINE 7 COL 1
            DISPLAY '7 --> Afficher/Modifier Réclamation' LINE 8 COL 1
            DISPLAY '8 --> Afficher Article' LINE 9 COL 1
            DISPLAY '9 --> Statistiques sur un Article' LINE 10 COL 1
            DISPLAY '0 --> Quitter' LINE 11 COL 1
            ACCEPT Wmenu LINE 12 COL 1
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
                PERFORM AFFICHER_CLIENT
            END-IF
            IF Wmenu = 6 THEN
                PERFORM AFFICHER_COMMANDE
            END-IF
            IF Wmenu = 7 THEN
                PERFORM GERER_RECLAMATION
            END-IF
            IF Wmenu = 8 THEN
                PERFORM AFFICHAGE_ARTICLE
            END-IF
            IF Wmenu = 9 THEN
                DISPLAY 'Coucou'
            END-IF
        END-PERFORM
        STOP RUN.




*> Procédures relatives au fichier réclamation

        EFFECTUER_RECLAMATION.
            OPEN I-O reclamations
            IF frecl_stat = 35 THEN
                    OPEN OUTPUT reclamations
                    CLOSE reclamations
                    OPEN I-O compteurs
                    IF fcpt_stat = 35 THEN
                        OPEN OUTPUT compteurs
                        CLOSE compteurs
                        OPEN I-O compteurs
                    END-IF
                    MOVE 1 TO fcpt_id
                    MOVE 1 TO fcpt_val
                    WRITE fcptTampon END-WRITE
                    CLOSE compteurs
                    OPEN I-O reclamations
            END-IF

            PERFORM WITH TEST AFTER UNTIL Wrep = 0
            OPEN I-O compteurs
            MOVE 1 TO fcpt_id
            READ compteurs
            MOVE fcpt_val TO frecl_id
            COMPUTE fcpt_val = fcpt_val + 1
            REWRITE fcptTampon
            CLOSE compteurs
            OPEN I-O commandes
            DISPLAY '------ RECLAMATION ------' WITH BLANK SCREEN
            DISPLAY 'Numero de commande :' LINE 2 COL 1
            DISPLAY 'Motif de votre reclamation :' LINE 3 COL 1
            DISPLAY 'Detaillez votre probleme :' LINE 4 COL 1
            DISPLAY "Date d'aujourd'hui (JJ/MM/AAAA) :" LINE 5 COL 1

            ACCEPT frecl_idco LINE 2 COL 22
            MOVE frecl_idco TO fco_id
            READ commandes
            INVALID KEY

                DISPLAY 'Commande inexistante' WITH BLANK SCREEN
                PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                   DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
                   ACCEPT Wrep LINE 2 COL 21
                END-PERFORM

            NOT INVALID KEY

            ACCEPT frecl_motif LINE 3 COL 30
            ACCEPT frecl_description LINE 4 COL 28
            ACCEPT frecl_date LINE 5 COL 35
            MOVE 'ouvert' TO frecl_etat
            WRITE freclTampon END-WRITE
            DISPLAY 'Réclamation réalisée avec succès.' LINE 7 COL 1
            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
              DISPLAY 'Nouvelle reclamation ? 1 ou 0 :' LINE 9 COL 1
              ACCEPT Wrep LINE 9 COL 33
            END-PERFORM

            END-READ
          END-PERFORM
          CLOSE commandes
          CLOSE reclamations.

          GERER_RECLAMATION.
          OPEN I-O reclamations
          IF frecl_stat = 35 THEN

            DISPLAY 'Fichier réclamation inexistant' WITH BLANK SCREEN
            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
               DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
               ACCEPT Wrep LINE 2 COL 22
            END-PERFORM

         ELSE

          PERFORM WITH TEST AFTER UNTIL Wrep = 0
          DISPLAY '----- Recherche Reclamation -----' WITH BLANK SCREEN
            DISPLAY "Identifiant de la reclamation :" LINE 2 COL 1
            ACCEPT frecl_id LINE 2 COL 33
            READ reclamations
            INVALID KEY
                DISPLAY 'Réclamation inexistante' LINE 3 COL 1
            NOT INVALID KEY
               DISPLAY 'Id reclamation :' LINE 3 COL 1
               DISPLAY frecl_id LINE 3 COL 14
               DISPLAY 'Id commande :' LINE 4 COL 1
               DISPLAY frecl_idco LINE 4 COL 15
               DISPLAY 'Motif :' LINE 5 COL 1
               DISPLAY frecl_motif LINE 5 COL 8
               DISPLAY 'Etat :' LINE 6 COL 1
               DISPLAY frecl_etat LINE 6 COL 18
               DISPLAY 'Date :' LINE 7 COL 1
               DISPLAY frecl_date LINE 7 COL 18
            END-READ
            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Afficher une autre recl ? 1 ou 0 :' LINE 8 COL 1
             ACCEPT Wrep LINE 8 COL 34
            END-PERFORM
          END-PERFORM
          END-IF
          CLOSE reclamations.

*> Procédures relatives au fichier clients

        AJOUT_CLIENT.
        OPEN I-O clients
        IF fcl_stat = 35 THEN
                OPEN OUTPUT clients
                CLOSE clients
                OPEN I-O compteurs
                IF fcpt_stat = 35 THEN
                    OPEN OUTPUT compteurs
                    CLOSE compteurs
                    OPEN I-O compteurs
                END-IF
                MOVE 2 TO fcpt_id
                MOVE 1 TO fcpt_val
                WRITE fcptTampon END-WRITE
                CLOSE compteurs
                OPEN I-O clients
        END-IF

        PERFORM WITH TEST AFTER UNTIL Wrep = 0
        OPEN I-O compteurs
        MOVE 2 TO fcpt_id
        READ compteurs
        MOVE fcpt_val TO fcl_id
        COMPUTE fcpt_val = fcpt_val + 1
        REWRITE fcptTampon
        CLOSE compteurs
        DISPLAY '------- AJOUT CLIENT -------' WITH BLANK SCREEN
          DISPLAY 'Nom Client :' LINE 3 COL 1
          DISPLAY 'Prenom Client :' LINE 4 COL 1
          DISPLAY 'Telephone Client :' LINE 5 COL 1
          DISPLAY 'Mail client :' LINE 6 COL 1
          ACCEPT fcl_nom LINE 3 COL 14
          ACCEPT fcl_prenom LINE 4 COL 17
          ACCEPT fcl_tel LINE 5 COL 20
          ACCEPT fcl_mail LINE 6 COL 15
          WRITE fclTampon END-WRITE
          PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Ajouter un autre client ? 1 ou 0 :' LINE 8 COL 1
             ACCEPT Wrep LINE 9 COL 1
          END-PERFORM
        END-PERFORM
        CLOSE clients.

        AFFICHER_CLIENT.
        OPEN I-O clients
        IF fcl_stat = 35 THEN

            DISPLAY 'Fichier clients inexistant' WITH BLANK SCREEN
            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
               DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
               ACCEPT Wrep LINE 2 COL 22
            END-PERFORM
        ELSE

        PERFORM WITH TEST AFTER UNTIL Wrep = 0
        DISPLAY '------- Recherche Client -------' WITH BLANK SCREEN
        DISPLAY "Identifiant du client :" LINE 2 COL 1
        ACCEPT fcl_id LINE 2 COL 25
        READ clients
        INVALID KEY
            DISPLAY 'Client Inexistant' LINE 3 COL 1
        NOT INVALID KEY
           DISPLAY 'ID client :' LINE 3 COL 1
           DISPLAY fcl_id LINE 3 COL 13
           DISPLAY 'Nom :' LINE 4 COL 1
           DISPLAY fcl_nom LINE 4 COL 7
           DISPLAY 'Prenom :' LINE 5 COL 1
           DISPLAY fcl_prenom LINE 5 COL 10
           DISPLAY 'Tel :' LINE 6 COL 1
           DISPLAY fcl_tel LINE 6 COL 7
           DISPLAY 'Mail :' LINE 7 COL 1
           DISPLAY fcl_mail LINE 7 COL 8
        END-READ
         PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
           DISPLAY 'Afficher autre client ? 1 ou 0 :' LINE 9 COL 1
           ACCEPT Wrep LINE 9 COL 34
         END-PERFORM
        END-PERFORM

        END-IF
        CLOSE clients.

*> Procédures relatives au fichier commandes

        AJOUT_COMMANDE.
          OPEN I-O commandes
          IF fco_stat = 35 THEN
                  OPEN OUTPUT commandes
                  CLOSE commandes
                  OPEN I-O compteurs
                  IF fcpt_stat = 35 THEN
                      OPEN OUTPUT compteurs
                      CLOSE compteurs
                      OPEN I-O compteurs
                  END-IF
                  MOVE 3 TO fcpt_id
                  MOVE 1 TO fcpt_val
                  WRITE fcptTampon END-WRITE
                  CLOSE compteurs
                  OPEN I-O commandes
          END-IF

          PERFORM WITH TEST AFTER UNTIL Wrep = 0
          DISPLAY '------- AJOUT COMMANDE -------' WITH BLANK SCREEN

          DISPLAY 'Identifiant Client :' LINE 3 COL 1
          DISPLAY 'Identifiant Article :' LINE 4 COL 1
          DISPLAY 'Date de commande (JJ/MM/AAAA): ' LINE 5 COL 1
          DISPLAY 'Code Promo :' LINE 6 COL 1

          OPEN I-O compteurs
          MOVE 3 TO fcpt_id
          READ compteurs
          MOVE fcpt_val TO fco_id
          COMPUTE fcpt_val = fcpt_val + 1
          REWRITE fcptTampon
          CLOSE compteurs

          OPEN I-O clients
          OPEN I-O articles

          ACCEPT fco_idcl LINE 3 COL 22
          MOVE fco_idcl TO fcl_id
          READ clients

          INVALID KEY

            DISPLAY 'Client inexistant' WITH BLANK SCREEN
            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
               DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
               ACCEPT Wrep LINE 2 COL 22
            END-PERFORM

          NOT INVALID KEY

          ACCEPT fco_idart LINE 4 COL 23
          MOVE fco_idart TO fart_id
          READ articles

              INVALID KEY
                DISPLAY 'Article inexistant' WITH BLANK SCREEN
                PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                   DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
                   ACCEPT Wrep LINE 2 COL 22
                END-PERFORM

              NOT INVALID KEY
                ACCEPT fco_date LINE 5 COL 32
                ACCEPT fco_promo LINE 6 COL 14
                WRITE fcoTampon END-WRITE
                PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                   DISPLAY 'Nouvelle commande ? 1 ou 0 :' LINE 8 COL 1
                   ACCEPT Wrep LINE 8 COL 30
                END-PERFORM
          END-READ
          END-READ
          END-PERFORM
          CLOSE clients
          CLOSE articles
          CLOSE commandes.


          AFFICHER_COMMANDE.
          OPEN I-O commandes
          IF fco_stat = 35 THEN
            DISPLAY 'Fichier commandes inexistant' WITH BLANK SCREEN
            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
               DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
               ACCEPT Wrep LINE 2 COL 22
            END-PERFORM
          ELSE
          PERFORM WITH TEST AFTER UNTIL Wrep = 0
          DISPLAY '------- Recherche Commande -------' WITH BLANK SCREEN
          DISPLAY "Identifiant de la commande :" LINE 2 COL 1
          ACCEPT fco_id LINE 2 COL 30
          READ commandes
          INVALID KEY
              DISPLAY 'Commande Inexistante' LINE 3 COL 1
          NOT INVALID KEY
             DISPLAY 'ID commande :' LINE 3 COL 1
             DISPLAY fco_id LINE 3 COL 15
             DISPLAY 'Client :' LINE 4 COL 1
             DISPLAY fco_idcl LINE 4 COL 10
             DISPLAY 'Article :' LINE 5 COL 1
             DISPLAY fco_idart LINE 5 COL 11
             DISPLAY 'Date :' LINE 6 COL 1
             DISPLAY fco_date LINE 6 COL 8
             DISPLAY 'Code Promo utilisé :' LINE 7 COL 1
             DISPLAY fco_promo LINE 7 COL 22
         END-READ
           PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Afficher autre commande ? 1 ou 0 :' LINE 9 COL 1
             ACCEPT Wrep LINE 9 COL 36
           END-PERFORM
         END-PERFORM
         END-IF
         CLOSE commandes.

*> Procédures relatives au fichier articles

        AJOUT_ARTICLE.
          OPEN I-O articles
          IF fart_stat = 35 THEN
                  OPEN OUTPUT articles
                  CLOSE articles
                  OPEN I-O compteurs
                  IF fcpt_stat = 35 THEN
                      OPEN OUTPUT compteurs
                      CLOSE compteurs
                      OPEN I-O compteurs
                  END-IF
                  MOVE 4 TO fcpt_id
                  MOVE 1 TO fcpt_val
                  WRITE fcptTampon END-WRITE
                  CLOSE compteurs
                  OPEN I-O articles
          END-IF
          PERFORM WITH TEST AFTER UNTIL Wrep = 0
          DISPLAY '------- AJOUT ARTICLE -------' WITH BLANK SCREEN

          DISPLAY 'Nom article :' LINE 3 COL 1
          DISPLAY 'Type article :' LINE 4 COL 1
          DISPLAY 'Description article :' LINE 5 COL 1
          DISPLAY 'Prix article :' LINE 6 COL 1
          DISPLAY 'Duree garantie :' LINE 7 COL 1
          OPEN I-O compteurs
          MOVE 4 TO fcpt_id
          READ compteurs
          MOVE fcpt_val TO fart_id
          COMPUTE fcpt_val = fcpt_val + 1
          REWRITE fcptTampon
          CLOSE compteurs
          ACCEPT fart_nom LINE 3 COL 15
          ACCEPT fart_type LINE 4 COL 16
          ACCEPT fart_description LINE 5 COL 23
          ACCEPT fart_prix LINE 6 COL 16
          ACCEPT fart_dureegaranti LINE 7 COL 18
          WRITE fartTampon END-WRITE
          PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Ajouter un autre article ? 1 ou 0 :' LINE 8 COL 1
             ACCEPT Wrep LINE 8 COL 36
          END-PERFORM
          END-PERFORM
          CLOSE articles.

        AFFICHAGE_ARTICLE.
        OPEN I-O articles
        IF fart_stat = 35 THEN
            DISPLAY 'Fichier commandes inexistant' WITH BLANK SCREEN
            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
              DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
              ACCEPT Wrep LINE 2 COL 22
            END-PERFORM
        ELSE
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
          DISPLAY '------- Recherche Article -------' WITH BLANK SCREEN
          DISPLAY "Identifiant de l'article :" LINE 2 COL 1
          ACCEPT fart_id LINE 2 COL 28
          READ articles
          INVALID KEY
              DISPLAY 'Article Inexistant' LINE 3 COL 1
          NOT INVALID KEY
             DISPLAY 'Id article :' LINE 3 COL 1
             DISPLAY fart_id LINE 3 COL 14
             DISPLAY 'Nom article :' LINE 4 COL 1
             DISPLAY fart_nom LINE 4 COL 15
             DISPLAY 'Prix :' LINE 5 COL 1
             DISPLAY fart_prix LINE 5 COL 8
             DISPLAY 'Durée garantie :' LINE 6 COL 1
             DISPLAY fart_dureegaranti LINE 6 COL 18
         END-READ
        PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
           DISPLAY 'Afficher autre article ? 1 ou 0 :' LINE 7 COL 1
           ACCEPT Wrep LINE 7 COL 35
        END-PERFORM
        END-PERFORM
        END-IF
        CLOSE articles.
