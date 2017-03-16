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
          02 frecl_motif PIC A(40).
          02 frecl_description PIC A(40).
          02 frecl_etat PIC A(10).
          02 frecl_note PIC X(2).
          02 frecl_date_data.
            05 frecl_date.
                10 frecl_annee PIC 9(4).
                10 frecl_mois PIC 9(2).
                10 frecl_jour PIC 9(2).

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
          02 fco_date_data.
            05 fco_date.
                10 fco_annee PIC 9(4).
                10 fco_mois PIC 9(2).
                10 fco_jour PIC 9(2).
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
          02 fstata_nbrecl PIC 9(3).
          02 fstata_nbCommande PIC 9(3).
          02 fstata_pourcent PIC 9(2).

WORKING-STORAGE SECTION.
        77 frecl_stat PIC 9(2).
        77 fcl_stat PIC 9(2).
        77 fco_stat PIC 9(2).
        77 fart_stat PIC 9(2).
        77 fstata_stat PIC 9(2).
        77 fcpt_stat PIC 9(2).
        77 frecl_idtemp PIC X(6).
        77 Wrep PIC 9.
        77 Wrep2 PIC 9.
        77 Wfin PIC 9.
        77 tmp PIC 9(3).
        77 Wmenu PIC 9(1).
        77 Wnom PIC A(20).
        77 Wind PIC 9(6).
        77 W1 PIC 9(2).
        77 tmp_id_article PIC X(6).
        77 etatRec PIC 9(1).
        01 SYS-DATE-DATA.
            05 SYS-DATE .
                10 SYS-YEAR PIC 9(4).
                10 SYS-MONTH PIC 9(2).
                10 SYS-DAY PIC 9(2).



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
                PERFORM AFFICHER_STATS_ARTICLES
            END-IF
        END-PERFORM
        STOP RUN.



        AFFICHER_STATS_ARTICLES.
            OPEN I-O statistiquesArticles
            PERFORM WITH TEST AFTER UNTIL Wrep = 0
            IF fstata_stat = 35 THEN


              PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                DISPLAY "Pas d'articles, pas de stats" WITH BLANK SCREEN
                DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
                ACCEPT Wrep LINE 2 COL 21
              END-PERFORM

            ELSE
                DISPLAY "---- Stats Article ----" WITH BLANK SCREEN
                DISPLAY "Renseigner l'id artcile :" LINE 2 COL 1
                ACCEPT fstata_idart LINE 2 COL 27
                READ statistiquesArticles
                INVALID KEY
                  PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                    DISPLAY "ID article incorrect" WITH BLANK SCREEN
                    DISPLAY "Réessayer ? 1 ou 0 :" LINE 2 COL 1
                    ACCEPT Wrep LINE 2 COL 21
                  END-PERFORM
                NOT INVALID KEY
                  MULTIPLY fstata_nbrecl BY 100 GIVING tmp END-MULTIPLY
    DIVIDE tmp BY fstata_nbCommande GIVING fstata_pourcent END-DIVIDE
                  REWRITE fstataTampon
                  DISPLAY "Pourcentage de réclamation :" LINE 3 COL 1
                  DISPLAY fstata_pourcent LINE 3 COL 30
                  DISPLAY "%" LINE 3 COL 32
                  PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                    DISPLAY 'Autre article ? 1 ou 0 :' LINE 5 COL 1
                    ACCEPT Wrep LINE 5 COL 26
                  END-PERFORM
                END-READ
            END-IF
            END-PERFORM
            CLOSE statistiquesArticles.
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

            OPEN I-O compteurs
            OPEN I-O commandes
            OPEN I-O statistiquesArticles
            MOVE 1 TO fcpt_id

            PERFORM WITH TEST AFTER UNTIL Wrep = 0
            READ compteurs
            MOVE FUNCTION CURRENT-DATE TO frecl_date

            DISPLAY '------ RECLAMATION ------' WITH BLANK SCREEN
            DISPLAY 'Numéro de Réclamation :' LINE 2 COL 1
            DISPLAY 'Date de la réclamation :' LINE 3 COL 1
            DISPLAY 'Numero de commande :' LINE 4 COL 1
            DISPLAY 'Motif de votre reclamation :' LINE 5 COL 1
            DISPLAY 'Detaillez votre probleme :' LINE 6 COL 1
            DISPLAY fcpt_val LINE 2 COL 25
            DISPLAY frecl_jour LINE 3 COL 26
            DISPLAY '/' LINE 3 COL 28
            DISPLAY frecl_mois LINE 3 COL 29
            DISPLAY '/' LINE 3 COL 31
            DISPLAY frecl_annee LINE 3 COL 32

            ACCEPT frecl_idco LINE 4 COL 22
            MOVE frecl_idco TO fco_id
            READ commandes
            INVALID KEY

                DISPLAY 'Commande inexistante' WITH BLANK SCREEN
                PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                   DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
                   ACCEPT Wrep LINE 2 COL 21
                END-PERFORM

            NOT INVALID KEY

            ACCEPT frecl_motif LINE 5 COL 30
            ACCEPT frecl_description LINE 6 COL 28
            MOVE 'ouvert' TO frecl_etat
            MOVE fcpt_val TO frecl_id
            COMPUTE fcpt_val = fcpt_val + 1
            IF fstata_stat = 35 THEN
                DISPLAY 'Problème statistiquesArticles' LINE 7 COL 1
                PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                  DISPLAY 'Réessayer ? 1 ou 0 :' LINE 9 COL 1
                  ACCEPT Wrep LINE 9 COL 33
                END-PERFORM
            ELSE
                MOVE fco_idart TO fstata_idart
                READ statistiquesArticles
                REWRITE fcptTampon
                COMPUTE fstata_nbrecl = fstata_nbrecl + 1
                REWRITE fstataTampon
                WRITE freclTampon END-WRITE
                DISPLAY 'Réclamation réalisée avec succès.' LINE 7 COL 1
                PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                  DISPLAY 'Nouvelle reclamation ? 1 ou 0 :' LINE 9 COL 1
                  ACCEPT Wrep LINE 9 COL 33
                END-PERFORM
            END-IF
            END-READ
          END-PERFORM
          CLOSE statistiquesArticles
          CLOSE compteurs
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
               DISPLAY frecl_id LINE 3 COL 16
               DISPLAY 'Id commande :' LINE 4 COL 1
               DISPLAY frecl_idco LINE 4 COL 15
               DISPLAY 'Motif :' LINE 5 COL 1
               DISPLAY frecl_motif LINE 5 COL 9
               DISPLAY 'Etat :' LINE 6 COL 1
               DISPLAY frecl_etat LINE 6 COL 8
               DISPLAY 'Date :' LINE 7 COL 1
               DISPLAY frecl_jour LINE 7 COL 8
               DISPLAY '/' LINE 7 COL 10
               DISPLAY frecl_mois LINE 7 COL 11
               DISPLAY '/' LINE 7 COL 13
               DISPLAY frecl_annee LINE 7 COL 14
            END-READ
            PERFORM WITH TEST AFTER UNTIL Wrep2 = 0 OR Wrep2 = 1
                DISPLAY 'Modifier cette réclamation :' LINE 9 COL 1
                DISPLAY '1 --> OUI' LINE 10 COL 1
                DISPLAY '0 --> NON' LINE 11 COL 1
                ACCEPT Wrep2 LINE 12 COL 1
            END-PERFORM
            IF Wrep2 = 1 THEN
          DISPLAY "---- Modification Réclamation ----" WITH BLANK SCREEN
              DISPLAY 'ID Réclamation modifiée :' LINE 3 COL 1
              DISPLAY frecl_id LINE 3 COL 26
              DISPLAY 'Nouvel Etat de la réclamation :' LINE 4 COL 1
              DISPLAY '1 --> ouvert' LINE 5 COL 1
              DISPLAY '2 --> en traitement' LINE 6 COL 1
              DISPLAY '3 --> cloture' LINE 7 COL 1
              DISPLAY 'Autre --> ne rien faire' LINE 8 COL 1
              DISPLAY 'Choix :' LINE 9 COL 1
              ACCEPT etatRec LINE 9 COL 9
              IF etatRec = 1 THEN
                MOVE 'ouvert' TO frecl_etat
                REWRITE freclTampon
              END-IF
              IF etatRec = 2 THEN
                MOVE 'traitement' TO frecl_etat
                REWRITE freclTampon
              END-IF
              IF etatRec = 3 THEN
                MOVE 'cloture' TO frecl_etat
                REWRITE freclTampon
              END-IF
            END-IF

            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Traiter une autre recl ? 1 ou 0 :' LINE 14 COL 1
             ACCEPT Wrep LINE 14 COL 35
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
        OPEN I-O compteurs
        MOVE 2 TO fcpt_id

        PERFORM WITH TEST AFTER UNTIL Wrep = 0
        READ compteurs
        DISPLAY '------- AJOUT CLIENT -------' WITH BLANK SCREEN
          DISPLAY 'Identifiant Client :' LINE 2 COL 1
          DISPLAY 'Nom Client :' LINE 3 COL 1
          DISPLAY 'Prenom Client :' LINE 4 COL 1
          DISPLAY 'Telephone Client :' LINE 5 COL 1
          DISPLAY 'Mail client :' LINE 6 COL 1
          DISPLAY fcpt_val LINE 2 COL 22
          ACCEPT fcl_nom LINE 3 COL 14
          ACCEPT fcl_prenom LINE 4 COL 17
          ACCEPT fcl_tel LINE 5 COL 20
          ACCEPT fcl_mail LINE 6 COL 15
          MOVE fcpt_val TO fcl_id
          COMPUTE fcpt_val = fcpt_val + 1
          REWRITE fcptTampon

          WRITE fclTampon END-WRITE
          PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Ajouter un autre client ? 1 ou 0 :' LINE 8 COL 1
             ACCEPT Wrep LINE 9 COL 1
          END-PERFORM
        END-PERFORM
        CLOSE compteurs
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
          OPEN I-O compteurs
          OPEN I-O clients
          OPEN I-O statistiquesArticles
          OPEN I-O articles
          MOVE 3 TO fcpt_id
          PERFORM WITH TEST AFTER UNTIL Wrep = 0

          READ compteurs
          MOVE FUNCTION CURRENT-DATE TO fco_date

          DISPLAY '------- AJOUT COMMANDE -------' WITH BLANK SCREEN
          DISPLAY 'Identifiant de la commande :' LINE 2 COL 1
          DISPLAY 'Date de commande :' LINE 3 COL 1
          DISPLAY 'Identifiant Client :' LINE 4 COL 1
          DISPLAY 'Identifiant Article :' LINE 5 COL 1
          DISPLAY 'Code Promo :' LINE 6 COL 1
          DISPLAY fcpt_val LINE 2 COL 30


          DISPLAY fco_jour LINE 3 COL 20
          DISPLAY '/' LINE 3 COL 22
          DISPLAY fco_mois LINE 3 COL 23
          DISPLAY '/' LINE 3 COL 25
          DISPLAY fco_annee LINE 3 COL 26
          ACCEPT fco_idcl LINE 4 COL 22
          MOVE fco_idcl TO fcl_id
          READ clients
          INVALID KEY

            DISPLAY 'Client inexistant' WITH BLANK SCREEN
            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
               DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
               ACCEPT Wrep LINE 2 COL 22
            END-PERFORM

          NOT INVALID KEY

          ACCEPT fco_idart LINE 5 COL 23
          MOVE fco_idart TO fart_id
          READ articles

              INVALID KEY
                DISPLAY 'Article inexistant' WITH BLANK SCREEN
                PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                   DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
                   ACCEPT Wrep LINE 2 COL 22
                END-PERFORM

              NOT INVALID KEY
                MOVE fart_id TO fstata_idart
                READ statistiquesArticles
                ACCEPT fco_promo LINE 6 COL 14
                MOVE fcpt_val TO fco_id
                COMPUTE fstata_nbCommande = fstata_nbCommande + 1
                REWRITE fstataTampon
                COMPUTE fcpt_val = fcpt_val + 1
                REWRITE fcptTampon
                WRITE fcoTampon END-WRITE
                PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                   DISPLAY 'Nouvelle commande ? 1 ou 0 :' LINE 8 COL 1
                   ACCEPT Wrep LINE 8 COL 30
                END-PERFORM

                END-READ
          END-READ
          END-PERFORM
          CLOSE compteurs
          CLOSE statistiquesArticles
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
             DISPLAY fco_jour LINE 6 COL 8
             DISPLAY '/' LINE 6 COL 10
             DISPLAY fco_mois LINE 6 COL 11
             DISPLAY '/' LINE 6 COL 13
             DISPLAY fco_annee LINE 6 COL 14
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

          OPEN I-O statistiquesArticles
          IF fstata_stat = 35 THEN
            OPEN OUTPUT statistiquesArticles
            CLOSE statistiquesArticles
            OPEN I-O statistiquesArticles
          END-IF


          OPEN I-O compteurs
          MOVE 4 TO fcpt_id
          PERFORM WITH TEST AFTER UNTIL Wrep = 0

          READ compteurs

          DISPLAY '------- AJOUT ARTICLE -------' WITH BLANK SCREEN
          DISPLAY 'Identifiant article :' LINE 2 COL 1
          DISPLAY 'Nom article :' LINE 3 COL 1
          DISPLAY 'Type article :' LINE 4 COL 1
          DISPLAY 'Description article :' LINE 5 COL 1
          DISPLAY 'Prix article :' LINE 6 COL 1
          DISPLAY 'Duree garantie :' LINE 7 COL 1

          DISPLAY fcpt_val LINE 2 COL 23



          ACCEPT fart_nom LINE 3 COL 15
          ACCEPT fart_type LINE 4 COL 16
          ACCEPT fart_description LINE 5 COL 23
          ACCEPT fart_prix LINE 6 COL 16
          ACCEPT fart_dureegaranti LINE 7 COL 18
          MOVE fcpt_val TO fart_id
          MOVE fcpt_val TO fstata_idart
          MOVE 0 TO fstata_pourcent
          MOVE 0 TO fstata_nbCommande
          MOVE 0 TO fstata_nbrecl
          COMPUTE fcpt_val = fcpt_val + 1

          REWRITE fcptTampon
          WRITE fstataTampon END-WRITE
          WRITE fartTampon END-WRITE
          PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Ajouter un autre article ? 1 ou 0 :' LINE 8 COL 1
             ACCEPT Wrep LINE 8 COL 37
          END-PERFORM
          END-PERFORM
          CLOSE compteurs
          CLOSE statistiquesArticles
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
