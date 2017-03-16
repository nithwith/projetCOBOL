*> PROCEDURES RELATIVES AUX reclamations

*> Procedure permettant d'ajouter une réclamation
        EFFECTUER_RECLAMATION.
            OPEN I-O reclamations
            *> On ouvre en I-O le fichier "reclamations"
            *> S'il n'exsite pas on le créer et on ouvre le fichier "compteurs"
            *> pour associer l'enregistrement d'id 1 et de valeur 1 à "reclamation"
            *> Si le fichier "compteurs" n'existe pas on le créer
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
            *> On récupère le compteur pour définir l'id
            READ compteurs

            *> On récupère la date du jour pour l'associer
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
            *> Si la commande n'existe pas on affiche un message
                DISPLAY 'Commande inexistante' WITH BLANK SCREEN
                PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                   DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
                   ACCEPT Wrep LINE 2 COL 21
                END-PERFORM

            NOT INVALID KEY
            *> Si la commande existe on poursuit le traitement
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
*> On incrémente le nombre de réclamation dans "statistiquesArticles" pour l'article en question
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

*> PROCEDURE servant à afficher et changer le statut d'une réclamation
          GERER_RECLAMATION.
          OPEN I-O reclamations

          IF frecl_stat = 35 THEN

            DISPLAY 'Fichier réclamation inexistant' WITH BLANK SCREEN
            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
               DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
               ACCEPT Wrep LINE 2 COL 22
            END-PERFORM

         ELSE
         *> ON AFFICHE la réclamation
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
            *> On demande si l'utilisateur souhaite modifier la reclamation
            PERFORM WITH TEST AFTER UNTIL Wrep2 = 0 OR Wrep2 = 1
                DISPLAY 'Modifier cette réclamation :' LINE 9 COL 1
                DISPLAY '1 --> OUI' LINE 10 COL 1
                DISPLAY '0 --> NON' LINE 11 COL 1
                ACCEPT Wrep2 LINE 12 COL 1
            END-PERFORM
            IF Wrep2 = 1 THEN
          DISPLAY "---- Modification Réclamation ----" WITH BLANK SCREEN
        *> On lui propose de modifier seulement l'état avec un systeme de menu basé sur des chiffres
              DISPLAY 'ID Réclamation modifiée :' LINE 3 COL 1
              DISPLAY frecl_id LINE 3 COL 26
              DISPLAY 'Nouvel Etat de la réclamation :' LINE 4 COL 1
              DISPLAY '1 --> ouvert' LINE 5 COL 1
              DISPLAY '2 --> en traitement' LINE 6 COL 1
              DISPLAY '3 --> cloture' LINE 7 COL 1
              DISPLAY 'Autre --> Quitter' LINE 8 COL 1
              DISPLAY 'Choix :' LINE 9 COL 1
              ACCEPT etatRec LINE 9 COL 9
              *> on associe l'état en fonction du chiffre choisi
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
