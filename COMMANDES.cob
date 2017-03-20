*> PROCEDURE RELATIVES AUX COMMANDES

*> PROCEDURE PERMETTANT L'ajout d'une commande
        AJOUT_COMMANDE.
          OPEN I-O commandes
          *> On ouvre le fichier en I-O, s'il n'existe pas on le créer
          *> On ouvre le fichier compteurs pour associer le compteur au fichier
          *> Si le fichier "compteurs" n'existe pas on le créer
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
          *> On ouvre les fichiers 4 fichiers
          OPEN I-O compteurs
          OPEN I-O clients
          OPEN I-O statistiquesArticles
          OPEN I-O articles
          MOVE 3 TO fcpt_id
          PERFORM WITH TEST AFTER UNTIL Wrep = 0

          READ compteurs
          *> On récupère la date du jour pour l'afficher et l'associer à l'enregistrement
          MOVE FUNCTION CURRENT-DATE TO fco_date

          DISPLAY '------- AJOUT COMMANDE -------' WITH BLANK SCREEN
          DISPLAY 'Identifiant de la commande :' LINE 2 COL 1
          DISPLAY 'Date de commande :' LINE 3 COL 1
          DISPLAY 'Identifiant Client (XXX):' LINE 4 COL 1
          DISPLAY 'Identifiant Article (XXX):' LINE 5 COL 1
          DISPLAY 'Code Promo :' LINE 6 COL 1
          DISPLAY fcpt_val LINE 2 COL 30


          DISPLAY fco_jour LINE 3 COL 20
          DISPLAY '/' LINE 3 COL 22
          DISPLAY fco_mois LINE 3 COL 23
          DISPLAY '/' LINE 3 COL 25
          DISPLAY fco_annee LINE 3 COL 26
          ACCEPT fco_idcl LINE 4 COL 27
          MOVE fco_idcl TO fcl_id
          *> On vérifie que le client existe bien
          READ clients
          INVALID KEY

            DISPLAY 'Client inexistant' WITH BLANK SCREEN
            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
               DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
               ACCEPT Wrep LINE 2 COL 22
            END-PERFORM

          NOT INVALID KEY

          ACCEPT fco_idart LINE 5 COL 28
          *> Si le client existe, on vérifie que l'article existe aussi
          MOVE fco_idart TO fart_id
          READ articles

              INVALID KEY
                DISPLAY 'Article inexistant' WITH BLANK SCREEN
                PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                   DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
                   ACCEPT Wrep LINE 2 COL 22
                END-PERFORM

              NOT INVALID KEY
                *> Si le client et l'article existent, on incrémente le compteur de commande pour l'article en question
                *> dans le fichier "statistiquesArticles". On réalise aussi l'incrément sur le compteur.
                MOVE fart_id TO fstata_idart
                READ statistiquesArticles
                ACCEPT fco_promo LINE 6 COL 14
                MOVE fcpt_val TO fco_id
                COMPUTE fstata_nbCommande = fstata_nbCommande + 1
                REWRITE fstataTampon
                COMPUTE fcpt_val = fcpt_val + 1
                REWRITE fcptTampon
                *> On ajoute la commande au fichier
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

*> PROCEDURE PERMETTANT L'AFFICHAGE D'UNE COMMANDE, procédure similaire à AFFICHER_CLIENT

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
          DISPLAY "Identifiant de la commande (XXX):" LINE 2 COL 1
          ACCEPT fco_id LINE 2 COL 35
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
