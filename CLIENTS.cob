
*> PROCEDURE PERMETANT L'AJOUT D'UN CLIENT

        AJOUT_CLIENT.
        *> On cherche à ouvrir le fichier "clients" en INPUT-OUTPUT
        *> S'il n'existe pas on le créer,on ouvre le fichier "compteurs"
        *> afin d'associer le compteur des id de "clients" à l'enregistrement portant l'id 2 dans "compteurs"
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
          *> On récupère toutes les valeurs et on associe la valeur de l'id client
          MOVE fcpt_val TO fcl_id
          *> On incrémente ensuite de 1 la valeur de fcpt_val pour le prochain ajout
          COMPUTE fcpt_val = fcpt_val + 1
          *> On réécrit fcptTampon pour que le changement soit pris en compte
          REWRITE fcptTampon
          *> On écrit ensuite fclTampon pour ajouter le client
          WRITE fclTampon END-WRITE
          PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Ajouter un autre client ? 1 ou 0 :' LINE 8 COL 1
             ACCEPT Wrep LINE 9 COL 1
          END-PERFORM
        END-PERFORM
        CLOSE compteurs
        CLOSE clients.


*> PROCEDURE PERMETTANT L'AFFICHAGE D'UN CLIENT

        AFFICHER_CLIENT.
        OPEN I-O clients

        *> Si le fichier n'existe pas on affiche l'erreur
        IF fcl_stat = 35 THEN

            DISPLAY 'Fichier clients inexistant' WITH BLANK SCREEN
            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
               DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
               ACCEPT Wrep LINE 2 COL 22
            END-PERFORM
        ELSE
        *> Sinon on affiche un "menu" demandant l'id du client
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
        DISPLAY '------- Recherche Client -------' WITH BLANK SCREEN
        DISPLAY "Identifiant du client (XXX):" LINE 2 COL 1
        ACCEPT fcl_id LINE 2 COL 30
        READ clients
        *> On lit le fichier à partir de l'id saisi par le user
        INVALID KEY
            *> Si la clé est invalide, le client n'existe pas
            DISPLAY 'Client Inexistant' LINE 3 COL 1
        NOT INVALID KEY
           *> Si la clé est valide, on affiche le contenu du tampon
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
        *> On termine la lecture et on propose une nouvelle recherche
         PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
           DISPLAY 'Afficher autre client ? 1 ou 0 :' LINE 9 COL 1
           ACCEPT Wrep LINE 9 COL 34
         END-PERFORM
        END-PERFORM

        END-IF
        CLOSE clients.
