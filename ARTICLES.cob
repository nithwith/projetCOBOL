
*> PROCEDURE PERMETTANT L'AJOUT D'UN ARTICLE
        AJOUT_ARTICLE.
        *> On cherche à ouvrir le fichier "articles" en INPUT-OUTPUT
        *> S'il n'existe pas on le créer,on ouvre le fichier "compteurs"
        *> afin d'associer le compteur des id de "articles" à l'enregistrement portant l'id 4 dans "compteurs"
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
          *> On ouvre en I-O le fichier "statistiquesArticles"
          *> S'il n'existe pas on le créer
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
          *> On récupère l'id de l'article grâce au fichier "compteurs"
          MOVE fcpt_val TO fart_id
          *> Associe la valeur de l'id au fichier stat
          MOVE fcpt_val TO fstata_idart
          *> On associe à chaque valeur 0 car il n'y pas encore eu de commande ou de réclamation sur l'article
          MOVE 0 TO fstata_pourcent
          MOVE 0 TO fstata_nbCommande
          MOVE 0 TO fstata_nbrecl

          *> ON incrémente une fois que tous les champs de saisi ont été passés
          COMPUTE fcpt_val = fcpt_val + 1
          *> On réécrit le tampon dans le fichier "compteurs"
          REWRITE fcptTampon
          *> On écrit les tampons respectifs dans "statistiquesArticles" et "articles"
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


*> PROCEDURE PERMETTANT L'AFFICHAGE D'UN AFFICHAGE_ARTICLE
*> Le processus est similaire à celui du fichier "clients"
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
