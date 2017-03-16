*> PROCEDURE RELATIVE AU FICHIER "statistiquesArticles"
    AFFICHER_STATS_ARTICLES.
        OPEN I-O statistiquesArticles
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
        *> Si le fichier n'existe pas on affiche le message
        IF fstata_stat = 35 THEN


          PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
            DISPLAY "Pas d'articles, pas de stats" WITH BLANK SCREEN
            DISPLAY 'Réessayer ? 1 ou 0 :' LINE 2 COL 1
            ACCEPT Wrep LINE 2 COL 21
          END-PERFORM

        ELSE
        *> Sinon on demande l'id article et on calcul le taux de réclamation par rapport
        *> au nombre de commandes passés pour l'article
        *> On écrit la valeur dans l'enregistrement 
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
