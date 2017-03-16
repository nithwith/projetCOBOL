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

*> Fichier servant à sauvegarder l'identifiant de chaque fichier
*> pour l'incrémentation automatique
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

PROCEDURE DIVISION.
    *> Corps principal de notre programme avec affichage du menu
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

*> Nom des fichiers dans lesquelles se trouvent les procédures relatives
*> à chaque fichier

    COPY ARTICLES.
    COPY CLIENTS.
    COPY COMMANDES.
    COPY RECLAMATIONS.
    COPY STATS_ARTICLES.
