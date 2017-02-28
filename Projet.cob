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
       ALTERNATE RECORD KEY frecl_idco WITH DUPLICATES
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
          02 fco_idart PIC X(6).
          02 fco_codep PIC X(4).
      
        FD articles.
        01 fartTampon.  
          02 fart_id PIC X(6).
          02 fart_nom PIC A(20).
          02 fart_type PIC A(10). 
          02 fart_description PIC A(40).
          02 fart_prix PIC X(4).
          02 fart_dureegaranti PIC A(20).        

WORKING-STORAGE SECTION.
        77 frecl_stat PIC 9(2). 
        77 fcl_stat PIC 9(2).
        77 fco_stat PIC 9(2). 
        77 fart_stat PIC 9(2).
        77 Wrep PIC 9.
        77 Wfin PIC 9.
        77 Wnom PIC A(20).
        77 Wtrouve PIC 9.
        77 Wind PIC 9(10).
        77 W1 PIC 9(2).

PROCEDURE DIVISION.
        PERFORM CREATION_FICHIERS
        PERFORM EFFECTUER_RECLAMATION
        STOP RUN.
         
*> procédure pour créer nos fichiers 
        CREATION_FICHIERS.
        OPEN OUTPUT reclamations
        CLOSE reclamations
        OPEN OUTPUT clients
        CLOSE clients
        OPEN OUTPUT commandes
        CLOSE commandes
        OPEN OUTPUT articles
        CLOSE articles.

*> numero de réclamation commence a 1. 
        NOMBRE_RECLAMATIONS.
        MOVE 1 TO W1
        MOVE 0 TO Wind
        MOVE 0 TO Wfin
        OPEN INPUT reclamations
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ reclamations
           AT END MOVE 1 TO Wfin
*>              DISPLAY 'Aucune réclamation'
           NOT AT END
                COMPUTE Wind=Wind+W1 
           END-READ
        END-PERFORM
        COMPUTE Wind=Wind+W1
        MOVE Wind TO frecl_id 
        CLOSE reclamations.  
        
        EFFECTUER_RECLAMATION.
          PERFORM NOMBRE_RECLAMATIONS
          OPEN I-O reclamations
          DISPLAY "------ RECLAMATION ------"
          DISPLAY "Veuillez remplir ce formulaire:"
          DISPLAY 'Quel est votre numéro de commande?'
          ACCEPT frecl_idco
          DISPLAY 'Quel est le motif de votre réclamation?'
          ACCEPT frecl_motif
          DISPLAY 'Détaillez votre problème:'
          ACCEPT frecl_description 
          MOVE 'ouvert' TO frecl_etat
          DISPLAY 'Informations enregistrées avec succès'
          DISPLAY 'Nous traitons votre requête'
          WRITE freclTampon END-WRITE
          CLOSE reclamations.


