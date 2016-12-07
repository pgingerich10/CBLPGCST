       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 CBLPGCST.
       AUTHOR.                     PHILIP GINGERICH.
       DATE-WRITTEN.               12/05/2015.
       DATE-COMPILED.              
       
      *****************************************************************
      * 
      *    THIS PROGRAM WILL CREATE A REPORT THAT CONTAINS THE COST TO
      *    CREATE EACH MACHINE.    
      * 
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
           SELECT MACHINE-MASTER
               ASSIGN TO "MASTER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
               
           SELECT INVENTORY-DATA
               ASSIGN TO "INVENTORY.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
               
           SELECT PRTOUT
               ASSIGN TO "MACHINECOST.PRT"
               ORGANIZATION IS RECORD SEQUENTIAL.
               
       DATA DIVISION.
       FILE SECTION.
       
       FD  MACHINE-MASTER
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 13 CHARACTERS
           DATA RECORD IS MACHINE-RECORD.
           
       01  MACHINE-RECORD.
           05  I-MACHINE           PIC X(5).
           05  I-MACHINE-PART      PIC X(5).
           05  I-PART-QTY          PIC 999.
           
       FD  INVENTORY-DATA
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 16 CHARACTERS
           DATA RECORD IS INVENTORY-RECORD.
           
       01  INVENTORY-RECORD.
           05  I-INV-PART          PIC X(5).
           05  I-INV-QTY           PIC 999.
           05  I-INV-COST          PIC 9(6)V99.
       
       FD  PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 80 CHARACTERS
           LINAGE IS 60 WITH FOOTING AT 55
           DATA RECORD IS PRTLINE.
           
       01  PRTLINE                 PIC X(80).
       
       WORKING-STORAGE SECTION.
       
       01  WORK-AREA.
           05  C-PARTS-SUBTOT      PIC 9(8)V99     VALUE ZEROS.
           05  C-MACHINE-TOT       PIC 9(10)V99    VALUE ZEROS.
           05  H-MACHINE           PIC X(5)        VALUE SPACES.
           05  MORE-RECS           PIC X           VALUE 'Y'.
           05  C-PCTR              PIC 99          VALUE ZEROS.
           05  WK-HOLD             PIC X           VALUE SPACE.
           
       01  CURRENT-DATE.
           05  I-DATE.
               10  I-YEAR    PIC 9(4).
               10  I-MONTH   PIC 99.
               10  I-DAY     PIC 99.
           
       01  TITLE-LINE.
           05  FILLER              PIC X(6)    VALUE "DATE: ".
           05  O-MONTH             PIC 99.
           05  FILLER              PIC X       VALUE "/".
           05  O-DAY               PIC 99.
           05  FILLER              PIC X       VALUE "/".
           05  O-YEAR              PIC 9(4).
           05  FILLER              PIC X(11)   VALUE SPACES.
           05  FILLER              PIC X(45)   VALUE 
                                           "GINGERICH'S MACHINE REPORT".
           05  FILLER              PIC X(6)    VALUE 'PAGE: '.
           05  O-PCTR              PIC Z9.
           
       01  COL-HDG-LINE.
           05  FILLER              PIC X(15)   VALUE "MACHINE".
           05  FILLER              PIC X(15)   VALUE "PART".
           05  FILLER              PIC XXX     VALUE "QTY".
           05  FILLER              PIC X(7)    VALUE  SPACES.
           05  FILLER              PIC X(15)   VALUE "COST (PER PART)".
           05  FILLER              PIC X(7)    VALUE SPACES.
           05  FILLER              PIC X(18)   VALUE 
                                               "PART SUBTOTAL COST".    
       
       01  DETAIL-LINE.
           05  O-MACHINE           PIC X(5).
           05  FILLER              PIC X(10)    VALUE SPACES.
           05  O-PART              PIC X(5).
           05  FILLER              PIC X(10)    VALUE SPACES.
           05  O-QTY               PIC ZZ9.
           05  FILLER              PIC X(11)    VALUE SPACES.
           05  O-PART-COST         PIC $$$$,$$9.99.
           05  FILLER              PIC X(11)    VALUE SPACES.
           05  O-PARTS-SUBTOT      PIC $$$,$$$,$$9.99.
           
           
           
       01  TOTAL-LINE.
           05  FILLER              PIC X(47)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "MACHINE TOTAL: ".
           05  O-MACHINE-TOT       PIC $$$,$$$,$$$,$$9.99.
           
       01 BLANK-LINE               PIC X       VALUE SPACE.
           
       PROCEDURE DIVISION.
       
       0000-CBLPGCST.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = "N".
           PERFORM 3000-CLOSING.
           STOP RUN.
           
       1000-INIT.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE.
           MOVE I-MONTH TO O-MONTH.
           MOVE I-DAY TO O-DAY.
           MOVE I-YEAR TO O-YEAR.
           OPEN INPUT MACHINE-MASTER.
           OPEN OUTPUT PRTOUT.
           PERFORM 9100-HDGS.
           PERFORM 9200-READ-MACHINE.
           MOVE I-MACHINE TO H-MACHINE.
           
       2000-MAINLINE.
           OPEN INPUT INVENTORY-DATA.
           PERFORM 9300-READ-INVENTORY.
           PERFORM 2100-DETAIL WITH TEST BEFORE
               UNTIL I-MACHINE NOT EQUAL H-MACHINE.
           WRITE PRTLINE FROM TOTAL-LINE
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM BLANK-LINE
               AFTER ADVANCING 2 LINES.
           MOVE ZEROS TO C-MACHINE-TOT.
           CLOSE INVENTORY-DATA.
           MOVE I-MACHINE TO H-MACHINE.
               
       2100-DETAIL.
           IF I-MACHINE-PART = I-INV-PART
               PERFORM 2110-CALCS
               PERFORM 2120-MOVES
               PERFORM 9200-READ-MACHINE
           ELSE
               READ INVENTORY-DATA
           END-IF.
                   
       2110-CALCS.
           COMPUTE C-PARTS-SUBTOT = I-PART-QTY * I-INV-COST.
           ADD C-PARTS-SUBTOT TO C-MACHINE-TOT.
           
       2120-MOVES.
           MOVE H-MACHINE TO O-MACHINE.
           MOVE I-MACHINE-PART TO O-PART.
           MOVE I-PART-QTY TO O-QTY.
           MOVE I-INV-COST TO O-PART-COST.
           MOVE C-PARTS-SUBTOT TO O-PARTS-SUBTOT.
           MOVE C-MACHINE-TOT TO O-MACHINE-TOT.
           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 1 LINE
                   AT EOP
                       PERFORM 9100-HDGS.
           
       3000-CLOSING.
           CLOSE MACHINE-MASTER.
           CLOSE PRTOUT.
           
       9100-HDGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.
           WRITE PRTLINE FROM TITLE-LINE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM COL-HDG-LINE
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM BLANK-LINE
               AFTER ADVANCING 2 LINES.
           
       9200-READ-MACHINE.
           READ MACHINE-MASTER
               AT END
                   MOVE 'N' TO MORE-RECS
                   MOVE SPACES TO I-MACHINE.
                   
       9300-READ-INVENTORY.
           READ INVENTORY-DATA
               AT END
                   DISPLAY "INVENTORY DATA CONTAINS NO MORE RECORDS"
                   ACCEPT WK-HOLD.