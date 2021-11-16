      ******************************************************************
      * Author: VINCENT SENCLAIR
      * Date: 4-18-18
      * Purpose: MERGE SORTED ELEMENTS AND GROUPS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MERGER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.
      *    CONTROL VARIABLES
       01 LCOUNT PIC 99 VALUE 1.
       01 RCOUNT PIC 99 VALUE 1.

      *    ARRAY USED FOR SORTING
       01 MASTER.
           05 MDATA PIC 99 OCCURS 0 TO 100 TIMES
           DEPENDING ON FULLSIZE
           INDEXED BY INDX-1.

       LINKAGE SECTION.
      *    UNSORTED ARRAY TAKEN IN WHEN MERGER IS CALLED
       01 SORTEDMASTER.
           05 SORTEDDATA PIC 99 OCCURS 100 TIMES INDEXED BY INDX-S.

      *    SIZE VARIABLES
       01 LSIZE PIC 99.
       01 RSIZE PIC 99.
       01 FULLSIZE PIC 99.

       01 LEFTSIDE.
           05 LDATA PIC 99 OCCURS 0 TO 100 TIMES
           DEPENDING ON LSIZE
           INDEXED BY INDX-2.

       01 RIGHTSIDE.
           05 RDATA PIC 99 OCCURS 0 TO 100 TIMES
           DEPENDING ON RSIZE
           INDEXED BY INDX-3.

       PROCEDURE DIVISION USING SORTEDMASTER, LEFTSIDE,
       RIGHTSIDE, FULLSIZE, LSIZE, RSIZE.
       MAIN-PROCEDURE.
          DISPLAY "Working in merger"

      *    LOOP TO SORT GIVEN ARRAY
            PERFORM VARYING INDX-1 FROM 1 BY 1 UNTIL INDX-1 > FULLSIZE
            SET INDX-2 TO LCOUNT
            SET INDX-3 TO RCOUNT
            IF RCOUNT > RSIZE
                OR (LCOUNT <= LSIZE AND LDATA(INDX-2) < RDATA(INDX-3))
                THEN
                SET MDATA(INDX-1) TO LDATA(LCOUNT)
                COMPUTE LCOUNT = LCOUNT + 1
            ELSE
                SET MDATA(INDX-1) TO RDATA(RCOUNT)
                COMPUTE RCOUNT = RCOUNT + 1
            END-IF
            END-PERFORM

            MOVE MASTER TO SORTEDMASTER.

          DISPLAY "Exiting merger"  .
       END PROGRAM MERGER.
