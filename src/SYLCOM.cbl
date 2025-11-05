*****************************************************************
      * シラバス管理システム - 共通ルーチンモジュール
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYLCOM.
       AUTHOR. SHINYAY.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CURRENT-DATE.
          05 WS-CURRENT-YEAR       PIC 9(4).
          05 WS-CURRENT-MONTH      PIC 9(2).
          05 WS-CURRENT-DAY        PIC 9(2).

       LINKAGE SECTION.
       01 LS-FUNCTION-CODE         PIC X.
          88 LS-VALIDATE-COURSE-ID       VALUE 'C'.
          88 LS-VALIDATE-DATE            VALUE 'D'.
          88 LS-FORMAT-ERROR             VALUE 'E'.
          88 LS-GET-CURRENT-DATE         VALUE 'T'.

       01 LS-PARAMETER-1           PIC X(50).
       01 LS-PARAMETER-2           PIC X(50).
       01 LS-RESULT                PIC X(200).
       01 LS-RETURN-CODE           PIC 9  VALUE 0.
          88 LS-SUCCESS                  VALUE 0.
          88 LS-ERROR                    VALUE 1.

       PROCEDURE DIVISION USING LS-FUNCTION-CODE, LS-PARAMETER-1,
                                LS-PARAMETER-2, LS-RESULT, LS-RETURN-CODE.
       MAIN-PROCESS.
           EVALUATE TRUE
               WHEN LS-VALIDATE-COURSE-ID
                   PERFORM VALIDATE-COURSE-ID
               WHEN LS-VALIDATE-DATE
                   PERFORM VALIDATE-DATE
               WHEN LS-FORMAT-ERROR
                   PERFORM FORMAT-ERROR-MESSAGE
               WHEN LS-GET-CURRENT-DATE
                   PERFORM GET-CURRENT-DATE
               WHEN OTHER
                   MOVE "未定義の機能コードです。" TO LS-RESULT
                   MOVE 1 TO LS-RETURN-CODE
           END-EVALUATE.

           GOBACK.

       VALIDATE-COURSE-ID.
      * 科目コードの妥当性検証（例：先頭2文字はアルファベット、残り4文字は数字）
           IF LS-PARAMETER-1(1:2) IS ALPHABETIC AND
              LS-PARAMETER-1(3:4) IS NUMERIC
               MOVE "科目コード有効" TO LS-RESULT
               MOVE 0 TO LS-RETURN-CODE
           ELSE
               STRING "科目コードは先頭2文字がアルファベット、"
                      "残り4文字が数字である必要があります。"
                      DELIMITED BY SIZE
                      INTO LS-RESULT
               END-STRING
               MOVE 1 TO LS-RETURN-CODE
           END-IF.

       VALIDATE-DATE.
      * 日付形式の検証（YYYYMMDD形式）
           IF LS-PARAMETER-1(1:4) IS NUMERIC AND
              LS-PARAMETER-1(5:2) IS NUMERIC AND
              LS-PARAMETER-1(7:2) IS NUMERIC
               IF LS-PARAMETER-1(5:2) >= 01 AND
                  LS-PARAMETER-1(5:2) <= 12 AND
                  LS-PARAMETER-1(7:2) >= 01 AND
                  LS-PARAMETER-1(7:2) <= 31
                   MOVE "日付有効" TO LS-RESULT
                   MOVE 0 TO LS-RETURN-CODE
               ELSE
                   MOVE "日付の値が範囲外です。" TO LS-RESULT
                   MOVE 1 TO LS-RETURN-CODE
               END-IF
           ELSE
               STRING "日付形式が不正です（YYYYMMDD形式である"
                      "必要があります）。"
                      DELIMITED BY SIZE
                      INTO LS-RESULT
               END-STRING
               MOVE 1 TO LS-RETURN-CODE
           END-IF.

       FORMAT-ERROR-MESSAGE.
      * エラーメッセージのフォーマット
           STRING "エラー: " DELIMITED BY SIZE
                  LS-PARAMETER-1 DELIMITED BY SIZE
                  " - " DELIMITED BY SIZE
                  LS-PARAMETER-2 DELIMITED BY SIZE
                  INTO LS-RESULT
           END-STRING.
           MOVE 0 TO LS-RETURN-CODE.

       GET-CURRENT-DATE.
      * 現在の日付を取得
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE TO LS-RESULT.
           MOVE 0 TO LS-RETURN-CODE.
