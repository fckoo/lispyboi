(assert-false (equal 'a 'b))
(assert-true (equal 'a 'a))
(assert-true (equal 3 3))

(assert-true (equal 1 1))
(assert-false (equal 1 2))
(assert-false (equal 2 1))

(assert-true (equal #\space #\space))
(assert-false (equal #\a #\b))
(assert-false (equal #\b #\a))

(assert-true (equal #\A #\A))
(assert-false (equal #\A #\a))

(assert-true (equal '(1 2 3 4 5) '(1 2 3 4 5)))

;;                     v            v
(assert-false (equal '(9 2 3 4 5) '(1 2 3 4 5)))

(assert-true (equal (cons 1 2) (cons 1 2)))

;;                         v          v
(assert-false (equal (cons 1 2) (cons 2 2)))

(assert-true (equal (cons 1 (cons 2 (cons 3 nil)))
                    (list 1 2 3)))

(assert-true (equal '(1 (2 3 (4 5 6 (7 . 42) 8) 9) 10)
                    '(1 (2 3 (4 5 6 (7 . 42) 8) 9) 10)))

(assert-false (equal '(1 (2 3 (4 5 6 (7 . 42) 8) 9) 10)
                     ;;               v
                     '(1 (2 3 (4 5 6 (8 . 42) 8) 9) 10)))

;;                                        vv
(assert-false (equal '(1 (2 3 (4 5 6 (7 . 41) 8) 9) 10)
                     '(1 (2 3 (4 5 6 (7 . 42) 8) 9) 10)))


(assert-true (equal "hello world" "hello world"))


(let ((function-kanji (make-string #\京 #\ム #\ヱ #\ロ #\発 #\木 #\し #\び #\士 #\懐 #\追 #\ト #\ざ #\極 #\角 #\じ #\人 #\公 #\め #\む #\ざ #\表 #\一 #\ぞ #\ぶ #\ゅ #\発 #\日 #\に #\こ #\適 #\1 #\院 #\ヘ #\ト #\ワ #\ル #\交 #\近 #\エ #\フ #\車 #\出 #\ニ #\ヤ #\シ #\室 #\改 #\す #\発 #\廃 #\ず #\イ #\ー #\み #\導 #\道 #\ト #\マ #\型 #\島 #\3 #\0 #\目 #\そ #\く #\ぱ #\紙 #\計 #\ク #\ト #\困 #\稿 #\卓 #\捨 #\縦 #\で #\か #\。 #\芸 #\あ #\ル #\ご #\読 #\備 #\ん #\ト #\ぎ #\答 #\界 #\ヲ #\タ #\ミ #\化 #\9 #\5 #\際 #\メ #\ケ #\今 #\亡 #\ヒ #\メ #\ケ #\ソ #\憶 #\政 #\写 #\ユ #\ワ #\ニ #\略 #\変 #\ヤ #\ロ #\メ #\新 #\開 #\活 #\逆 #\ヤ #\触 #\注 #\ト #\ヘ #\コ #\択 #\富 #\る #\す #\民 #\府 #\リ #\ヨ #\ム #\愛 #\3 #\5 #\偶 #\凝 #\峡 #\9 #\6 #\朝 #\ロ #\真 #\定 #\青 #\微 #\酸 #\亨 #\拙 #\ト #\の #\。))

      (reader-kanji "京ムヱロ発木しび士懐追トざ極角じ人公めむざ表一ぞぶゅ発日にこ適1院ヘトワル交近エフ車出ニヤシ室改す発廃ずイーみ導道トマ型島30目そくぱ紙計クト困稿卓捨縦でか。芸あルご読備んトぎ答界ヲタミ化95際メケ今亡ヒメケソ憶政写ユワニ略変ヤロメ新開活逆ヤ触注トヘコ択富るす民府リヨム愛35偶凝峡96朝ロ真定青微酸亨拙トの。"))


  (assert-true (equal reader-kanji reader-kanji)))


(assert-false (equal (make-array 10) (make-array 10)))

(assert-true (equal (type-of (make-array 10))
                    (type-of (make-array 10))))
