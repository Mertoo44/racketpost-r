#lang racket
(require racket/gui/base)
(require racket/date)
(require db)
(require racket/draw)

;; ---------------------------------------------------------
;; 1. VERİTABANI AYARLARI
;; ---------------------------------------------------------
(define db-path "posture_data.sqlite")
(define db-conn
  (sqlite3-connect #:database db-path #:mode 'create))

(query-exec db-conn
            "CREATE TABLE IF NOT EXISTS logs (
               id INTEGER PRIMARY KEY AUTOINCREMENT,
               timestamp TEXT,
               type TEXT,
               response TEXT
             )")

(define (get-current-timestamp)
  (parameterize ([date-display-format 'iso-8601])
    (date->string (current-date) #t)))

(define (log-event type response)
  (query-exec db-conn
              "INSERT INTO logs (timestamp, type, response) VALUES (?, ?, ?)"
              (get-current-timestamp) type response))

(define (get-today-stats)
  (define today (substring (get-current-timestamp) 0 10))
  (query-rows db-conn
              "SELECT type, response FROM logs WHERE timestamp LIKE ?"
              (string-append today "%")))

;; ---------------------------------------------------------
;; 2. MANTIK VE ANALİZ
;; ---------------------------------------------------------
(define (generate-health-advice success-rate total-count)
  (cond
    [(= total-count 0) "Veri yok. Kullanmaya devam et."]
    [(>= success-rate 0.8) "Harika! Sağlığını koruyorsun."]
    [(>= success-rate 0.5) "Fena değil ama daha dikkatli ol."]
    [else "Dikkat! Bugün çok hareketsiz kaldın."]))

(define (show-daily-report)
  (define logs (get-today-stats))
  (define total (length logs))
  (define completed (count (lambda (row) (equal? (vector-ref row 1) "evet")) logs))
  (define success-rate (if (> total 0) (/ completed total) 0.0))
  (define percentage (real->decimal-string (* success-rate 100) 1))
  (define advice (generate-health-advice success-rate total))
  
  (message-box "Günlük Rapor"
               (format "Toplam: ~a\nYapılan: ~a\nBaşarı: %~a\n\nÖneri:\n~a"
                       total completed percentage advice)
               #f '(ok)))

;; ---------------------------------------------------------
;; 3. GÖRSEL VE ARAYÜZ
;; ---------------------------------------------------------

;; Ana Pencere
(define main-frame (new frame% [label "Postür Asistanı"] [width 400] [height 300]))
(define main-panel (new vertical-panel% [parent main-frame] [alignment '(center center)] [spacing 20]))
(define status-msg (new message% [parent main-panel] [label "Sistem Beklemede..."] [font (make-object font% 12 'default)]))

;; --- RESİMLİ VE BÜYÜK YAZILI HATIRLATMA ---
(define (trigger-reminder-with-image)
  (define random-img-num (+ 1 (random 3))) 
  (define img-path (format "img/~a.png" random-img-num))
  (define user-response "hayır") 
  
  (define dlg (new dialog% 
                   [label "Mola Zamanı!"]
                   [parent main-frame]
                   [width 400]     ;; Pencereyi biraz genişlettik
                   [height 450]))  ;; Pencereyi biraz uzattık
  
  ;; --- BURASI GÜNCELLENDİ: YAZI BÜYÜTÜLDÜ ---
  (new message% [parent dlg] 
       [label "Uzun süredir çalışıyorsun.\nLütfen şu hareketi yap:"]
       [font (make-object font% 16 'default 'normal 'bold)]) ;; 16 punto ve Kalın
  
  ;; Resmi yükle
  (if (file-exists? img-path)
      (new message% [parent dlg] 
           [label (make-object bitmap% img-path)]) 
      (new message% [parent dlg] 
           [label (format "(Resim bulunamadı: ~a)" img-path)]))
  
  ;; Butonlar Paneli
  (define btn-panel (new horizontal-panel% 
                         [parent dlg]
                         [alignment '(center center)]
                         [spacing 40])) ;; Butonların arasını açtık
  
  (new button% [parent btn-panel] [label "Yaptım"]
       [min-width 80]
       [callback (lambda (b e) (set! user-response "evet") (send dlg show #f))])
  
  (new button% [parent btn-panel] [label "Atla"]
       [min-width 80]
       [callback (lambda (b e) (set! user-response "hayır") (send dlg show #f))])
  
  (send dlg show #t)
  
  (if (string=? user-response "evet")
      (begin
        (log-event "mola" "evet")
        (send status-msg set-label "Harika! Esnedin."))
      (begin
        (log-event "mola" "hayır")
        (send status-msg set-label "Mola atlandı. Dikkat et!"))))

;; Zamanlayıcı (Timer)
(define reminder-timer (new timer% 
                            [notify-callback (lambda () (trigger-reminder-with-image))] 
                            [interval #f]))

;; Kontrol Butonları
(define start-btn 
  (new button% [parent main-panel] [label "Takibi Başlat (Demo: 5sn)"] [min-width 200]
       [callback (lambda (b e)
                   (send reminder-timer start 5000) 
                   (send status-msg set-label "Aktif! Bekleniyor...")
                   (send start-btn enable #f)
                   (send stop-btn enable #t))]))

(define stop-btn 
  (new button% [parent main-panel] [label "Durdur"] [min-width 200] [enabled #f]
       [callback (lambda (b e)
                   (send reminder-timer stop)
                   (send status-msg set-label "Durduruldu.")
                   (send start-btn enable #t)
                   (send stop-btn enable #f))]))

(new button% [parent main-panel] [label "Raporu Gör"] [min-width 200]
     [callback (lambda (b e) (show-daily-report))])

(send main-frame show #t)