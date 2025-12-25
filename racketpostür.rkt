#lang racket/gui

(require db racket/draw racket/runtime-path)

;; ============================================================
;; 1. DOSYA YOLLARI
;; ============================================================
(define-runtime-path img-klasoru "img")
(define-runtime-path db-yolu "saglik_takip.db")

;; ============================================================
;; 2. VERİTABANI İŞLEMLERİ
;; ============================================================
(define db-conn (sqlite3-connect #:database db-yolu #:mode 'create))

(query-exec db-conn
  "CREATE TABLE IF NOT EXISTS kayitlar (
     id INTEGER PRIMARY KEY AUTOINCREMENT,
     tarih DATE DEFAULT CURRENT_DATE,
     tip TEXT, 
     durum TEXT
  )")

(define (kayit-ekle tip durum)
  (query-exec db-conn "INSERT INTO kayitlar (tip, durum) VALUES (?, ?)" tip durum))

;; ============================================================
;; 3. EGZERSİZ VERİLERİ
;; ============================================================
(define egzersiz-listesi 
  '(("Boynunuzu sağa sola yavaşça esnetin." . "boyun.png")
    ("Bileklerinizi gerdirip bekleyin." . "bilek.png")
    ("Omuzlarınızı dairesel hareketle oynatın." . "omuz.png")
    ("Kollarınızı ileri uzatıp sırtınızı, sonra geri çekip göğsünüzü esnetin." . "sirt_gogus.png")))

(define (resim-yukle dosya-adi)
  (define tam-yol (build-path img-klasoru dosya-adi))
  (if (file-exists? tam-yol) (make-object bitmap% tam-yol) #f))

;; ============================================================
;; 4. HATIRLATICI PENCERESİ
;; ============================================================
(define (hatirlatici-penceresi tip mesaj-baslik)
  (define secilen (list-ref egzersiz-listesi (random (length egzersiz-listesi))))
  (define metin (car secilen))
  (define dosya (cdr secilen))
  (define bmp (resim-yukle dosya))
  
  (define dialog (new dialog% [label mesaj-baslik] [width 450] [height 480]))
  
  (new canvas% [parent dialog] [min-width 350] [min-height 350]
       [paint-callback (lambda (canvas dc)
                         (if bmp 
                             (send dc draw-bitmap bmp 0 0)
                             (send dc draw-text (format "Hata: ~a bulunamadı!" dosya) 20 20)))])
  
  (new message% [parent dialog] [label metin] [font (make-object font% 11 'default 'normal 'bold)])
  
  (define btn-panel (new horizontal-panel% [parent dialog] [alignment '(center center)]))
  (new button% [parent btn-panel] [label "Yaptım ✅"] 
       [callback (lambda (b e) (kayit-ekle tip "yapildi") (send dialog show #f))])
  (new button% [parent btn-panel] [label "Atla ❌"] 
       [callback (lambda (b e) (kayit-ekle tip "atlandi") (send dialog show #f))])
  
  (send dialog show #t))

;; ============================================================
;; 5. RAPORLAMA VE AKILLI ÖNERİLER (Düzeltildi)
;; ============================================================
(define (rapor-ekrani-goster)
  (define rows (query-rows db-conn "SELECT tip, durum FROM kayitlar WHERE tarih = CURRENT_DATE"))
  
  (define m-yap (length (filter (lambda (r) (and (equal? (vector-ref r 0) "mola") (equal? (vector-ref r 1) "yapildi"))) rows)))
  (define m-at  (length (filter (lambda (r) (and (equal? (vector-ref r 0) "mola") (equal? (vector-ref r 1) "atlandi"))) rows)))
  (define d-yap (length (filter (lambda (r) (and (equal? (vector-ref r 0) "durus") (equal? (vector-ref r 1) "yapildi"))) rows)))
  (define d-at  (length (filter (lambda (r) (and (equal? (vector-ref r 0) "durus") (equal? (vector-ref r 1) "atlandi"))) rows)))

  (define oneri
    (cond
      [(and (= (+ m-yap m-at d-yap d-at) 0)) "Henüz veri toplanmadı."]
      [(> m-at m-yap) "⚠️ Çok mola atlıyorsunuz! Gözlerinizi dinlendirin."]
      [(> d-at d-yap) "⚠️ Duruşunuzu düzeltmeyi ihmal ediyorsunuz!"]
      [else "✨ Harika! Sağlıklı bir çalışma disiplini."]))

  (define rapor-dialog (new dialog% [label "Sağlık Raporu"] [width 450]))
  (define panel (new group-box-panel% [parent rapor-dialog] [label "İstatistikler"]))
  
  ;; "At" yerine "Atlandı" olarak güncellendi
  (new message% [parent panel] [label (format "Mola: ~a Tamamlandı / ~a Atlandı" m-yap m-at)])
  (new message% [parent panel] [label (format "Duruş: ~a Tamamlandı / ~a Atlandı" d-yap d-at)])
  
  (define op (new group-box-panel% [parent rapor-dialog] [label "Öneri"]))
  (new message% [parent op] [label oneri] [auto-resize #t])
  
  (new button% [parent rapor-dialog] [label "Tamam"] [callback (lambda (b e) (send rapor-dialog show #f))])
  (send rapor-dialog show #t))

;; ============================================================
;; 6. ANA KONTROL PANELİ
;; ============================================================
(define main-frame (new frame% [label "Racket Sağlık Asistanı v7.1"] [width 450] [height 500]))

(define status-panel (new horizontal-panel% [parent main-frame] [alignment '(center center)] [stretchable-height #f]))
(define status-label (new message% [parent status-panel] 
                          [label "DURUM: Beklemede 🛑"] 
                          [auto-resize #t] 
                          [font (make-object font% 12 'default 'normal 'bold)]))

(define m-group (new group-box-panel% [parent main-frame] [label "☕ Mola Ayarları"]))
(define m-panel (new horizontal-panel% [parent m-group]))
(define mola-dk (new text-field% [parent m-panel] [label "Dk: "] [init-value "45"]))
(define mola-sn (new text-field% [parent m-panel] [label "Sn: "] [init-value "0"]))

(define d-group (new group-box-panel% [parent main-frame] [label "🧍 Duruş Ayarları"]))
(define d-panel (new horizontal-panel% [parent d-group]))
(define durus-dk (new text-field% [parent d-panel] [label "Dk: "] [init-value "20"]))
(define durus-sn (new text-field% [parent d-panel] [label "Sn: "] [init-value "0"]))

(define mola-timer (new timer% [notify-callback (lambda () (hatirlatici-penceresi "mola" "Mola Zamanı!"))]))
(define durus-timer (new timer% [notify-callback (lambda () (hatirlatici-penceresi "durus" "Duruş Kontrolü!"))]))

(new button% [parent main-frame] [label "▶️ Takibi Başlat"] [callback (lambda (b e) 
  (define m-toplam (* (+ (* (or (string->number (send mola-dk get-value)) 0) 60) (or (string->number (send mola-sn get-value)) 0)) 1000))
  (define d-toplam (* (+ (* (or (string->number (send durus-dk get-value)) 0) 60) (or (string->number (send durus-sn get-value)) 0)) 1000))
  (send mola-timer stop) (send durus-timer stop)
  (when (> m-toplam 0) (send mola-timer start m-toplam))
  (when (> d-toplam 0) (send durus-timer start d-toplam))
  (send status-label set-label "DURUM: Hatırlatıcılar Aktif 🚀"))])

(new button% [parent main-frame] [label "⏸️ Takibi Durdur"] [callback (lambda (b e) (send mola-timer stop) (send durus-timer stop) (send status-label set-label "DURUM: Durduruldu 🛑"))])
(new button% [parent main-frame] [label "📊 Sağlık Raporunu Gör"] [callback (lambda (b e) (rapor-ekrani-goster))])
(new button% [parent main-frame] [label "❌ Uygulamadan Çık"] [callback (lambda (b e) (exit))])

(send main-frame show #t)