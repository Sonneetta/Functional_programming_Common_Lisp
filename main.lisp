(defstruct manufacturer
  id         
  name       
  country
)

(defstruct drone
  id              
  manufacturer-id 
  model           
  price
)         

(defun make-manufacturers-from-list (fields)
  "Створює структуру manufacturers зі списку полів."
  (make-manufacturer 
   :id (parse-integer (nth 0 fields))
   :name (string-trim '(#\Space) (nth 1 fields))
   :country (string-trim '(#\Space) (nth 2 fields))))

(defun make-drone-from-list (fields)
  "Створює структуру drone зі списку полів."
  (make-drone
   :id (parse-integer (nth 0 fields))
   :manufacturer-id  (parse-integer  (nth 1 fields))
   :model (string-trim '(#\Space) (nth 2 fields))
   :price (parse-integer  (nth 3 fields))))

(defun split-string-custom (string delimiter)
  "Розбиває рядок STRING на частини, використовуючи символ DELIMITER."
  (let ((result '())
        (start 0)
        (length (length string)))
    (dotimes (i (1+ length))
      (let ((char (if (< i length) (char string i) nil)))
        (if (or (eql char delimiter) (null char))
            (progn
              (when (< start i)
                (push (subseq string start i) result))
              (setf start (1+ i))))))
    (nreverse result)))

(defun read-manufacturers-table (filename)
  "Зчитує таблицю з файлу і повертає список структур manufacturer."
  (let ((manufacturers '()))
    (with-open-file (stream filename :direction :input)
      (read-line stream) 
      (loop for line = (read-line stream nil nil)
            while line
            do (let ((fields (split-string-custom line #\,)))
                 (push (make-manufacturers-from-list fields) manufacturers))))
    (nreverse manufacturers)))

(defun read-drones-table (filename)
  "Зчитує таблицю з файлу і повертає список структур drone."
  (let ((drones '()))
    (with-open-file (stream filename :direction :input)
      (read-line stream) 
      (loop for line = (read-line stream nil nil)
            while line
            do (let ((fields (split-string-custom line #\,)))
                 (push (make-drone-from-list fields) drones))))
    (nreverse drones)))

(defun select (filename filter-fn)
  "Зчитує таблицю з файлу, а потім повертає лямбда-вираз для фільтрації записів за допомогою FILTER-FN."
  (lambda ()
    (let ((records (if (search "manufacturers.csv" filename)
                       (read-manufacturers-table filename)
                       (read-drones-table filename))))
      (remove-if-not filter-fn records))))

(defun test-select ()
  "Тестує функцію SELECT з умовами фільтрації."
  (let* ((manufacturers-file "manufacturers.csv")
         (drones-file "drones.csv")
         (filter-manufacturers (select manufacturers-file
                                  (lambda (manufacturer)
                                    (string=  (manufacturer-name manufacturer) "Aerovironment"))))
         (filter-drones (select drones-file
                                   (lambda (drone)
                                     (= (drone-manufacturer-id drone) 1)))))  ;; виправлено тут
    
    (format t "~&--- Filtered Manufacturers (Manufacturer Name = Aerovironment) ---~%")
    (dolist (manufacturer (funcall filter-manufacturers))
      (format t "ID: ~A~%Name: ~A~%Country: ~A~%" 
              (manufacturer-id manufacturer)
              (manufacturer-name manufacturer)
              (manufacturer-country manufacturer))
      (format t "------------------------~%"))

    (format t "~&--- Filtered Drones (Manufacturer Id = 1) ---~%")
    (dolist (drone (funcall filter-drones))
      (format t "ID: ~A~%Model: ~A~%Price: ~A~%" 
              (drone-id drone)
              (drone-model drone)
              (drone-price drone))
      (format t "Manufacturer ID: ~A~%" (drone-manufacturer-id drone))
      (format t "------------------------~%"))))

(defun test-read-from-tabels ()
  "Тестує зчитування таблиць і виводить їх."
  (let ((manufacturers (read-manufacturers-table "manufacturers.csv"))
        (drones (read-drones-table "drones.csv")))
    (format t "~&--- Manufacturers ---~%")
    (dolist (manufacturer manufacturers)
      (format t "ID: ~A~%Name: ~A~%Country: ~A~%" 
              (manufacturer-id manufacturer)
              (manufacturer-name manufacturer)
              (manufacturer-country manufacturer))
      (format t "------------------------~%"))

    (format t "~&--- Drones ---~%")
    (dolist (drone drones)
      (format t "ID: ~A~%Model: ~A~%Price: ~A~%" 
              (drone-id drone)
              (drone-model drone)
              (drone-price drone))
      (format t "Manufacturer ID: ~A~%" (drone-manufacturer-id drone))
      (format t "------------------------~%"))))


(defun manufacturer-to-hashtable (manufacturer)
  "Перетворює структуру manufacturer у геш-таблицю."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash 'id ht) (manufacturer-id manufacturer)
          (gethash 'name ht) (manufacturer-name manufacturer)
          (gethash 'country ht) (manufacturer-country manufacturer))
    ht))

(defun convert-manufacturers-to-hashtables (manufacturers)
  "Перетворює список структур manufacturer у список геш-таблиць."
  (mapcar #'manufacturer-to-hashtable manufacturers))

(defun drone-to-hashtable (drone)
  "Перетворює структуру drone у геш-таблицю."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash 'id ht) (drone-id drone)
          (gethash 'manufacturer-id ht) (drone-manufacturer-id drone)
          (gethash 'model ht) (drone-model drone)
          (gethash 'price ht) (drone-price drone))
    ht))

(defun convert-drones-to-hashtables (drones)
"Перетворює список структур drone у список геш-таблиць."
  (mapcar #'drone-to-hashtable drones))

(defun test-transform-to-hash ()
  "Тестує перетворення даних drone та manufacturer у геш-таблиці."
  (format t "~&---  Manufacturers ---~%")
  (let* ((manufacturers (select "manufacturers.csv" #'identity))
         (hashtables (convert-manufacturers-to-hashtables (funcall manufacturers))))
    (dolist (ht hashtables)
      (maphash (lambda (key value) 
                 (format t "~a: ~a~%" key value)) 
               ht)))

  (format t "~&~%")
  (format t "~&---  Drones ---~%")
  (let* ((drones (select "drones.csv" #'identity))
         (hashtables (convert-drones-to-hashtables (funcall drones))))
    (dolist (ht hashtables)
      (maphash (lambda (key value) 
                 (format t "~a: ~a~%" key value)) 
               ht))))


(defun write-manufacturers-to-csv (file-path records &optional write-headers)
  "Записує список структур manufacturer у файл CSV.
WRITE-HEADERS - якщо T, додає заголовки до CSV."
  (with-open-file (stream file-path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (when write-headers
      (format stream "ID,Name,Country~%"))
    (dolist (record records)
      (format stream "~A,~A,~A~%"
              (manufacturer-id record)
              (manufacturer-name record)
              (manufacturer-country record)))))


(defun write-drones-to-csv (file-path records &optional write-headers)
  "Записує список структур drone у файл CSV.
WRITE-HEADERS - якщо T, додає заголовки до CSV."
  (with-open-file (stream file-path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (when write-headers
      (format stream "ID,Manufacturer-id,Model,Price~%"))
    (dolist (record records)
      (format stream "~A,~A,~A,~A~%"
              (drone-id record)
              (drone-manufacturer-id record)
              (drone-model record)
              (drone-price record)))))

(defun test-write-to-csv () 
  "Тестує функції запису даних у CSV для manufacturer та drone."
  (let* ((manufacturers-file "manufacturers_out.csv")
         (drones-file "drones_out.csv")
         (filter-manufacturers (select "manufacturers.csv"
                                  (lambda (manufacturer)
                                    (= (manufacturer-id manufacturer) 3))))
         (filter-drones (select "drones.csv"
                                   (lambda (drone)
                                     (string= (drone-model drone) "Phantom")))))

    (write-manufacturers-to-csv manufacturers-file (funcall filter-manufacturers) t)
    (write-drones-to-csv drones-file (funcall filter-drones) t)
    
    (format t "~&--- Written manufacturers CSV ---~%")
    (with-open-file (stream manufacturers-file :direction :input)
      (loop for line = (read-line stream nil)
            while line
            do (format t "~A~%" line)))
    
    (format t "~&--- Written drones CSV ---~%")
    (with-open-file (stream drones-file :direction :input)
      (loop for line = (read-line stream nil)
            while line
            do (format t "~A~%" line)))))

(defun print-manufacturers-table (manufacturers)
  "Виводить список структур manufacturer у вигляді таблиці."
  (format t "~%~A~%" (make-string 50 :initial-element #\-))
  (format t "~10T~A~23T~A~40T~A~%" "ID" "Name" "Country")
  (format t "~A~%" (make-string 50 :initial-element #\-))
  (dolist (manufacturer manufacturers)
    (format t "~T~10T~A~23T~A~40T~A~%"
            (manufacturer-id manufacturer)
            (manufacturer-name manufacturer)
            (manufacturer-country manufacturer)))
  (format t "~A~%" (make-string 50 :initial-element #\-)))

(defun print-drones-table (drones)
  "Виводить список структур drone у вигляді таблиці."
  (format t "~%~A~%" (make-string 50 :initial-element #\-))
  (format t "~T~5T~A~9T~A~25T~A~40T~A~%" "ID" "Manufacturer ID" "Model" "Price")
  (format t "~A~%" (make-string 50 :initial-element #\-))
  (dolist (drone drones)
    (format t "~T~5T~A~15T~A~25T~A~40T~A~%"
            (drone-id drone)
            (drone-manufacturer-id drone)
            (drone-model drone)
            (drone-price drone)))
  (format t "~A~%" (make-string 50 :initial-element #\-)))

(defun test-pretty-print ()
  "Тестує красивий вивід записів таблиці."
  (let ((manufacturers (read-manufacturers-table "manufacturers.csv"))
        (drones (read-drones-table "drones.csv")))
    (format t "~%-------------- Manufacturers Table --------------~%")
    (print-manufacturers-table manufacturers)

    (format t "~%------------------ Drones Table ------------------~%")
    (print-drones-table drones)))

(defun run-all-tests ()
  "Виконує всі тести."
  (format t "~%--- Running Test: test-select ---~%")
  (test-select)

  (format t "~%--- Running Test: test-read-from-tabels ---~%")
  (test-read-from-tabels)

  (format t "~%--- Running Test: test-transform-to-hash ---~%")
  (test-transform-to-hash)

  (format t "~%--- Running Test: test-write-to-csv ---~%")
  (test-write-to-csv)

  (format t "~%--- Running Test: test-pretty-print ---~%")
  (test-pretty-print))