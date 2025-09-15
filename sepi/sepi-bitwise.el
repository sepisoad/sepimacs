;; Define macro for bitwise AND (&)
(defmacro & (&rest args)
  "Bitwise AND of ARGS, wrapping `logand'."
  `(logand ,@args))

;; Define macro for bitwise OR (|)
(defmacro | (&rest args)
  "Bitwise OR of ARGS, wrapping `logior'."
  `(logior ,@args))

;; Define macro for bitwise XOR (^)
(defmacro ^ (&rest args)
  "Bitwise XOR of ARGS, wrapping `logxor'."
  `(logxor ,@args))

;; Define macro for bitwise NOT (~)
(defmacro ~ (arg)
  "Bitwise NOT of ARG, wrapping `lognot'."
  `(lognot ,arg))

;; Define macro for left shift (<<)
(defmacro << (value count)
  "Left shift VALUE by COUNT bits, wrapping `lsh'."
  `(lsh ,value ,count))

;; Define macro for right shift (>>)
(defmacro >> (value count)
  "Arithmetic right shift VALUE by COUNT bits, wrapping `ash'."
  `(ash ,value (- ,count)))

;; Define macro for binary conversion (bin)
(defmacro bin (n)
  "Convert integer N to a binary string."
  `(let ((num ,n)
         (binary ""))
     (if (< num 0)
         (error "Negative numbers not supported")
       (while (> num 0)
         (setq binary (concat (number-to-string (logand num 1)) binary))
         (setq num (ash num -1)))
       (if (string-empty-p binary) "0" binary))))

;; Define macro for hexadecimal conversion (hex)
(defmacro hex (n)
  "Convert integer N to a hexadecimal string."
  `(format "%x" ,n))

(provide 'sepi-bitwise)
