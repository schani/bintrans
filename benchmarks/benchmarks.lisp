(load "let-match")

(defun dcs (x)
  (substitute #\d #\. (substitute #\p #\+ (substitute #\_ #\- (string-downcase (symbol-name x))))))

(defparameter *configs* nil)
(defparameter *benchmarks* nil)

(defun make-script (config benchmark)
  (with-open-file (out (format nil "bench.~A.~A" (dcs benchmark) (dcs config)) :direction :output :if-exists :supersede)
    (let ((*standard-output* out)
	  (commands (cdr (assoc config *configs*)))
	  (cmdline (cadr (assoc benchmark *benchmarks*)))
	  (files (cddr (assoc benchmark *benchmarks*)))
	  (run-commands '((exec)))
	  (current-run nil))
      (labels ((exec (command)
		 (case-match command
		   ((make ?makefile)
		    (format t "make -f benchmarks/~A clean~%make -f benchmarks/~:*~A all~%" makefile))
		   ((exec . ?args)
		    (let ((out-file (or (car args) (format nil "out.~~A.~A.~~A" (dcs config)))))
		      (format t "cd benchrun~%time ~A >&../benchmarks/~A~%cd ..~%" cmdline (format nil out-file (dcs benchmark) current-run))))
		   ((run . ?new-run-commands)
		    (setq run-commands new-run-commands))
		   ((runs ?n)
		    (dotimes (i n)
		      (setq current-run i)
		      (dolist (run-command run-commands)
			(exec run-command))))
		   ((rm ?file)
		    (format t "rm -f benchrun/~A~%" file))
		   ((static-liveness)
		    (format t "./alive benchrun/cfg.txt /tmp/liveness.txt~%./convert_liveness </tmp/liveness.txt >benchrun/liveness.out~%"))
		   (t
		    (error "unknown command ~A" command)))))
	(assert (and (not (null commands)) (not (null cmdline))))
	(format t "#!/bin/bash~%mkdir -p benchrun~%cd benchrun~%rm -rf *~%cd ..~%")
	(dolist (file files)
	  (format t "cp ~A ./~%" file))
	(dolist (command commands)
	  (exec command))))))

(defun make-all-scripts ()
  (with-open-file (out "bench.all" :direction :output :if-exists :supersede)
    (format out "#!/bin/bash~%")
    (dolist (config *configs*)
      (dolist (benchmark *benchmarks*)
	(make-script (car config) (car benchmark))
	(format out "sh benchmarks/bench.~A.~A~%" (dcs (car benchmark)) (dcs (car config)))))))

(defmacro defconfig (name &rest commands)
  `(setq *configs* (acons ',name ',commands *configs*)))

(defmacro defbenchmark (name cmdline &optional files)
  `(setq *benchmarks* (acons ',name (cons ,cmdline ',files) *benchmarks*)))

(defconfig noliveness
  (make "Makefile.noliveness")
  (runs 5))

(defconfig liveness0
  (make "Makefile.liveness0.cfg")
  (exec "out.~A.liveness0.static.collect")
  (static-liveness)
  (run (exec "out.~A.liveness0.static.~A"))
  (runs 5)
  (make "Makefile.liveness0")
  (run
   (rm "liveness.out")
   (exec "out.~A.liveness0.~A.0")
   (exec "out.~A.liveness0.~A.1")
   (exec "out.~A.liveness0.~A.2")
   (exec "out.~A.liveness0.~A.3")
   (exec "out.~A.liveness0.~A.4"))
  (runs 5))

(defconfig liveness1
  (make "Makefile.liveness1.cfg")
  (exec "out.~A.liveness1.static.collect")
  (static-liveness)
  (run (exec "out.~A.liveness1.static.~A"))
  (runs 5)
  (make "Makefile.liveness1")
  (run
   (rm "liveness.out")
   (exec "out.~A.liveness1.~A.0")
   (exec "out.~A.liveness1.~A.1")
   (exec "out.~A.liveness1.~A.2")
   (exec "out.~A.liveness1.~A.3")
   (exec "out.~A.liveness1.~A.4"))
  (runs 5))

(defbenchmark compress "../bintrans /bin/compress95 <../compress.inputs/bigtest.in")
(defbenchmark m88ksim "../bintrans /bin/m88ksim <ctl.raw"
  ("../m88ksim.inputs/ctl.raw" "../m88ksim.inputs/*.big"))
(defbenchmark go_5stone21 "../bintrans /bin/go 50 21 <../go.inputs/ref/5stone21.in")
(defbenchmark go_9stone21 "../bintrans /bin/go 50 21 <../go.inputs/ref/9stone21.in")
(defbenchmark xlisp "../bintrans /bin/xlisp <../xlisp.inputs/full.lsp")
;(defbenchmark imc "../bintrans /bin/imc 10")